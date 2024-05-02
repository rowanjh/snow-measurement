library(here)
library(dplyr)
library(sf)
library(stringr)
library(data.table)
library(purrr)

# Convenience variables
TZ_AK <- "America/Anchorage"
CRS_BARROW <- paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                    ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
CRS_BASEMAP <- 3857
CRS_WGS84 <- 4326

#' Load drone snowcover data file
#' 
#' The `drone-cover.csv` file gives the proportion of snow cover detected in each
#' drone photograph. The csv file was generated in an earlier step, see 
#' `process-drone-images.R`. Each row represents one photograph, and the 
#' quadrat ID, date, and proportion snow cover are provided.
#' 
#' This function loads and does basic preprocessing on the file, including some
#' date corrections.
#' 
#' @param path path to .csv file with snowcover data. Assumed to have columns:
#'      `id`, `lat`, `lon`, `dt`, `cover`, `thresh`, `file`
#' @return data.frame with columns poly_id, lat, lon, dt, cover, thresh, date, 
#'      img_id
load_snow_data_drone <- function(path = here('outputs', 'drone-cover-csv', 
                                             'drone-snowcover.csv')){
    cover_data <- read.csv(path) |>
        rename(poly_id = id) |>
        mutate(poly_id = paste0("pt_", poly_id))
    
    # Fix wrong dates for the first day of images, perhaps before drone synced time?
    cover_data[grepl("snowmelt-transect-1/", cover_data$file),'dt'] <- "2022-05-29 14:30:00"
    
    # Throw out id 3, which only had 1 image, 98% cover
    cover_data <- cover_data[cover_data$poly_id != "pt_3",]
    
    # Convert dt to datetime
    cover_data <- cover_data |>
        mutate(dt = as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S", tz = TZ_AK),
               date = as.Date(dt, tz = TZ_AK)) |>
        arrange(poly_id, dt)

    cover_data <- cover_data |> 
        st_as_sf(coords = c('lon', 'lat'), crs = CRS_WGS84, remove = FALSE)
    
    cover_data <- cover_data |> mutate("rowid" = row_number()) |> relocate(rowid)

    return(cover_data)
}


#' Add implied 100% and 0% cover values to the drone snow cover dataset
#' 
#' We know from ground observations that before drone surveys began, there was 
#' 100% cover for all sampling quadrats, and when drone surveys stopped, there 
#' was 0% cover (except for quadrat 40). This function adds an observation of 
#' 100% cover for every day before the surveys, until the `backwards_to` date. 
#' Similarly, an observation of 0% is added for every day until the 
#' `forwards_to` date. The time of the observation is arbitrarily set to 12
#' noon.
#' 
#' @param cover_data sf data.frame with cover values for each drone photograph, 
#'      as returned by `load_snow_data_drone`
#' @param backwards_to (date, chr, or POSIXct) A date prior to the first drone 
#'      survey to back fill with 100% cover
#' @param forwards_to (date, chr, or POSIXct) A date after the final drone 
#'      survey to forward-fill with 0% cover
extrapolate_drone_data <- function(cover_data, backwards_to, forwards_to){
    # Back-fill cover = 1 for all points until the backward_to date
    polys_all <- unique(cover_data$poly_id)
    dates_pre <- seq.Date(as.Date(backwards_to), as.Date("2022-05-28"), by = 1)
    covers_pre <- expand.grid(poly_id = polys_all, date = dates_pre, cover = 1)
    
    # 30 points were omitted from final drone surveys due to cover being 0% 
    # already. Add the implied 0 cover for these points on the final drone 
    # survey dates
    polys_final_survey <- cover_data |> filter(date == "2022-06-21") |> pull(poly_id)
    dates_final_surveys <- as.Date(c("2022-06-21", "2022-06-22"))
    covers_final_surveys <- expand.grid(poly_id = polys_all[!polys_all %in% polys_final_survey], 
                                        date = dates_final_surveys, 
                                        cover = 0)
    
    # Forward-fill cover = 0 until the forwards_to date
    # Note that 1 point had significant cover left after the final survey, 
    # so is not forward_filled (pt_38 still had 2.5% cover on the last day but a 
    # strong downwards trajectory in melt, I think it would be 0 very soon.)
    polys_not40 <- polys_all[!polys_all == "pt_40"]
    dates_post <- seq.Date(as.Date("2022-06-23"), as.Date(forwards_to), by = 1)
    covers_post <- expand.grid(poly_id = polys_not40, date = dates_post, cover = 0)
    
    # Copy lat/lon/poly_id info for each polygon
    poly_info <- cover_data |> 
        filter(date == "2022-05-29") |>
        select(poly_id, lat, lon) |> 
        st_drop_geometry()
    
    # Combine synthesised datasets
    generated_data <- covers_pre |> 
        bind_rows(covers_final_surveys) |> 
        bind_rows(covers_post) |> 
        left_join(poly_info, by = "poly_id") |>
        st_as_sf(coords = c("lon", "lat"), crs = CRS_WGS84, remove = FALSE) |>
        mutate(dt = date |> as.character() |> paste("12:00:00"), 
               dt = as.POSIXct(dt, tz = TZ_AK),
               rowid = row_number() + nrow(cover_data),
               augmented = TRUE)
    
    # Combine synthesized data with original cover dataset
    bind_rows(cover_data, generated_data) |>
        arrange(date, poly_id) |>
        mutate(augmented = ifelse(is.na(augmented), FALSE, augmented))
}

#' Load MODIS and VIIRS snowcover data
#'
#' Load the file containing MODIS and VIIRS snow cover data, and and do basic 
#' preprocessing. The `sat-cover-nsidc.csv` file contains the % cover estimates 
#' extracted at quadrats, nest/capture site buffers, or the total plot area.
#' 
#' Rows in the input csv file represent extracted values for one polygon 
#' from one raster pixel in a satellite image. If a polygon overlapped 
#' multiple raster cells, then a row with the %cover for each cell in that 
#' polygon is given, and the degree of overlap between the polygon and raster  
#' cell is given, enabling calculation of a weighted average cover for the 
#' polygon
#' 
#' Uses data.table for faster processing if using large dataset
#' 
#' Notable columns in the input file are below
#'      poly_id:
#'          Identifier for the target polygon.
#'      cell: 
#'          ID of satellite raster cell intersecting the polygon
#'      ndsi_scaled:
#'          NDSI reading for this raster cell
#'      coverage_fraction: 
#'          Proportion of the raster cell that is covered by the target polygon, 
#'          i.e. 0.017 = the polygon only covers 2% of the raster cell
#'      fraction: 
#'          Proportion of the polygon that is covered by this raster cell, 
#'          i.e. 0.5 = 50% of the polygon was in this raster cell, and hence 
#'          50% of the polygon was in other raster cell(s)
#' 
#' @param path (character) path to .csv file with snowcover data, as output by
#'      the script `process-sat-images.R`  
#' @param raw_only (logical) if TRUE, no processing is done and the raw input
#'      file is returned. Useful for some descriptive statistics
#' @return data.frame showing the %cover for each polygon for each satellite 
#'      image. Data for a polygon may be averaged over multiple satellite raster
#'      cells, thus %cover represents a weighted average. Polygons are excluded 
#'      if more than 50% of their area containins invalid or unknown values e.g.
#'      due to cloud cover
#'      
#'      Important columns include:
#'      rowid:
#'          Unique row ID
#'      ID: 
#'          A polygon ID, describing one of the following:
#'              Drone photograph footprint         e.g. "pt_1"
#'              Nest site + 250m circular buffer   e.g. "P1101"
#'              Male cap site 250m circ. buffer    e.g. "M14126042"
#'              Full drone surveyed area           "full_plot"
#'              Nest searched area                 "full_nestsearch_area"
#'      poly_type:
#'          Type of polygon, either "drone_grid", "nest", "male_firstcap", 
#'          "full_plot", "full_neastsearch_area"
#'      dt_collect:
#'          datetime of satellite NDSI data collection
#'      ndsi: 
#'          raw ndsi value from NDSI layer of raster
#'      ndsi_rescaled: 
#'          raw ndsi value x 0.0001 (MODIS) or ndsi x 0.001 ()
#'      cover: 
#'          Main snowcover value used in this study (ndsi_rescaled * 1.45 -0.01)
#'      cover_fudge: 
#'          Cover values with 1 and 0 converted to 0.9999 and 0.0001
#'      cover_default: 
#'          Raw value from NDSI_snow_cover layer, not used
#'      basicqa:
#'          value extracted from satellite raster layer "basicqa", see handbooks
#'          for full information about MODIS/VIIRS NDSI snow dataset layers
#'      basicqa_code: 
#'          text description of basicqa code.
#'      flags:
#'          value extracted from the raster layer "qa_algorithm_flags".
#'      bit1-bit8:
#'          Separated individual bit flags from the layer "qa_algorithm_flags"
#'      file_stem:
#'          filename of data source
#'      cover_comment:
#'          unused
#'      
#'      
#'      
load_snow_data_nsidc <- 
    function(path = here("outputs", "sat-cover-csv", "sat-cover-nsidc.csv"),
             raw_only = FALSE){
    # Load csv
    sat_data <- fread(file = path) # dt_collect is assumed UTC by fread
    
    # Add unique rowid. Character eases combining with sentinel dataset later
    sat_data[, rowid := .I]
    sat_data$rowid <- as.character(sat_data$rowid)
    
    # Aggregate MOD and MYD datasets, recode
    sat_data[, dataset_type := .(fcase(
        grepl("M[OY]D10_L2", file_stem), "MODIS_L2",
        grepl("M[OY]D10A1", file_stem), "MODIS_L3",
        grepl("VNP10_", file_stem), "VIIRS_L2",
        grepl("VNP10A1", file_stem), "VIIRS_L3"
    ))]
    if (raw_only) 
        return(sat_data)
    
    # Identify invalid data
    sat_data[, valid :=.(fcase(
        ndsi == -32768, FALSE, #MODIS invalid
        ndsi > 20000, FALSE, #VIIRS invalid
        cover_default > 100, FALSE,
        bit5 == 1, FALSE,
        bit7 == 1, FALSE,
        grepl("MYD10_L2_A2022181_2250", file_stem), FALSE,
        rep(TRUE, nrow(sat_data)), TRUE
    ))]

    # Summarise data for each polygon
    # This ran faster if split into two parts and results joined (hence `a` 
    # and `b`)

    # poly_covers_summarised <-
    #     sat_data[,.(
    #         fraction_valid = sum(valid * fraction),
    #         cover_weighted = sum(cover * valid * fraction) / sum(valid * fraction),
    #         dt_collect = first(dt_collect),
    #         sat_csv_firstrowid = first(rowid),
    #         poly_type = first(poly_type)
    #     ), .(poly_id,file_stem)]
    
    # Calculate degree of overlap between polygons and raster cells
    a <- sat_data[,.(
        fraction_valid = sum(valid * fraction),
        cover_fraction_valid = sum(cover * valid * fraction)#,
        ), .(poly_id,file_stem)]
    # calculate weighted cover for each polygon 
    a[, cover_weighted := (cover_fraction_valid / fraction_valid)]
    
    # Gather details for each polygon 
    b <- sat_data[,.(
        dt_collect = first(dt_collect),
        rowid = first(rowid), 
        poly_type = first(poly_type),
        dataset_type = first(dataset_type)
    ), .(poly_id,file_stem)]
    
    # Recombine 
    poly_covers_summarised <- a[b, on = c("poly_id", "file_stem")]
    
    # Exclude polygons with <50% valid coverage. Create cover_Fudge column
    poly_covers_summarised <- poly_covers_summarised[
        fraction_valid > 0.5,
          ][, cover := cover_weighted
            ][,cover_fudge := fifelse(cover == 1, 0.9999, cover)
              ][,cover_fudge := fifelse(cover_fudge == 0, 0.0001, cover_fudge)
                ][
                    ][order(poly_id, dt_collect)]
    
    setcolorder(poly_covers_summarised, 
                c('poly_id', 'dt_collect', 'poly_type', 'cover', 
                'cover_fudge', 'fraction_valid', 'rowid',
                'dataset_type'))
    setDF(poly_covers_summarised)
    
    return(poly_covers_summarised)
}

#' Load sentinel data-2 snow cover
#' 
#' This loads the relevant sentinel-2 data, which were extracted by
#' Tom Versluijs using scripts at https://github.com/TVersluijs/RGEE_Snowmelt. 
#' 
#' Snow cover was estimated for target polygons using three approaches, 
#' 1) the Salomonson & Appel (2006) formula to convert NDSI to FSC, 
#' 2) the Gascoin et al. (2020)  formula to convert NDSI to FSC, and
#' 3) an NDSI thresholding & binarization approach 
#' 
#' Approaches 1 and 2 calculate the average NDSI and FSC over all pixels within
#' the target polygon. Approach 3 is described in the above github repository
load_snow_data_sentinel <- function(raw_fsc_only = FALSE){
    # Load files containing NDSI and FSC values for each target polygon 
    f <- dir(here('data', 'satellite', 'sentinel-2'), recursive = TRUE, 
             full.names = TRUE)
    f_rawobs <- f[grepl("_MeanBandValues", f)]
    sentinel_raw_fsc <- map(f_rawobs, \(x) fread(x, data.table = FALSE)) |> 
        set_names(str_extract(f_rawobs, 
                              "(?<=Average_BandValues/)[A-Za-z_]*(?=/)")) |>
        bind_rows(.id = "poly_type")
    
    if(raw_fsc_only) 
        return(sentinel_raw_fsc)
    
    # Tidy file
    sentinel_raw_fsc <- sentinel_raw_fsc |>
        rename(poly_id = SubArea,
               dt_collect = Date) |>
        filter(!is.na(NDSI)) |>
        rename(sentinel_gascoin = FSC_Gascoin2020, 
               sentinel_slmnsn_appl = FSC_Aalstad2020) |>
        pivot_longer(c(sentinel_gascoin, sentinel_slmnsn_appl), 
                     names_to = "dataset_type",
                     values_to = "cover") |>
        mutate(rowid = paste0("s_fsc_", row_number())) |>
        select(poly_type, dt_collect, poly_id, dataset_type, cover, rowid) |>
        mutate(poly_type = tolower(poly_type)) |>
        mutate(poly_type = case_when(
            poly_type == "drone" ~ "drone_grid",
            poly_type == "male_captures" & 
                grepl("_375$", poly_id) ~ "firstcap_buff_375",
            poly_type == "male_captures" & 
                grepl("_500$", poly_id) ~ "firstcap_buff_500",
            poly_type == "nests" & grepl("_375$", poly_id) ~ "nest_buff_375",
            poly_type == "nests" & grepl("_500$", poly_id) ~ "nest_buff_500",
            TRUE ~ poly_type
        )) |> 
        # Seems wrong collection time in file: shows hour 10, but should be 22.
        mutate(dt_collect = dt_collect + hours(12))
    
    # Load sentinel data from the pixel binarization approach
    f_pt_grid <- 
        here('data', 'satellite', 'sentinel-2', 
             "SnowFraction_Pixel", "Drone", 
             "20231116150219_UTQ22_S2_Res20_Locations_Data_SnowFraction.csv")
    f_full_plot <- 
        here('data', 'satellite', 'sentinel-2', 
             "SnowFraction_Pixel", "Full_plot", 
             "20231116151111_UTQ22_S2_Res20_Locations_Data_SnowFraction.csv")
    f_malecaps <- 
        here('data', 'satellite', 'sentinel-2', 
             "SnowFraction_Pixel", "Male_captures", 
             "20231116151739_UTQ22_S2_Res20_Locations_Data_SnowFraction.csv")
    f_nests <- 
        here('data', 'satellite', 'sentinel-2', 
             "SnowFraction_Pixel", "Nests", 
             "20231116144559_UTQ22_S2_Res20_Locations_Data_SnowFraction.csv")
    
    binary_pt_grid <- read.csv(f_pt_grid)
    binary_fullplot <- read.csv(f_full_plot)
    binary_malecaps <- read.csv(f_malecaps)
    binary_nests <- read.csv(f_nests)
    
    sentinel_binary <- 
        bind_rows(binary_pt_grid,
                  binary_fullplot,
                  binary_malecaps,
                  binary_nests) |>
        rename(poly_id = LocationID,
               cover = SnowFraction) |> 
        mutate(dataset_type = "sentinel_binary") |>
        # Does doy = 1 essentially mean 1st Jan, 00:00:01, or 1st Jan 23:59:59?
        mutate(dt_collect = dmy_hms("01/01/2022 00:00:00") + days(doy-1)) |>
        mutate(rowid = paste0("s_bin_", row_number())) |>
        select(-NDSI_threshold, -doy) |>
        mutate(poly_type = case_when(
            poly_id == "full_plot" ~ "full_plot",
            grepl("pt_", poly_id) ~ "drone_grid",
            grepl("^M.*_375$", poly_id) ~ "firstcap_buff_375",
            grepl("^M.*_500$", poly_id) ~ "firstcap_buff_500",
            grepl("^P.*_375$", poly_id) ~ "nest_buff_375",
            grepl("^P.*_500$", poly_id) ~ "nest_buff_500",
            TRUE ~ NA
        )) 
    
    # Combine all sentinel data
    bind_rows(sentinel_raw_fsc, sentinel_binary) |>
        # ignore 375m diameter regions
        filter(!grepl("_375$", poly_id)) |>
        # Match polygon ids to format in MODIS/VIIRS datasets
        mutate(poly_id = case_when(
            grepl("_500$", poly_id) ~ gsub("_500", "_buff_500", poly_id),
            TRUE ~ poly_id)
        ) |>
        mutate(cover_fudge = fifelse(cover == 1, 0.9999, cover),
               cover_fudge := fifelse(cover_fudge == 0, 0.0001, cover_fudge)) |>
        mutate(dt_collect = `second<-`(dt_collect, 0))
}

#' Get GPS coordinates for drone sampling grid points
#' 
#' @param path path to csv containing sampling grid points
#' 
#' @returns sf data frame with columns point_id, geometry (lat/lon)
get_dr_coords <- function(path = here("data", "drone-sampling-coords", 
                                  "sampling-grid-coordinates.csv")){
    read.csv(path) |> 
        rename(pt_id = ID) |>
        mutate(pt_id = paste0("pt_", pt_id)) |>
        # points 3 and 5 unused
        filter(pt_id != "pt_3" & pt_id != "pt_5") |>
        st_as_sf(coords = c("X", "Y"), crs = CRS_WGS84)
}

#' Get the extent of the drone surveyed area
#' 
#' Any location within 200m of a sampling coordinate will be considered part of 
#' the study area
#' 
#' @returns an sfc polygon object giving the outline of the surveyed area
get_drone_survey_extent <- function(){
    # Get precise outline of long-term study plot, which clips waterbodies and 
    # other natural boundaries
    plot_outline <- 
        read_sf(here("data", "plot-outline", "plot_outline.shp")) |> 
        st_transform(crs = 4326) |> 
        select(geometry)

    # Get the area within 200m of a sampling point
    study_plot_extent <- load_snow_data_drone() |>
        # Convert to UTM zone 4N EPSG:32604 to change unit to meters
        st_transform(32604) |>
        st_buffer(200) |>
        st_union() |>
        st_transform(crs = 4326)

    # retain the part of the study plot covered by drone surveys
    st_intersection(study_plot_extent, plot_outline)
}

#' Create rectangular buffer around polygon
#'
#' Creates a rectangular buffer around point geometries 
#' The CRS is reprojected such that the units in meters, and width = easting 
#' in meters and height = northing in meters. 
#' 
#' @param pts sf data.frame with point geometry
#' @param width width of the rectangular buffer in meters (easting)
#' @param height height of the rectangular buffer in meters (northing)
rect_buffer <- function(pts, width, height){
    # Remember original CRS
    orig_crs <- st_crs(pts)
    
    # The first 
    # Put box around each point
    boxes <- pts |> 
        # Convert points to projected CRS with units in meters 
        st_transform(CRS_BARROW) |>
        pull(geometry) |>
        # bbox every points
        map(st_bbox) |>
        # Expand box to width and height. Assumes image width is north/south,
        map(~.x + c(-height/2,-width/2,height/2,width/2)) |>
        # convert back to sf
        map(~st_as_sfc(.x)) |>
        map_df(st_as_sf) |>
        rename(geometry = x)
    
    # CRS is not retained in the previous operations, tell it what the CRS is
    st_crs(boxes) <- CRS_BARROW
    
    # Return to original CRS, and replace points with polygons 
    boxes |> 
        st_transform(orig_crs) |>
        bind_cols(st_drop_geometry(pts))
}

