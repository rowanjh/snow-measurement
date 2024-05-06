# ~~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' This script is provided with the following paper:
#' 
#' <Anonymized>#' 
#' -----------
#' Description
#' -----------
#'
#' Purpose: 
#'      This script processes and summarises MODIS & VIIRS satellite images. 
#'      Specifically, it goes over every raster and extracts cover data
#'      at a set of target polygons, namely: 38 quadrats from the drone 
#'      sampling grid (poly_type = 'drone_grid'), 56 nest sites (poly_type = 'nest'), 
#'      132 male capture points (poly_type = 'male_firstcap'). In addition
#'      data were extracted from the full-plot area covered by drone surveys 
#'      (poly_type = full_plot), and the area that was extensively nest 
#'      searched (poly_type = full_nestsearch_area).
#'      
#'      Drone quadrats are created by making a rectangular buffer around drone 
#'      sampling points that approximates the size of a drone photograph's 
#'      footprint (~79m x 52m, long axis oriented north-south). 
#'      exactextractr's raster extraction takes a weighted average cover across
#'      all complete and partial raster cells that intersect the target polygon.
#'          
#'      Refer to the manual of the respective dataset provided by the NSIDC for 
#'      more detailed information about the satellite snow datasets 
#'      e.g. "MODIS Snow Products Collection 6.1 User Guide"
#'
#' Bit Flags
#'      Bit0: inland water screen
#'      Bit1: low reflectance in modis band 2 or 4 (the raw bands used to calc NDSI),
#'            value reverted to "no decision"
#'      Bit2: Low NDSI (0-0.1 NDSI). Cover value reverted to "not snow"
#'      Bit3: Surface unusually warm for snow (band 31 brightness temperature > 281K),
#'            if elevation <1300 the value is reverted to "not snow"
#'      Bit4: 0.25 < SWIR ≤ 0.45: unusuall high reflectance for snow, flag is set but 
#'            snow cover value unaffected. SWIR >0.45 means flag is set and value is 
#'            reverted to "not snow"
#'      Bit5: Probably cloudy according to the cloud mask. Value not changed.
#'      Bit6: Probably clear according to the cloud mask. Value not changed.
#'      Bit7: If solar zenith >70, flag set, value unchanged. In flag is >85, flag 
#'            set and value reverted to "night"
#'
#' Outputs:
#'      outputs/sat-cover-csv/sat-cover.csv
#'          csv file giving details of each satellite image, including cover,
#'          ndsi, qa flags. Each satellite raster has values extracted for each
#'          of the 38 sampling locations, 50 nest sites, and 132 male first 
#'          capture sites. Each sampling location is represented 
#'          by a rectangle giving the approximate ground area captured by a the 
#'          drone photographs. If the rectangle intersects multiple satellite
#'          raster cells, the raster values for every cell is given, and the 
#'          fraction of the rectangle intersecting each cell. 
#'      
# ~~~~~~~~~~~~~~~ Load packages & Initialization ~~~~~~~~~~~~~~~~~~~~~~~~----
library(here)
library(dplyr)
library(exactextractr)
library(terra)
library(doFuture)
library(glue)
library(data.table)
source(here("src", "helpers-sat.R"))
source(here("src", "helpers-cover-geo.R"))
source(here("src", "helpers-phenology.R"))
source(here("src", "utils", "drone-cam-calculator.R"))

# Helpful CRS's
CRS_BARROW <- paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                    ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
CRS_DEFAULT <- 4326

# Get footprint size of drone images (width and height in meters)
DRN_IM_SIZE <- get_dimensions(60, cam_mavic)
EXTENT_DRONE <- get_drone_survey_extent()

# Datasets (assumes there is a folder with this name in data/satellite directory)
datasets <- c("MOD10_L2", "MOD10A1", "MYD10_L2", "MYD10A1", "VNP10A1")
years <- 2022

# Load list of all files
chunks <- crossing(dataset = datasets, year = years)

files <- map2(chunks$dataset, chunks$year, 
              ~dir(here("data", "satellite", .x, .y), full.names = TRUE)) |> 
    set_names(paste(chunks$dataset, chunks$year, sep = "_"))

# Get file stems
stems <- map2(files, names(files), 
              function(x,y){
                  dataset <- substr(y, 1, nchar(y)-5)
                  str_extract(x, paste0("(?<=/)", dataset, "(_[:alnum:]+){4}")) |> 
                      unique()
              })
stems <- stems[map_lgl(stems, ~length(.x) > 0)]

files <- unlist(files, use.names = FALSE)

# Get 2022 nest locations
NESTS_2022 <- get_nest_observations()
EGGS_INCUBATOR <- get_egg_data()
CAPS <- get_capture_data()

NEST_INFO_2022 <- summarise_nests(NESTS_2022, EGGS_INCUBATOR) |>
    st_intersection(EXTENT_DRONE)

# Get male capture locations
MALE_FIRSTCAP <- CAPS |> 
    filter(!is.na(date),
           sex == "m") |> 
    arrange(dt_cap) |> 
    group_by(ID) |> 
    filter(dt_cap == min(dt_cap)) |>
    ungroup()

# Output directory
outdir <- here("outputs", "sat-cover-csv")
if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
}
if (!dir.exists(paste0(outdir, "/temp"))) {
    dir.create(paste0(outdir, "/temp"))
}

# ~~~~~~~~~~~~~~~ Get polygons to extract from satellite rasters ~~~~~~~~~~~----
TARGETS_2022 <- list()

# 50-80m rectangles representing drone photograph footprints
TARGETS_2022$drone_rects <- get_dr_coords() |> 
    rect_buffer(DRN_IM_SIZE$img_width, DRN_IM_SIZE$img_height) |>
    rename(poly_id = pt_id) |> 
    mutate(poly_type = "drone_grid")


# 500m diameter buffer around nests
TARGETS_2022$nest_buff_500 <- NEST_INFO_2022 |>
    buffer_nest(500)

TARGETS_2022$firstcap_buff_500 <- MALE_FIRSTCAP |>
    buffer_firstcap(500)

# 2km2 boundary of 2022 study plot
TARGETS_2022$plot_boundary <- get_drone_survey_extent() |> 
    st_sf() |>
    `st_geometry<-`("geometry") |>
     mutate(poly_id = "full_plot", poly_type = "full_plot")

# Collate all target ROIs
TARGETS_2022 <- TARGETS_2022 |>
    # normlaize CRS
    map(function(x) st_transform(x, CRS_WGS84)) |> 
    # Combine all targets into one df
    bind_rows()

# ~~~~~~~~~~~~~~~ Process MODIS files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
# Loop over every file, extract NDSI snow cover for the grid points.

# Parallelizing only helps a little bit on our system, io bound
n_cpu_cores <- 3
future::plan("multisession", workers = n_cpu_cores)

# Each chunk includes files from 1 dataset. Add dataset ID to the stem
chunks <- map2(names(stems), stems, ~list(id = .x, stems = .y))

time_start <- Sys.time()

sat_covers <- foreach(chunk = chunks, 
                      .options.future = list(scheduling = 1)) %dofuture% {
    chunk_stems <- chunk$stems
    chunk_id <- chunk$id
    
    results <- vector("list", length(chunk$stems))
    for(i in 1:length(chunk$stems)){
        stem <- chunk$stems[i]
        stem_dt <- get_stem_dt(stem)
        # stem <- get_modis_filestem(f)

        # Collect all tiffs representing different channels for this swath/tile 
        this_set <- files[grepl(stem, files)]
        
        # NDSI 
        rndsi <- get_matches(this_set, "_NDSI_(?!Snow)") |> rast()
        # Snow_Cover
        rcover <- get_matches(this_set, "_NDSI_Snow_Cover_(?![AB])") |> rast()
        # Snow_Cover_BasicQA
        rbasicqa <- get_matches(this_set, "Basic_QA") |> rast()
        # Snow_Cover_Algorithm flags
        rflags <- get_matches(this_set, "Algorithm") |> rast()

        # Stack raster layers
        rall <- c(rndsi, rcover, rbasicqa, rflags)
        names(rall) <- c("ndsi", "cover_default", "basicqa", "flags")
        
        extracts <- exact_extract(rall, 
                      TARGETS_2022 |> st_transform(st_crs(rall)), 
                      include_cell = TRUE, 
                      include_cols = c("poly_id", "poly_type"),
                      progress = FALSE) |> 
            bind_rows() 
        
        # Tidy and add extra information
        extracts <- extracts |> 
            group_by(poly_id) |> 
            mutate(fraction = coverage_fraction / sum(coverage_fraction),
                   dt_collect = stem_dt,
                   file_stem = stem) |>
            relocate(poly_id, poly_type, dt_collect) |> 
            ungroup()
        results[[i]] <- extracts
    }
    results <- bind_rows(results)
    fwrite(results |> mutate(dt_collect = as.character(dt_collect)), 
           file = here("outputs", "sat-cover-csv", "temp", 
                                glue("{chunk_id}.csv")))
    results
}
time_end <- Sys.time()
print(glue("processing time: {difftime(time_end, time_start, units = 'mins') |> round()} mins"))
plan(sequential)

COVERS_SAT <- bind_rows(sat_covers)

# # To fetch intermediate files from a job (e.g. in case of forloop error)
# fi <- dir(here("outputs", "sat-cover-csv", "temp")) %>% gsub(".csv", "", .)
# all(names(stems) %in% fi) & all(fi %in% names(stems))
# fif <- dir(here("outputs", "sat-cover-csv", "temp"), full.names = TRUE)
# sat_covers <- map(fif, ~fread(.x, data.table = FALSE))

# Or simply:
# sat_covers <- map(dir(here("outputs", "sat-cover-csv", "temp"), full.names = TRUE), fread)

# ~~~~~~~~~~~~~~~ Clean up sat cover data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
# Add descriptive columns for error types and QA flags. 

COVERS_SAT <- COVERS_SAT |> 
    mutate(
        # Reason for having no snow cover value
        cover_comment = case_when(
            cover_default == 200 ~ "missing data",
            cover_default == 201 ~ "no decision",
            cover_default == 211 ~ "night",
            cover_default == 237 ~ "inland water",
            cover_default == 239 ~ "ocean",
            cover_default == 250 ~ "cloud",
            cover_default == 251 ~ "missing L1B data VIIRS",
            cover_default == 252 ~ "L1B calib fail VIIRS",
            cover_default == 253 ~ "bowtie trim VIIRS",
            cover_default == 254 ~ "detector saturated",
            cover_default == 255 ~ "NA fill",
            cover_default >= 0 & cover_default <= 100 ~ "cover value given",
            TRUE ~ "Unknown comment code"),
        # basic QA coding
        basicqa_code = case_when(
            basicqa == 0 ~ "best",
            basicqa == 1 ~ "good",
            basicqa == 2 ~ "ok",
            basicqa == 3 ~ "poor",
            basicqa == 4 ~ "other",
            basicqa == 211 ~ "night",
            basicqa == 239 ~ "ocean",
            basicqa == 251 ~ "missing L1B data VIIRS",
            basicqa == 252 ~ "L1B calib fail VIIRS",
            basicqa == 253 ~ "bowtie trim VIIRS",
            basicqa == 255 ~ "unusable input or no data",
            TRUE ~ "unknown comment code"))

COVERS_SAT <- COVERS_SAT |> 
    mutate(ndsi_scaled = case_when(
        # VIIRS datasets
        grepl("VNP", file_stem) ~ ndsi * 0.001,
        grepl("M[OY]D", file_stem) ~ ndsi * 0.0001,
        TRUE ~ NA),
    cover = ndsi_scaled * 1.45 - 0.01,
    # Truncate the adjusted cover to within 0-1 range
    cover = ifelse(cover < 0, 0, cover),
    cover = ifelse(cover > 1, 1, cover))


# Get algorithm binary flag QA details
getbits <- function(int){
    row <- intToBits(int)[1:8] |> t()
    return(row)
}

flags <- map(COVERS_SAT$flags, getbits) |>
    unlist() |>
    as.integer() |>
    matrix(ncol = 8, byrow = TRUE) |> 
    as.data.frame() |> 
    set_names(paste0("bit",0:7))

COVERS_SAT <- cbind(COVERS_SAT, flags)

# Reorder 
COVERS_SAT <- COVERS_SAT |> 
    relocate(poly_id, poly_type, dt_collect, cover)

# Export as csv
fwrite(COVERS_SAT |> mutate_all(as.character), 
       here("outputs", "sat-cover-csv", "sat-cover-nsidc.csv"),
       row.names = FALSE)
