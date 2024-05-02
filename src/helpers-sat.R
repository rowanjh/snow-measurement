library(terra) 
library(stringr)
library(lubridate)

# get_modis_filestem <- function(f){
#     # Take a path and extract the MODIS filename (cut off directory etc)
#     str_extract(f, "(?<=/)M[OY]D10.*(?=.tif)")
# }

# get_swath_stem <- function(f){
#     # Get the stem of a modis filename (e.g. ignore _ndsi, _ndsi_algorithm_flags)
#     str_extract(f, "^.*MOD_Swath_Snow_NDSI")
# }

#' Get the datetime from a vector of MODIS/VIIRS filestems
#'
#' Satellite dts are in UTC, and will get converted to barrow time.
get_stem_dt <- function(stems){
    # Remove directories/extensions around the filename
    # dt_string <- str_extract(stem, "(?<=A)[0-9]{7}*_+[0-9]{0-4}")
    dts <- str_extract(stems, "(?<=_A)[0-9]{7}(_[0-9]{4}){0,1}")
    result <- rep(as.POSIXct(NA), length(dts))
    
    # If full datetime is given, convert to POSIXct 
    result[nchar(dts) == 12] <- dts[nchar(dts) == 12] |>
        strptime(format = "%Y%j_%H%M", tz = "UTC") |>
        with_tz("America/Anchorage") |> 
        as.POSIXct()
    # If only yearday is given (e.g. daily image), give it a time of Noon UTC.
    result[nchar(dts) == 7] <- dts[nchar(dts) == 7] |>
        strptime(format = "%Y%j", tz = "UTC") |>
        as.POSIXct() %>%
        {. + hours(12)} |>
        with_tz("America/Anchorage")
    return(result)
}

# Load a stack of 4 GeoTIFF files given a swath stem (NDSI, snow cover, basicqa, algorithm flags)
load_swath_stack <- function(swath, files){
    set <- files[grepl(swath, files)]
    rndsi <- rast(set[!grepl("Snow_Cover", set)])
    rcover <- rast(set[grepl("Snow_Cover_(?![AB])", set, perl = TRUE)])
    rbasicqa <- rast(set[grepl("Basic_QA", set)])
    rflags <- rast(set[grepl("Flags", set)])
    rall <- c(rndsi, rcover, rbasicqa, rflags)
    names(rall) <- c("ndsi", "cover", "basicqa", "flags")
    return(rall)
}

get_matches <- function(strings, pattern){
    strings[grepl(pattern, strings, perl = TRUE)]
}

# Extract raster values at point 
