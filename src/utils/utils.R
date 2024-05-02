#' Create an named empty list from a vector.
#' 
#' The list will have the same length as the vector, and each list element
#' will be named according to the vector. Every list element can be pre-filled 
#' with contents (e.g. an empty nested sub-list, common for result-holder objects)
init_list <- function(names, contents = NULL){
    results <- vector("list", length(names)) |> set_names(names)
    # If contents, populate every element of the list with contents
    if(!is.null(contents)){
        results <- map(results, function(x)contents)
    }
    results
}

#' Get difference between two datetime objects in seconds, return as numeric
dtime_secs <- function(dts1, dts2){
    difftime(dts1, dts2, units = "secs") |> as.numeric()
}
#' Get difference between two datetime objects in hours, return as numeric
dtime_hrs <- function(dts1, dts2){
    difftime(dts1, dts2, units = "hours") |> as.numeric()
}

#' Get difference between two datetime objects in days, return as numeric
dtime_days <- function(dts1, dts2){
    difftime(dts1, dts2, units = "days") |> as.numeric()
}

# Round a number, keeping trailing zeros
rnd <- function(num, places = 2){
    format(round(num, digits=places), nsmall = places) 
}

# convert a date to day of year (in decimal). All dates should be from same 
# year or another implementation is needed
doy <- function(dates){
    thisyear <- unique(year(dates))
    if (length(thisyear) > 1) 
        stop("function breaks if all dates not from same year")
    year_origin <- ymd_hms(paste0(thisyear, "-01-01 00:00:00"), tz = tz(dates))
    doy <- dtime_days(dates, year_origin)
    return(doy)
}