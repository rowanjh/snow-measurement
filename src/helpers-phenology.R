library(tidyr)
library(dplyr)
library(lubridate)

TZ_AK <- "America/Anchorage"
CRS_BARROW <- paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                     ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
CRS_WGS84 <- 4326

#' Load nest dataset
#' 
#' The nests dataset is provided in `nests.csv`. Rows represent an observation 
#' of a nest on a single occasion. Most nests will have multiple observations
#' 
#' @returns data.frame nest observations in rows
get_nest_observations <- function(){
    nests <- read.csv(here("data", "bird-monitoring", "nests.csv"),
                      na.strings = "") |>
        mutate(datetime = ymd_hms(datetime, truncated = 3), tz = TZ_AK)
}

#' Summarise nest observations 
#' 
#' Creates a table with 1 row per nest, containing pertinent information for 
#' each nest
#' @param nests nests dataset as returned by `get_nest_observations()`
#' @param eggs eggs dataset as returned by `get_egg_data()`
#' 
#' @returns data.frame with one nest per row, and key nest details in columns
summarise_nests <- function(nests, eggs){
    coords <- get_nest_coords(nests)
    timeline <- get_nest_timeline(nests, eggs)
    result <- coords |> 
        left_join(timeline, by = 'nest') 
    if(any(duplicated(result$nest))) 
        stop("Problem, duplicate nest IDs in output")
    return(result)
}

#' Add GPS coordinates to nest dataset
#' 
#' @param nests nests dataset with with gps_id and gps_pt columns
#' @return sf data.frame object with nest id and GPS location
get_nest_coords <- function(nests){
    # Load hand GPS
    GPS_hand <- 
        read.csv(here("data", "bird-monitoring", "gps-points.csv"), 
                 na.strings = "") |>
        select(gps_id, gps_point, lat, lon, datetime_) |>
        mutate(datetime_ = ymd_hms(datetime_, tz = TZ_AK))
    
    # Add coordinates to nest data 
    nest_coords <- nests |> 
        filter(nest_state == "F") |> 
        left_join(GPS_hand |> select(-datetime_)) |>
        select(nest, lat, lon) |>
        st_as_sf(coords = c('lon','lat'), crs = 4326)
    return(nest_coords)
}

#' Get fates of nests
#'
#' Determine whether nests survived or failed. The nests dataframe is summarised
#' to accomplish this

#' @param nests nests dataset as returned by `get_nest_observations()`
#' @returns a data.frame containing columns nest id, and fate
get_nest_fates <- function(nests){
    fates <- nests |>
        group_by(nest) |>
        summarise(fate = case_when("H" %in% nest_state ~ "Survived",
                                   "P" %in% nest_state ~ "Predated",
                                   "D" %in% nest_state ~ "Deserted",
                                   TRUE ~ "Failed (Oth)"))

    # Nests P214 and P1003 were still active at the end, until they were 
    # released by researchers due to having no chicks left in the incubator
    fates[fates$nest == "P214",'fate'] <- "Survived"
    fates[fates$nest == "P1003",'fate'] <- "Survived"
    return(fates)
}

get_nest_initiation <- function(nests, eggs){
    #' Estimate nest initiation dates
    #' 
    #' Use direct estimation where available, otherwise use hatch date - 22, or 
    #' as a last resort, flotation hatch estimate - 22.
    #'
    #' @param nests nests dataset as returned by `get_nest_observations()`
    #' @param eggs eggs dataset as returned by `get_egg_data()`
    #' 
    #' @returns data.frame with estimates of initiation time for each nest
    if("sf" %in% class(nests)){
        # sf breaks join functions, remove the geometry column
        nests <- nests |>st_drop_geometry()
    }
    # Clutch size when nest was found
    Fclutch <- nests %>% 
        filter(nest_state == "F") %>%
        select(nest, datetime, clutch_size) %>%
        rename(found_clutchsize = clutch_size,
               datetime_found = datetime)
    # Maximum clutch size
    maxclutch <- nests %>% 
        group_by(nest) %>% 
        summarise(max_clutch = max(clutch_size, na.rm = TRUE))
    
    # Check if nest was found during laying
    nest_info <- left_join(Fclutch, maxclutch) |>
        mutate(found_state = if_else(found_clutchsize == max_clutch, 
                                     "complete", "laying")) 
    
    # Get estimated hatch datetime
    eggs <- left_join(eggs, estimate_hatch_flotation(eggs))
    
    # Get earliest actual hatch and estimated hatch dates for a nest
    float_info <- eggs |>
        filter(!is.na(est_hatch_dt_float)) %>% # exclude damaged eggs
        group_by(nest) %>%
        summarise(min_est_hatch_dt = min(est_hatch_dt_float))
    hatch_info <- eggs %>%
        filter(!is.na(hatching_datetime)) %>% # exclude damaged eggs
        group_by(nest) %>%
        summarise(min_hatch_date = min(hatching_datetime))
    
    # Estimate initiation date for each nest
    clutch_init <- left_join(nest_info, float_info) |>
        left_join(hatch_info)
    
    clutch_init <- clutch_init |>
        mutate(init_dt = case_when(
            # If found during laying, estimate directly
            found_state == "laying" ~ datetime_found - days(found_clutchsize),
            # If not found during laying, count back from hatch date
            !is.na(min_hatch_date) ~ min_hatch_date - days(22) - days(max_clutch - 1),
            # # If egg didn't hatch, use the float estimate
            is.na(min_hatch_date) ~ min_est_hatch_dt - days(22) - days(max_clutch - 1),
            TRUE ~ as.POSIXct(NA))) |>
        mutate(first_fertile_day = init_dt - days(5),
               last_fertile_day = init_dt - days(1)) %>%
        # Estimate incubation start dt
        mutate(incub_dt = init_dt + days(max_clutch-1)) |>
        filter(!is.na(first_fertile_day))
    return(clutch_init |> select(nest, found_state, init_dt, incub_dt, first_fertile_day, last_fertile_day))
}

#' Get dates of important nest events
#' 
#' Get important events including nest initiation date. Some of these are not 
#' used   
#'
#' @param nests nests dataset as returned by `get_nest_observations()`
#' @param eggs eggs dataset as returned by `get_egg_data()`
#' @returns a data.frame with date of key events (e.g. initiation) for nests
get_nest_timeline <- function(nests, eggs){
    # Get dates for: pre-initiation, initiated, Failed, Hatched.
    preinitiation_times <- nests |>
        filter(nest_state == "F") |>
        rename(found_dt = datetime) |>
        mutate(preinit_dt = ymd_hms("2022-05-20 12:00:00", tz = TZ_AK)) |>
        select(nest, preinit_dt, found_dt)
    
    initiation_times <- get_nest_initiation(nests, eggs)
    
    hatch_times <- nests |> 
        filter(nest_state == "H") |>
        select(nest, datetime) |>
        rename(hatch_dt = datetime)
    
    # Get the first date of inactivity for a nest (i.e. process successive pP,pD)
    # Find when the nest was abandoned or predated 
    # Get the date for transition from active to inactive
    failed_nests <- get_nest_fates(nests) |> 
        filter(grepl("Failed|Predated|Deserted", fate)) |> 
        pull(nest)
    
    nests <- nests |>
        mutate(active = grepl("^F$|^C$|^I$", nest_state))
    
    # Temp helper function
    get_failure_dt <- function(dat){
        last_active_id <- dat |> 
            arrange(datetime) |>
            filter(active) |>
            slice_tail(n = 1) |>
            pull(rowid)
        first_inactive_id <- last_active_id + 1
        
        result <- dat |> filter(rowid == first_inactive_id)
        if (nrow(result) != 1){
            print(dat)
            print(result)
            stop(glue("Failed on nest {dat$nest[1]}"))
        }
        return(result)
    }
    fail_times <- nests |> 
        filter(nest %in% failed_nests) |> 
        split(~nest) |>
        map(get_failure_dt) |>
        bind_rows() |>
        rename(fail_dt = datetime)  |>
        select(nest, fail_dt)
    
    preinitiation_times |>  
        left_join(initiation_times) |> 
        left_join(fail_times) |>  
        left_join(hatch_times)
}

#' Load egg dataset
#' 
#' Eggs were collected from nests and stored in an incubator for hatching. The
#' dataset contains information for every egg in every clutch collected
#' 
#' @returns a data.frame with date of key events (e.g. initiation) for nests
get_egg_data <- function(){
    eggs_incubator <- 
        read.csv(here("data", "bird-monitoring", "eggs-incubator.csv"),
                 na.strings = "") |>
        mutate(arrival_datetime = as.POSIXct(arrival_datetime, tz = TZ_AK),
               hatch_start = as.POSIXct(hatch_start, tz = TZ_AK),
               eggs_in_hatcher = as.POSIXct(eggs_in_hatcher, tz = TZ_AK),
               hatching_datetime = as.POSIXct(hatching_datetime, tz = TZ_AK),
               measure_datetime = as.POSIXct(measure_datetime, tz = TZ_AK))
    return(eggs_incubator)
}

#' Load bird capture data file.
#' 
#' This dataset contains important information about all birds captured during
#' the study. Note, any birds with missing location will be purged from dataset.
#'
#' @returns a data.frame with date of key events (e.g. initiation) for nests
get_capture_data <- function(){
    cap <- read.csv(here("data", "bird-monitoring", "captures.csv"),
                    na.strings = "") |>
        mutate(dt_cap = ymd_hms(paste(date, cap_start), truncated = 3,
                                tz = TZ_AK),
               date = as.Date(date)) 
    # Load hand GPS
    GPS_hand <- read.csv(here("data", "bird-monitoring", "gps-points.csv"), 
                         na.strings = "") |>
        select(gps_id, gps_point, lat, lon)
    
    # Add coordinates to nest data 
    cap <- cap |> 
        left_join(GPS_hand, by = c("gps_id", "gps_point")) |>
        filter(!is.na(gps_point)) |> 
        st_as_sf(coords = c('lon','lat'), crs = CRS_WGS84)    

    return(cap)
}

#' Estimate hatch date using flotation method
#' 
#' Estimates hatch date using Liebezeit 2007's flotation formulae
#'
#' @param eggs eggs dataset as returned by `get_egg_data()`
estimate_hatch_flotation <- function(eggs){
    # PESA flotation model parameters
    fp <- list(a1 = -17.47, b1 = 0.82, a2 = -7.29, b2 = 1.23, c2 = -0.06)
    logit <- binomial()$linkfun
    
    # For float estimation
    eggs |> 
        mutate(est_hatch_dt_float = case_when(
            float_height <= 0 | is.na(float_height) ~ 
                arrival_datetime + 24*60*60 * 
                abs(fp$a1 + fp$b1 * logit((float_angle - 20) / 70) ),
            float_height > 0 ~ 
                arrival_datetime + 24*60*60 * 
                abs(fp$a2 + fp$b2 * float_height + fp$c2 * float_angle),
            TRUE ~ as.POSIXct(NA)
        )) |>
        select(nest, egg_id, est_hatch_dt_float)
}

#' Create a buffer around nest sites 
#' 
#' Uses `st_buffer()` and performs some basic additional tidying 
#'
#' @param nests dataset containing nest locations
#' @param buff_diameter size of buffer to add in meters. Caution: do not supply
#'      the radius, but the diameter of the buffer
buffer_nest <- function(nests, buff_diameter){
    nests |>     
        st_transform(CRS_BARROW) |>
        st_buffer(buff_diameter/2) |>
        select(nest) |>
        mutate(poly_id = glue("{nest}_buff_{buff_diameter}")) |>
        mutate(poly_type = glue("nest_buff_{buff_diameter}")) |> 
        select(-nest)
}

#' Create a buffer around male settlement sites 
#' 
#' Uses `st_buffer()` and performs some basic additional tidying 
#'
#' @param firstcaps dataset containing male capture locations
#' @param buff_diameter size of buffer to add in meters. Caution: do not supply
#'      the radius, but the diameter of the buffer
buffer_firstcap <- function(firstcaps, buff_diameter){
    firstcaps |>     
        st_transform(CRS_BARROW) |>
        st_buffer(buff_diameter/2) |>
        select(ID) |>
        mutate(poly_type = glue("firstcap_buff_{buff_diameter}"),
               ID = glue("M{ID}_buff_{buff_diameter}")) |>
        rename(poly_id = ID)
}
