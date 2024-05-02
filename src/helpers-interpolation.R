library(purrr)
#' Get date that a %cover threshold was crossed. 
#' 
#' Take a timeseries of cover values, and estimate the dt at which a critical 
#' threshold was crossed. The input should be a vector of time values, and a
#' vector giving %cover for those time values (numeric). Linear interpolation is
#' used to find the time of threshold crossing. If there are multiple crossings,
#' only the first is used
#' 
#' @param dt (POSIXct or numeric) vector of datetimes
#' @param cover (numeric) vector of cover values corresponding to avoce times 
#' @param thresh (numeric) vector of thresholds to calculate snowmelt for
#' @returns data.frame with two columns: threshold, and time (given in  original 
#'          time units)
get_linterp_thresholds <- function(dt, cover, thresh){
    if ("POSIXct" %in% class(dt)){
        thistz <- tz(dt)
    }
    dt_ests <- thresh |> 
        # Find the first time at which each threshold is crossed.
        map(~RootLinearInterpolant(dt, cover, .x)[1]) |> 
        unlist()
    if ("POSIXct" %in% class(dt)){
        dt_ests <- as.POSIXct(dt_ests, tz = tz(dt), 
                              origin = "1970-01-01 00:00.00 UTC")
        data.frame(thresh = thresh, dt = dt_ests)
    } else{ 
        data.frame(thresh = thresh, dt_offset = dt_ests)
    }
    
}

#' Estimate time(s) of crossing a target threshold
#' 
#' Takes a time series of x-y points, and returns times at which
#' a target y value is predicted to have occurred according to piecewise 
#' linear interpolation. 
#' 
#' Uses code from below stackoverflow post for analytical root finding:
#' stackoverflow.com/questions/52650467/how-to-estimate-x-value-from-y-value-input-after-approxfun-in-r
#' 
#' @param x vector of time values 
#' @param y vector of y (snow cover) values corresponding to the times in x
#' @param y0 threshold y value to estimate time of crossing(s)
#' 
#' @return time(s) at which threshold y0 is crossed
RootLinearInterpolant <- function (x, y, y0 = 0) {
    # First check if solution is exactly in series (most useful for y0 = 0)
    if (y0 %in% y){
        idx <- which(y %in% y0)
        return(x[idx])
    }
    # If not, use code from:
    # Code from stackoverflow.com/questions/52650467/how-to-estimate-x-value-from-y-value-input-after-approxfun-in-r
    if (is.unsorted(x)) {
        ind <- order(x)
        x <- x[ind]; y <- y[ind]
    }
    z <- y - y0
    ## which piecewise linear segment crosses zero?
    k <- which(z[-1] * z[-length(z)] < 0)
    ## analytically root finding
    xk <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
    xk
}
