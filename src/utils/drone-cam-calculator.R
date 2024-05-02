library(purrr)
# Calculates field of view at different heights and camera angles.
# Calculates min and max GSD.

# Mavic pro 2's camera parameters
cam_mavic <- list(
    fov = 77,
    vfov = 47.5,
    hfov = 66.8,
    pxwidth = 5472,
    pxheight = 3648)

# Input Height, Angle, get back image GSD and footprint rectangle
deg_to_rad <- function(angle_degs){
    angle_degs * (pi / 180)
}

#' Calculates the range in FOV for an image, relative to nadir
#'
#' @param angle a number giving the vertical camera angle of the drone, relative
#'      to nadir i.e. nadir = 0, oblique = 45, pointing at horizon = 90
#' @param cam list of properties of the camera, including vfov and hfov. 
image_fov_range <- function(angle, cam){
    list(middle = angle,
         top = angle + cam$vfov / 2,
         bot = angle - cam$vfov / 2,
         left = -(cam$hfov / 2), # always assumes that the camera has no roll,
         right = cam$hfov / 2) # always assumes the camera has no roll)
}

#' Calculate the dimensions of the ground footprint of a drone photograph.
#'
#' This is calculated using the field of view of the camera, the camera angle, 
#' and the height of the drone. It assumes flat topography, and nadir camera 
#' angle. 
#' 
#' @param height height/altitude of the drone
#' @param cam a list with properties of the camera, including field of view.
get_dimensions <- function(height, cam){
    # cam_angle assumed to be nadir
    cam_angle <- 0
    # Get angles of the middle, left, right, top, and bot of the drone photo, 
    # relative to nadir
    angles <- image_fov_range(cam_angle, cam)
    
    # Image width and height: calculate from center point (nadir) to top or right
    img_width <- angles$right |> deg_to_rad() |> tan() * height * 2
    img_height <- angles$top |> deg_to_rad() |> tan() * height * 2
    
    return(list(img_width = img_width, img_height = img_height))
}
