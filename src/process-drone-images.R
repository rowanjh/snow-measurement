# ~~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' This script is provided with the following paper:
#' 
#' <Anonymized>
#' 
#' -----------
#' Description
#' -----------
#' Purpose: 
#'      Process ~800 raw drone DNG photograph files to find %cover for each. A 
#'      lower-resolution snow mask is also exported that can be used for spatial 
#'      autocorrelation modelling. 
#'      
#' Script Outputs:
#'      CSV giving the percent snow cover for each drone image
#'      `./outputs/drone-cover-csv/drone-snowcover_{processed_dt}.csv`:
#'           Columns include %cover, point id, lat/lon, date of image capture 
#'           for each drone image. Datetime is in America/Anchorage timezone.
#' 
#'      ~800 snow mask .png files
#'      `./outputs/drone-snow-masks/{process_dt}/`:
#'          Each drone image is scaled down from 5472×3648 pixel resolution to a 
#'          164 x 109 px resolution (factor of 0.03), and has the snow 
#'          classification threshold applied. The threshold calculated from the 
#'          full-scale drone image is used. The scaling factor was tuned by 
#'          comparing snow masks at different resolutions, and selecting the 
#'          lowest scaling factor that appeared to retain the spatial structure. 
#'          1 pixel in the scaled down image corresponds to a ground area of 
#'          approximately 0.5m x 0.5m
#'      
#'      20 .pngs showing performance of snow classification algorithm. 
#'      `./outputs/drone-thresh-viz/`
#'          Each png includes a drone photograph, a mask showing which pixels
#'          were classified as snow-covered (white) or snow-free (black) for 
#'          that photo, and the blue pixel intensity histogram with the snow 
#'          classification threshold given with a red line.
#'      
#'  
#'  Notes: 
#'      Assumes drone photographs are in the folder `./data/drone-photos`
#'      
#'      Most of the processing steps are implemented in the python script 
#'      `process-drone-images.py`
#'      
#'      Each photograph was captured at one of 38 sampling points. To determine 
#'      which sampling point a photograph corresponds to, the location metadata 
#'      in each image file is compared to the sampling point locations, given
#'      in `data/drone-sampling-coords/sampling-grid-coordinates.csv`
#'      
# ~~~~~~~~~~~~~~~ Load packages & Initialization ~~~~~~~~~~~~~~~~~~~~~~~~----
library(reticulate)
library(here)
library(doFuture)
library(dplyr)
library(glue)
time_start <- Sys.time()

# Create a python environment with required modules using reticulate
install_python(version = '3.10')
virtualenv_create("snow-drone", python = "3.10")
virtualenv_install("snow-drone", c("numpy==1.26.4", 
                                   "scikit-image==0.22.0",
                                   "rawpy==0.19.1", 
                                   "exifread==3.0.0", 
                                   "pyproj==3.6.1",
                                   "matplotlib==3.8.4"))


# Prepare output paths & directories
outpath_cover_csv <- here("outputs", "drone-cover-csv", "drone-snowcover.csv")
outdir_masks <- here('outputs', 'drone-snow-masks')
outdir_thresh_viz <- here('outputs', 'drone-thresh-viz')

if (!dir.exists(here("outputs", "drone-cover-csv")))
    dir.create(here("outputs", "drone-cover-csv"), recursive = TRUE)
if (!dir.exists(outdir_masks))
    dir.create(outdir_masks, recursive = TRUE)
if (!dir.exists(outdir_thresh_viz))
    dir.create(outdir_thresh_viz, recursive = TRUE)

# List all image files 
files <- list.files(here("data", "drone-photos"), recursive = TRUE,
                    pattern = ".DNG$")

# Use multiple cores
# n_cpu_cores <- 4
n_cpu_cores <- min(20, parallel::detectCores())

# ~~~~~~~~~~~~~~~ Process image files ~~~~~~~~~~~~~~~~~~~~~~~~----
# Process files in loop: output snow masks and get pct cover etc. for each file. 

file_chunks <- split(here("data", "drone-photos", files), 
                     cut(seq_along(files),n_cpu_cores, labels = FALSE))

# cl = makePSOCKcluster(n_cpu_cores)  
# registerDoParallel(cl)
plan(multisession, workers = n_cpu_cores)
results <- foreach(chunk = file_chunks) %dofuture% {
    use_virtualenv("snow-drone")
    source_python(here("src", "process-drone-images.py"))
    pt_coords <- load_pt_coords(here("data", "drone-sampling-coords", 
                                     "sampling-grid-coordinates.csv"))
    covers <- list()
    for (f in chunk){
        covers[[f]] <- process_file(f, pt_coords, outdir_masks)
    }
    return(covers)
}
plan(sequential)

cover_data <- unlist(results, recursive = FALSE)

# ~~~~~~~~~~~~~~~ Export csv ~~~~~~~~~~~~~~~~~~~~~~~~----
# Save csv file with %cover for each image
covers_df <- lapply(cover_data, function(f) {
    file_label <- strsplit(f$file, "/")[[1]] |>
        rev() |>
        (`[`)(1:4) |> 
        rev() |> 
        paste(collapse = "/")
    data.frame(id = f$point_id,
               lat = f$lat_lon[[1]],
               lon = f$lat_lon[[2]],
               dt = f$img_dt,
               cover = f$cover,
               thresh = f$thresh,
               file = file_label)
    }) |>
    bind_rows() |> 
    arrange(id, dt)

time_end <- Sys.time()
print(glue("processing time: {difftime(time_end, time_start, units = 'mins') |> round()} mins"))

write.csv(covers_df, file = outpath_cover_csv, row.names = FALSE)

# ~~~~~~~~~~~~~~~ Plot snow classification thresholds ~~~~~~~~~~~~~~~~~~~~~~----
# Get a random sample of drone images
set.seed(123)
fs <- sample(files, 20)
use_virtualenv("snow-drone")
source_python(here("src", "process-drone-images.py"))

# Prep output paths
outpaths <- here(outdir_thresh_viz, gsub("/", "_", fs))
outpaths <- gsub(".DNG$", ".PNG", outpaths)

# Plot the sample drone images, save to output folder
for (i in 1:length(fs)){
    plot_file(f = here("data", "drone-photos", fs[i]), 
              outpath = outpaths[i])
}
# Clean up
rm(list=ls())
