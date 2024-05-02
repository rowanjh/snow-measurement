# Measurement error in remotely sensed fractional snow cover datasets: implications for ecological research

------------------------------------------------------------------------

This is the code repository for the following paper:

`Jacques-Hamilton, R., Valcu, M., Kwon, E., Versluijs, T.S.L., & Kempenaers, B (2024). Measurement error in remotely sensed fractional snow cover datasets: implications for ecological research`

# Overview

------------------------------------------------------------------------

This paper quantifies measurement error in common satellite-derived fractional snow cover datasets, and uses a case study to demonstrates how measurement error can distort conclusions in an ecological study

The dataset for this study can be downloaded from a repository on [Zenodo](link) (<doi:10.5281/zenodo.11058378>)

The code is available on Github (link removed for review).

## Instructions

------------------------------------------------------------------------

1.  Download or clone this code repository into a directory `snow-measurement/`. The script `src/runall.R` gives the overview of the analyses and is the starting point.

2.  Install packages using `renv::restore()` (see `Package management & installation` below)

3.  Download the zip file containing the data for the analyses from : [Zenodo](link) .

4.  Extract data archive into directory `snow-measurement/data`

5.  Run code in `src/runall.R` to reproduce the analyses

## Package management & installation

------------------------------------------------------------------------

This project uses renv for R package management. To install packages, launch `snow-measurement.Rproj`, then run `renv::restore()` to install all of the required packages with correct versions. `renv` will only work if the following three files are in the project: `renv/activate.R` `renv.lock`, and `.Rprofile`. For more information on renv see [here](https://rstudio.github.io/renv/articles/renv.html).

Python is used for some image processing. A python virtual environment is automatically installed and managed using reticulate.

## Notes

------------------------------------------------------------------------

Some parts of the analysis use parallel loops and can have high memory requirements. Use fewer parallel workers to reduce the memory footprint.

## Repository Structure

------------------------------------------------------------------------

-   `src/` contains all code for analyses.
-   `data` contains all drone photographs, satellite rasters, and bird data.
-   `outputs` contains intermediate data files, plots, and rendered notebooks.
-   `misc` contains miscellaneous files used for the analysis

## Dataset Description

------------------------------------------------------------------------

The dataset is available at [Zenodo](link). Data is in a 7zip file, use the 7zip tool to extract the archive https://www.7-zip.org/:

-   `bird-monitoring` contains data from the field study, including datasets for capture, nests, eggs, and field GPS.
-   `drone-photos` contains raw DNG photographs captured by the drone camera during snow surveys. 
-   `drone-sampling-coords` contains a csv file with the coordinates of where photographs were taken during the drone surveys
-   `plot-outline` a shapefile giving the outline of the study plot
-   `satellite` contains satellite rasters for MODIS and VIIRS datasets downloaded from NSIDC's data access API. Sentinel-2 summary data files are also given, which were produced using the scripts at [RGEE_Snowmelt](https://github.com/TVersluijs/RGEE_Snowmelt)
-   `satellite-roi` a shapefile with a polygon containing the study site and surrounding area, which is used for spatial subsetting when fetching data from the NSIDC API
