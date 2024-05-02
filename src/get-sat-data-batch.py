# ~~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' This script is provided with the following paper:
#' Jacques-Hamilton, Valcu, Kwon, Versluijs & Kempenaers (2024).  
#'      Measurement error in remotely sensed fractional snow cover datasets: 
#'      implications for ecological research.
#' 
#' Full code repository available at:
#' https://github.com/rowanjh/barrow-snowmelt
#' 
#' -----------
#' Description
#' -----------
#' 
#' This is an adapted version of the NSIDC's data access notebook available at 
#' https://github.com/nsidc/NSIDC-Data-Access-Notebook
#' 
#' This script downloads MODIS and VIIRS snow cover datasets from year 2022
#' using the NSIDC's data access API. Data are downloaded into the 
#' `./data/satellite` directory
#' 
#' Most of the work is done by the `fetch_nsidc` function, whic is defined in 
#' `get-sat-data-function.py`
#' 
#' The packages required are defined in `src/icepick-environment.yml`, which can
#' be installed with conda. 
#' 
#' An earthdata login and password are required to use the API.
#' 
#' Script can be run from a terminal using:
#' `conda env create -f src/icepick-environment.yml`
#' `conda activate icepick`
#' `pip install pyprojroot`
#' `python src/get-sat-data-batch.py`
#' 
# ~~~~~~~~~~~~~~~ Load modules & Initialization ~~~~~~~~~~~~~~~~~~~~~~~~----
import os
import pyprojroot
from get_sat_data_function import fetch_nsidc
base_path = pyprojroot.find_root(pyprojroot.has_dir(".git"))

years = [2022]

codes = [
    "MOD10_L2",
    "MYD10_L2",
    "MOD10A1",
    "MYD10A1",
    # "VNP10", # reprojected geotiffs seem to be broken.
    "VNP10A1",
]

for code in codes:
    for year in years:
        # Datasets available from different years.
        if year < 2003 and 'MYD10' in code:
            continue
        if year < 2012 and 'VNP' in code:
            continue
        if year == 2014 or year == 2017 and 'VNP' in code:
            # No data available yet for version 2 from 2014 at the time of request???
            continue
        if year < 2018 and 'VJ1' in code:
            continue

        # Reproject L2 products to UTM. L3 products sinusoidal by default
        if code in ['MOD10_L2', 'MYD10_L2', 'VJ110', 'VNP10']:
            proj = 'UNIVERSAL TRANSVERSE MERCATOR'
        else:
            proj = ''
        
        # Download data
        print(f'========== Requesting data for {code} {str(year)} ========\n')
        fetch_nsidc(
            year = str(year),
            dataset_id = code,
            aoi_path = os.path.join(base_path, "misc", "satellite-roi.shp"),
            outpath = os.path.join(base_path, "data", "satellite", code, str(year)),
            proj = proj
        )

