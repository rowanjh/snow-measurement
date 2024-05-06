# ~~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' This script is provided with the following paper:
#' 
#' <Anonymized>
#' -----------
#' Description
#' -----------
#' 
#' This script defines a function that cab fetch snow cover data from the 
#' NSIDC's data access API. The dataset ID, year & dates, aoi, and reprojection 
#' options can be specified, these should adhere to the requirements of the API.
#' 
#' The packages required are defined in `src/icepick-environment.yml`, which can
#' be installed with conda: `conda env create -f src/icepickenvironment.yml`
#' 
#' An earthdata login and password are required to use the API. If these are 
#' incorrect the script will termiante with an error
#' 
# ~~~~~~~~~~~~~~~ Load modules & Initialization ~~~~~~~~~~~~~~~~~~~~~~~~----
# From the NSIDC-Data-Access-Notebook
# Use conda 'icepick' environment (code/icepick-environment.yml)
import requests
import getpass
import json
import zipfile
import io
import math
import os
import shutil
import pprint
import re
import time
import geopandas as gpd
import fiona
import matplotlib.pyplot as plt
# To read KML files with geopandas, we will need to enable KML support in fiona (disabled by default)
fiona.drvsupport.supported_drivers['LIBKML'] = 'rw'
from shapely.geometry.polygon import orient
from statistics import mean
from datetime import datetime

###
# # Parameters needed
# dataset_id = 'MYD10_L2' # MOD10_L2 or MYD10_L2
# aoi_path = os.path.join(os.getcwd(), 'misc', 'satellite_roi', 
#                         'plot_extent_buffered_100km.shp')

def fetch_nsidc(year, dataset_id, day_start = '05-10', 
                day_end = '07-15', aoi_path = None, outpath = None,
                proj = 'UNIVERSAL TRANSVERSE MERCATOR'):
    """
    Fetches data from the nsidc API, e.g. MODIS data. Every layer from a 
    scene will be saved as a .tif. 

    Hard codes an earthdata username, and assumes password saved in a file.

    args:
        dataset_id: the dataset code, e.g. MOD10_L2, VNP10
        start_date: fetches all available images between start_date and end_date
        end_date: fetches all available images between start_date and end_date
        aoi_path: shapefile showing region of data to fetch.
        outpath: folder that satellite scenes will be saved to.
    """
    start_date = f'{year}-{day_start}'
    end_date = f'{year}-{day_end}'
    # Process inputs
    start_time = '00:01:00'
    end_time = '23:59:00'

    if not aoi_path:
        aoi_path = os.path.join(os.getcwd(), 'misc', 'satellite-roi', 
                                'roi_rect_5km.shp')
        
    time_now = datetime.today().strftime('%Y-%m-%d_%H%M')
    if not outpath:
        outpath = os.path.join(os.getcwd(), 'data', 'images-modis', 
                                f"{year}_{dataset_id}_{time_now}")    

    # Create an output folder if the folder does not already exist.
    if not os.path.exists(outpath):
        os.makedirs(outpath, exist_ok=True)

    print(f"Outputting images to {outpath}")
    
    ### Input Earthdata Login credentials
    uid = input('Earthdata Login user name: ') 
    pswd = getpass.getpass('Earthdata Login password: ')
    email = input('Email address associated with Earthdata Login account: ')

    ### Select data set and determine version number
    params = {
        'short_name': dataset_id
    }

    cmr_collections_url = 'https://cmr.earthdata.nasa.gov/search/collections.json'
    response = requests.get(cmr_collections_url, params=params)
    results = json.loads(response.content)

    # Find all instances of 'version_id' in metadata and print most recent version number

    versions = [el['version_id'] for el in results['feed']['entry']]
    latest_version = max(versions)
    print('The most recent version of ', dataset_id, ' is ', latest_version)

    ### Select time period of interest
    #Input temporal range 
    temporal = start_date + 'T' + start_time + 'Z' + ',' + end_date + 'T' + end_time + 'Z'

    ### Select area of interest
    aoi = '2'
    # Use geopandas to read in polygon file
    # Note: a KML or geojson, or almost any other vector-based spatial data format could be substituted here.
    # Go from geopandas GeoDataFrame object to an input that is readable by CMR
    gdf = gpd.read_file(aoi_path)

    # CMR polygon points need to be provided in counter-clockwise order. The last point should match the first point to close the polygon.
    
    # Simplify polygon for complex shapes in order to pass a reasonable request length to CMR. The larger the tolerance value, the more simplified the polygon.
    # Orient counter-clockwise: CMR polygon points need to be provided in counter-clockwise order. The last point should match the first point to close the polygon.
    
    poly = orient(gdf.simplify(0.0001, preserve_topology=False).loc[0],sign=1.0)
    
    geojson = gpd.GeoSeries(poly).to_json() # Convert to geojson
    geojson = geojson.replace(' ', '') #remove spaces for API call

    #Format dictionary to polygon coordinate pairs for CMR polygon filtering
    polygon = ','.join([str(c) for xy in zip(*poly.exterior.coords.xy) for c in xy])

    # print('Simplified polygon coordinates based on shapefile input:', polygon)
    
    buffer = gdf.buffer(50) #create buffer for plot bounds
    envelope = buffer.envelope  
    bounds = envelope.bounds
    
    # Load "Natural Earth” countries dataset, bundled with GeoPandas
    world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

    # Overlay glacier outline
    f, ax = plt.subplots(1, figsize=(12, 6))
    world.plot(ax=ax, facecolor='white', edgecolor='gray')
    gdf.plot(ax=ax, cmap='spring')
    ax.set_ylim([bounds.miny[0], bounds.maxy[0]])
    ax.set_xlim([bounds.minx[0], bounds.maxx[0]]);

    ### Determine how many granules exist over this time and area of interest.
    # Create CMR parameters used for granule search. Modify params depending on bounding_box or polygon input.

    granule_search_url = 'https://cmr.earthdata.nasa.gov/search/granules'


    # If polygon file input:
    search_params = {
        'short_name': dataset_id,
        'version': latest_version,
        'temporal': temporal,
        'page_size': 100,
        'page_num': 1,
        'polygon': polygon,
    }

    granules = []
    headers={'Accept': 'application/json'}
    while True:
        response = requests.get(granule_search_url, params=search_params, headers=headers)
        results = json.loads(response.content)

        if len(results['feed']['entry']) == 0:
            # Out of results, so break out of loop
            break

        # Collect results and increment page_num
        granules.extend(results['feed']['entry'])
        search_params['page_num'] += 1

    print('There are', len(granules), 'granules of', dataset_id, 'version', latest_version, 'over my area and time of interest.')

    ### Determine the average size of those granules as well as the total volume
    granule_sizes = [float(granule['granule_size']) for granule in granules]

    print(f'The average size of each granule is {mean(granule_sizes):.2f} MB and the total size of all {len(granules)} granules is {sum(granule_sizes):.2f} MB')

    ### Select the subsetting, reformatting, and reprojection services enabled for your data set of interest.
    # Query service capability URL 

    from xml.etree import ElementTree as ET

    capability_url = f'https://n5eil02u.ecs.nsidc.org/egi/capabilities/{dataset_id}.{latest_version}.xml'

    # Create session to store cookie and pass credentials to capabilities url

    session = requests.session()
    s = session.get(capability_url)
    response = session.get(s.url,auth=(uid,pswd))

    root = ET.fromstring(response.content)

    #collect lists with each service option

    subagent = [subset_agent.attrib for subset_agent in root.iter('SubsetAgent')]
    if len(subagent) > 0 :
        # variable subsetting
        variables = [SubsetVariable.attrib for SubsetVariable in root.iter('SubsetVariable')]  
        variables_raw = [variables[i]['value'] for i in range(len(variables))]
        variables_join = [''.join(('/',v)) if v.startswith('/') == False else v for v in variables_raw] 
        variable_vals = [v.replace(':', '/') for v in variables_join]

        # reformatting
        formats = [Format.attrib for Format in root.iter('Format')]
        format_vals = [formats[i]['value'] for i in range(len(formats))]
        format_vals.remove('')

        # reprojection options
        projections = [Projection.attrib for Projection in root.iter('Projection')]

    ### Select subsetting, reformatting, and reprojection service options, if available.
    #print service information depending on service availability and select service options
    agent = ''
    time_var = start_date + 'T' + start_time + ',' + end_date + 'T' + end_time 
    reformat = 'GeoTIFF'
    # projection = 'UNIVERSAL TRANSVERSE MERCATOR'
    # projection = 'GEOGRAPHIC'
    projection = proj
    projection_parameters = ''
    coverage = ''
    Boundingshape = geojson

    # if len(subagent) < 1 :
    #     print('No services exist for', dataset_id, 'version', latest_version)
    #     agent = 'NO'
    #     bbox = ''
    #     time_var = ''
    #     reformat = ''
    #     projection = ''
    #     projection_parameters = ''
    #     coverage = ''
    #     Boundingshape = ''
    # else:
    #     agent = ''
    #     subdict = subagent[0]
    #     if subdict['spatialSubsettingShapefile'] == 'true' and aoi == '2':
    #         bbox = ''
    #         ps = input('Subsetting by geospatial file (Esri Shapefile, KML, etc.) is available. Would you like to request this service? (y/n)')
    #         if ps == 'y': Boundingshape = geojson
    #         else: Boundingshape = '' 
    #     if subdict['temporalSubsetting'] == 'true':
    #         ts = input('Subsetting by time, based on the temporal range inputted above, is available. Would you like to request this service? (y/n)')
    #         if ts == 'y': time_var = start_date + 'T' + start_time + ',' + end_date + 'T' + end_time 
    #         else: time_var = ''
    #     else: time_var = ''
    #     if len(format_vals) > 0 :
    #         print('These reformatting options are available:', format_vals)
    #         reformat = input('If you would like to reformat, copy and paste the reformatting option you would like (make sure to omit quotes, e.g. GeoTIFF), otherwise leave blank.')
    #         if reformat == 'n': reformat = '' # Catch user input of 'n' instead of leaving blank
    #     else: 
    #         reformat = ''
    #         projection = ''
    #         projection_parameters = ''
    #     if len(projections) > 0:
    #         valid_proj = [] # select reprojection options based on reformatting selection
    #         for i in range(len(projections)):
    #             if 'excludeFormat' in projections[i]:
    #                 exclformats_str = projections[i]['excludeFormat'] 
    #                 exclformats_list = exclformats_str.split(',')
    #             if ('excludeFormat' not in projections[i] or reformat not in exclformats_list) and projections[i]['value'] != 'NO_CHANGE': valid_proj.append(projections[i]['value'])
    #         if len(valid_proj) > 0:
    #             print('These reprojection options are available with your requested format:', valid_proj)
    #             projection = input('If you would like to reproject, copy and paste the reprojection option you would like (make sure to omit quotes), otherwise leave blank.')
    #             # Enter required parameters for UTM North and South
    #             if projection == 'UTM NORTHERN HEMISPHERE' or projection == 'UTM SOUTHERN HEMISPHERE': 
    #                 NZone = input('Please enter a UTM zone (1 to 60 for Northern Hemisphere; -60 to -1 for Southern Hemisphere):')
    #                 projection_parameters = str('NZone:' + NZone)
    #             else: projection_parameters = ''
    #         else: 
    #             print('No reprojection options are supported with your requested format')
    #             projection = ''
    #             projection_parameters = ''
    #     else:
    #         print('No reprojection options are supported with your requested format')
    #         projection = ''
    #         projection_parameters = ''

    ### Select data access configurations
    #Set NSIDC data access base URL
    base_url = 'https://n5eil02u.ecs.nsidc.org/egi/request'

    #Set the request mode to asynchronous if the number of granules is over 100, otherwise synchronous is enabled by default
    if len(granules) > 100:
        request_mode = 'async'
        page_size = 2000
    else: 
        page_size = 100
        request_mode = 'stream'

    #Determine number of orders needed for requests over 2000 granules. 
    page_num = math.ceil(len(granules)/page_size)

    print('There will be', page_num, 'total order(s) processed for our', dataset_id, 'request.')

    ### Create the API endpoint 
    # If polygon file input:
    param_dict = {'short_name': dataset_id, 
                'version': latest_version, 
                'temporal': temporal, 
                'time': time_var, 
                'polygon': polygon,
                'Boundingshape': Boundingshape, 
                'format': reformat, 
                'projection': projection, 
                'projection_parameters': projection_parameters, 
                'Coverage': coverage, 
                'page_size': page_size, 
                'request_mode': request_mode, 
                'agent': agent, 
                'email': email, }

    #Remove blank key-value-pairs
    param_dict = {k: v for k, v in param_dict.items() if v != ''}

    #Convert to string
    param_string = '&'.join("{!s}={!r}".format(k,v) for (k,v) in param_dict.items())
    param_string = param_string.replace("'","")

    #Print API base URL + request parameters
    endpoint_list = [] 
    for i in range(page_num):
        page_val = i + 1
        API_request = api_request = f'{base_url}?{param_string}&page_num={page_val}'
        endpoint_list.append(API_request)

    # print(*endpoint_list, sep = "\n") 


    ### Request data

    # Different access methods depending on request mode:

    if request_mode=='async':
        # Request data service for each page number, and unzip outputs
        for i in range(page_num):
            page_val = i + 1
            print('Order: ', page_val)

        # For all requests other than spatial file upload, use get function
            param_dict['page_num'] = page_val
            request = session.get(base_url, params=param_dict)

            print('Request HTTP response: ', request.status_code)

        # Raise bad request: Loop will stop for bad response code.
            request.raise_for_status()
            # print('Order request URL: ', request.url)
            esir_root = ET.fromstring(request.content)
            # print('Order request response XML content: ', request.content)

        #Look up order ID
            orderlist = []   
            for order in esir_root.findall("./order/"):
                orderlist.append(order.text)
            orderID = orderlist[0]
            print('order ID: ', orderID)

        #Create status URL
            statusURL = base_url + '/' + orderID
            print('status URL: ', statusURL)

        #Find order status
            request_response = session.get(statusURL)    
            print('HTTP response from order response URL: ', request_response.status_code)

        # Raise bad request: Loop will stop for bad response code.
            request_response.raise_for_status()
            request_root = ET.fromstring(request_response.content)
            statuslist = []
            for status in request_root.findall("./requestStatus/"):
                statuslist.append(status.text)
            status = statuslist[0]
            print('Data request ', page_val, ' is submitting...')
            print('Initial request status is ', status)

        #Continue loop while request is still processing
            while status == 'pending' or status == 'processing': 
                print('.', end = '')
                time.sleep(10)
                loop_response = session.get(statusURL)

        # Raise bad request: Loop will stop for bad response code.
                loop_response.raise_for_status()
                loop_root = ET.fromstring(loop_response.content)

        #find status
                statuslist = []
                for status in loop_root.findall("./requestStatus/"):
                    statuslist.append(status.text)
                status = statuslist[0]
                if status == 'pending' or status == 'processing':
                    continue
                print('Retry request status is: ', status)

        #Order can either complete, complete_with_errors, or fail:
        # Provide complete_with_errors error message:
            if status == 'complete_with_errors' or status == 'failed':
                messagelist = []
                for message in loop_root.findall("./processInfo/"):
                    messagelist.append(message.text)
                print('error messages:')
                pprint.pprint(messagelist)

        # Download zipped order if status is complete or complete_with_errors
            if status == 'complete' or status == 'complete_with_errors':
                downloadURL = 'https://n5eil02u.ecs.nsidc.org/esir/' + orderID + '.zip'
                print('Zip download URL: ', downloadURL)
                print('Beginning download of zipped output...')
                zip_response = session.get(downloadURL)
                # Raise bad request: Loop will stop for bad response code.
                zip_response.raise_for_status()
                with zipfile.ZipFile(io.BytesIO(zip_response.content)) as z:
                    z.extractall(outpath)
                print('Data request', page_val, 'is complete.')
            else: print('Request failed.')
                
    else:
        for i in range(page_num):
            page_val = i + 1
            print('Order: ', page_val)
            print('Requesting...')
            request = session.get(base_url, params=param_dict)
            print('HTTP response from order response URL: ', request.status_code)
            request.raise_for_status()
            d = request.headers['content-disposition']
            fname = re.findall('filename=(.+)', d)
            dirname = os.path.join(outpath,fname[0].strip('\"'))
            print('Downloading...')
            open(dirname, 'wb').write(request.content)
            print('Data request', page_val, 'is complete.')
        
        # Unzip outputs
        for z in os.listdir(outpath): 
            if z.endswith('.zip'): 
                zip_name = outpath + "/" + z 
                zip_ref = zipfile.ZipFile(zip_name) 
                zip_ref.extractall(outpath) 
                zip_ref.close() 
                os.remove(zip_name) 

    ### Finally, we will clean up the Output folder by removing individual order folders:
    # Clean up Outputs folder by removing individual granule folders 

    for root, dirs, files in os.walk(outpath, topdown=False):
        for file in files:
            try:
                shutil.move(os.path.join(root, file), outpath)
            except OSError:
                pass
        for name in dirs:
            os.rmdir(os.path.join(root, name))    
    print(f"Completed. Images output to {outpath}")

# if __name__ == "main:":
#     fetch_nsidc(year = 2022, dataset_id = 'MOD10_L2')
#     fetch_nsidc(year = 2022, dataset_id = 'MYD10_L2')
