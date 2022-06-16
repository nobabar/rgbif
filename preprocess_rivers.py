'''
preprocess river data with qgis
either use directly in qgis console
or use in python (commented part, not implemented yet)
'''

import processing
import glob
from pathlib import Path

# extract layer from raster


def calculator(in_file, out_file, expression):
    processing.run('qgis:rastercalculator', {'INPUT': in_file,
                                             'CELLSIZE': 100,
                                             'EXPRESSION': expression,
                                             # from qgis.core
                                             'CRS': QgsCoordinateReferenceSystem('EPSG:3035'),
                                             'EXTENT': '3240269.854700000,3909569.854700000,2408574.042600000,2944074.042600000 [EPSG:3035]',
                                             'OUTPUT': out_file})


# compute layer statistics
def zonal_stat(in_file, zone_file, out_file):
    processing.run('qgis:rasterlayerzonalstats', {'INPUT': in_file,
                                                  'BAND': 1,
                                                  'ZONES': zone_file,
                                                  'ZONES_BAND': 1,
                                                  'OUTPUT_TABLE': out_file})


'''
from qgis.core import QgsApplication, QgsVectorLayer, QgsCoordinateReferenceSystem
from qgis import processing

# Supply path to qgis install location
QgsApplication.setPrefixPath("D:/Software/QGIS/apps/qgis-ltr", True)

# Create a reference to the QgsApplication.  Setting the
# second argument to False disables the GUI.
qgs = QgsApplication([], False)

# Load providers
qgs.initQgis()
'''

# Write your code here to load some layers, use processing
# algorithms, etc.
year = 2012

# list rivers shapefiles
shapefiles = [Path(file).as_posix() for file in glob.glob(
    f"./data/rivers/riparian_zones/{year}/*/*/*.shp")]

# convert to vector layers
for shapefile in shapefiles:
    outfile = shapefile.replace('.shp', '.tif')
    # vlayer = QgsVectorLayer(shapefile, "", "ogr")
    processing.run('gdal:rasterize', {"INPUT": shapefile,
                                      "FIELD": "CODE_1_12",
                                      "UNITS": 1,
                                      "WIDTH": 100.0,
                                      "HEIGHT": 100.0,
                                      "DATA_TYPE": 1,
                                      "OUTPUT": outfile})

# list raster layers files
tif_files = [Path(file).as_posix() for file in glob.glob(
    f"./data/rivers/riparian_zones/{year}/*/*/*.tif")]

# merge raster layers
# outfile = f"./data/rivers/riparian_zones/{year}/riparian_zones.tif"
# processing.run('gdal:merge', {"INPUT": tif_files,
#                               "DATA_TYPE": 1,
#                               "OUTPUT": outfile})

interest_rivers = ["rpz_DU017A", "rpz_DU019A"]

interest_tif = [x for x in tif_files if x.split(
    '/')[-1].split('.')[0] in interest_rivers]

# extract three layers and compute their stats
for i, tif in enumerate(interest_tif):
    zone_file = "./data/BioClim/2000/bio1.tif"
    out_urban = tif.replace('.tif', '_urban.tif')
    calculator(tif, out_urban, f'"{interest_rivers[i]}@1" = 1')
    zonal_stat(out_urban, zone_file, out_urban.replace('.tif', '_stats.gpkg'))
    out_crop = tif.replace('.tif', '_crop.tif')
    calculator(tif, out_crop, f'"{interest_rivers[i]}@1" = 2')
    zonal_stat(out_crop, zone_file, out_crop.replace('.tif', '_stats.gpkg'))
    out_land = tif.replace('.tif', '_land.tif')
    calculator(tif, out_land, f'"{interest_rivers[i]}@1" >= 3')
    zonal_stat(out_land, zone_file, out_land.replace('.tif', '_stats.gpkg'))


'''
# Finally, exitQgis() is called to remove the
# provider and layer registries from memory
qgs.exitQgis()
'''
