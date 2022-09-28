# -*- coding: utf-8 -*-
"""
Created on Tue Apr 12 19:35:08 2022

@author: Admin
"""
# https://stackoverflow.com/questions/647515/how-can-i-find-where-python-is-installed-on-windows
#import os, sys
#os.path.dirname(sys.executable)
#sys.version
#python -c "import os, sys; print(os.path.dirname(sys.executable))"
# exec(open('N:/Mi unidad/ideca/scripts/Monitoreo_extract-qgis.py').read())
import qgis
from qgis.analysis import QgsZonalStatistics
from os import path
import glob, os
tifDir = "G:/My Drive/Datos-NASA-ODS15/Talleres/Peru_riparios/04_calculoNacional"
os.chdir(tifDir )
shpDir = "L:/IDECA/svm_vect"
tifs = glob.glob("diff_NDVI_*.tif")
for f in range(len(tifs)):
    tif = tifs[f]
    shp0 = tif.replace('.tif', '').replace('diff_NDVI_', '').lower()
    shp = shpDir + '/xb5m_' + shp0 + '.shp' 
    #specify polygon shapefile vector
    #if not path.exists(out_name):
    rasterFilePath = tifDir + '/' + tif 
    if path.exists(rasterFilePath) and path.exists(shp):
        band=1    
        rLayer = QgsRasterLayer(rasterFilePath ,"my raster")
        vLayer = QgsVectorLayer(shp, 'layer', "ogr") 
        prov = vLayer.dataProvider()
        # specify raster filename
        field_names = [field.name() for field in prov.fields()]
        median_in_names = sum( [i.rfind('MEDIAN') != -1 for i in field_names] )
        print(shp, field_names, median_in_names )
        if median_in_names == 0:
            print(rasterFilePath, shp)
            #if not ( vLayer.isValid() and rLayer.isValid() ): # print "Error loading layers..."
            # usage - QgsZonalStatistics (QgsVectorLayer *polygonLayer, const QString &rasterFile, const QString &attributePrefix="", int rasterBand=1)
            zonalstats = QgsZonalStatistics( vLayer, rLayer, str(""), band, QgsZonalStatistics.Median)
            zonalstats.calculateStatistics( None )
            print(rasterFilePath, shp)

        
2
2
#################################
import qgis
from qgis.analysis import QgsZonalStatistics
from os import path
import glob, os
tifDir = "L:/IDECA/svm_comp"
os.chdir(tifDir )
shpDir = "L:/IDECA/svm_vect"
tifs = glob.glob("diff_NDVI_*.tif")        
tifs

band=1
f = 14
##################################

tif = tifs[f]
tif
shp0 = tif.replace('.tif', '').replace('diff_NDVI_', '').lower()
shp = shpDir + '/xb5m_' + shp0 + '.shp' 
rasterFilePath = tifDir + '/' + tif
rLayer = QgsRasterLayer(rasterFilePath ,"my raster")
vLayer = QgsVectorLayer(shp, 'layer', "ogr") 
prov = vLayer.dataProvider()
field_names = [field.name() for field in prov.fields()]
print(field_names)
print(rasterFilePath, shp)
median_in_names = sum( [i.rfind('_median') != -1 for i in field_names] )
if median_in_names == 0:
	# if '_median' in field_names:
    #for count, f in enumerate(field_names):
    #print(f"{count} {f}")
    zonalstats = QgsZonalStatistics( vLayer, rLayer, str("_"), band, QgsZonalStatistics.Median)
    zonalstats.calculateStatistics( None )


print(rasterFilePath, shp)     
f = f+1

  
    
#layer = qgis.utils.iface.activeLayer()
    