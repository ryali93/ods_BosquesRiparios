# -*- coding: utf-8 -*-
"""
Created on Wed May 18 14:04:22 2022

@author: IG
"""

from os import path
import glob, os
    
tifDir = "G:/My Drive/Datos-NASA-ODS15/Talleres/Peru_riparios/04_calculoNacional"
os.chdir(tifDir )
tifsBosques = glob.glob("**/*bosqueFecha*.tif")

for f in range(len(tifs)): # f = 1
    tif = tifsBosques[f]
    cuenca_id = tif.replace('cuenca_', '').replace('\\*', '')
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
