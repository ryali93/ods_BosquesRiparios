## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- "F:/ods15/riparios" # Usar / o \\
setwd( root ) # asignar ruta de trabajo

list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

outDir <- '04_calculoNacional'
dir.create(outDir, recursive = TRUE)


## Bosques -----
Ruta_bosques <- '01_DatosOriginales/Nacional/Bosque_No_Bosque_2020.tif'
Ruta_perdida <- '01_DatosOriginales/Nacional/Perdida_2001_2020.tif'

file.exists( Ruta_bosques )
file.exists( Ruta_perdida )

## Rios
Ruta_rios <- '01_DatosOriginales/Nacional/Riv_100000_350k_inters_BasinUH_231.shp'
file.exists( Ruta_rios )


## Cargar cuenca
Ruta_cuencas <- '01_DatosOriginales/Nacional/Bas_PE_UH_231.shp'
file.exists( Ruta_cuencas )

cuencas <- readOGR(Ruta_cuencas, encoding = 'utf8')

cuencas@proj4string@projargs

head(cuencas@data, 10) # Revisar tabla de atributos
tail(cuencas@data, 10) # Revisar tabla de atributos

plot(cuencas, axes = TRUE, main = 'Cuencas') # Graficar sencillo la capa

length(cuencas$ID) # Validar cantidad de objetos
nrow(cuencas) # Validar cantidad de objetos
length( unique(cuencas$ID) ) # Validar número de valores únicos en campo ID

cuencasFull <- cuencas

which(cuencasFull$ID == 159)

cuencas <- cuencasFull[which(cuencasFull$ID == 159),]
cuencas <- cuencasFull
plot(cuencas, add = TRUE, col = 'red', border = 'blue')
plot(cuencasFull[which(cuencasFull$ID == 159),], add = TRUE, 
     col = 'blue', border = 'green')


for (i in 1:nrow(cuencas)){ # }
  # i = which(cuencasFull$ID == 159)
  (id_cuenca <- cuencas$ID[i])
  
  print( paste0(' --- Cuenca ', i, '-', nrow(cuencas),  '  ID: ', id_cuenca))
  
  (cuenca_dir <- paste0(outDir, '/cuenca_ID', id_cuenca))
  dir.create(cuenca_dir)
  
  
  ## Cortar bosques ----
  archivo_bosques_cuenca <- paste0(cuenca_dir, '/bosque_ID', id_cuenca, '.tif')
  if (!file.exists(archivo_bosques_cuenca)){
    corte_bosque <- tryCatch(
      gdalUtilities::gdalwarp(cwhere = paste0('"ID"=', as.numeric(id_cuenca)),
                              srcfile = Ruta_bosques,
                              dstfile = archivo_bosques_cuenca,
                              cutline = Ruta_cuencas,
                              dstnodata = 999,
                              crop_to_cutline = TRUE,
                              overwrite = TRUE), 
      error = function(e) NULL)
    if( is.null(corte_bosque)){
      print(' Cuenca sin datos. Omitir siguientes pasos')
      next
    }
  }
  
  
  ## Extraer cuenca ----
  cuenca_poligono <- cuencas[i, ]
  # plot(cuenca_poligono, add = TRUE, col = 3)
  cuenca_extent <- c(cuenca_poligono@bbox[c(1, 2, 3, 4)])
  archivo_cuenca <- paste0(cuenca_dir, '/cuenca_ID', id_cuenca, '.shp')
  if (!file.exists(archivo_cuenca)){
    writeOGR(cuenca_poligono, cuenca_dir, paste0('cuenca_ID', id_cuenca),
             driver = 'ESRI Shapefile')
  }
  
  
  
  
  
  ## Cortar ríos ---
  archivo_rios_cuenca <- paste0(cuenca_dir, '/rios_ID', id_cuenca, '.shp')
  if (!file.exists(archivo_rios_cuenca)){
    gdalUtilities::ogr2ogr(src_datasource_name = paste0(root, '/', Ruta_rios), 
                           dst_datasource_name = paste0(root, '/', archivo_rios_cuenca),
                           where = paste0('"ID"=', as.numeric(id_cuenca)))
  }
  # plot(readOGR(archivo_rios_cuenca)) 
  # plot(cuenca_poligono, add = TRUE)
  
  ## Buffer rios ----
  archivo_buffer_30 <-  paste0(cuenca_dir, '/buffer30_rios_ID', id_cuenca, '.shp')
  if (!file.exists(archivo_buffer_30)){
    gdalUtilities::ogr2ogr(src_datasource_name = paste0(root, '/', archivo_rios_cuenca), 
                           dst_datasource_name = paste0(root, '/', archivo_buffer_30),
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 30) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_cuenca)) ))
  }
  
  archivo_buffer_100 <-  paste0(cuenca_dir, '/buffer100_rios_ID', id_cuenca, '.shp')
  if (!file.exists(archivo_buffer_100)){
    gdalUtilities::ogr2ogr(src_datasource_name = paste0(root, '/', archivo_rios_cuenca), 
                           dst_datasource_name = paste0(root, '/', archivo_buffer_100),
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 100) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(archivo_rios_cuenca)), ''))
  }
  
  ## Rasterizar rios
  tif_bosque <- raster(archivo_bosques_cuenca)
  extent_bosque <- tif_bosque@extent[c(1, 3, 2, 4)]
  
  (rio_rasterizado_30 <-  paste0(cuenca_dir, '/buffer30_ID', id_cuenca, '.tif'))
  if (  ! file.exists(rio_rasterizado_30) ) {
    system.time(
      gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_30, 
                                    dst_filename = rio_rasterizado_30, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    te =  extent_bosque)
      #<xmin> <ymin> <xmax> <ymax>
    )
  }
  
  
  (rio_rasterizado_100 <-  paste0(cuenca_dir, '/buffer100_ID', id_cuenca, '.tif'))
  if ( !file.exists(rio_rasterizado_30) ) {
    system.time(
      gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                    dst_filename = rio_rasterizado_100, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    te =  extent_bosque)
    )
  }
  
  
  ## Cortar perdida ----
  archivo_perdida_cuenca <- paste0(cuenca_dir, '/perdida_ID', id_cuenca, '.tif')
  if (!file.exists(archivo_perdida_cuenca)){
    
    gdalUtilities::gdalwarp(srcfile = Ruta_perdida,
                            dstfile = archivo_perdida_cuenca,
                            cutline = archivo_cuenca,
                            dstnodata = 999,
                            crop_to_cutline = TRUE,
                            overwrite = TRUE)
  }
  
  capa_bosque <- raster(archivo_bosques_cuenca)
  capa_perdida <- raster(archivo_perdida_cuenca)
  
  raster_2000 <- paste0(cuenca_dir, '/bosqueFecha_', 2000, '.tif' )
  if (!file.exists(raster_2000)){
    bosque_inicial2000 <- Which(capa_bosque == 3 | capa_bosque == 5)
    # plot(bosque_inicial2000)
    writeRaster(x = bosque_inicial2000, filename = raster_2000)
  } else {
    bosque_inicial2000 <- raster(raster_2000)
  }
  
  # fechas_perdida <- unique(capa_perdida[])
  # fechas_perdida <- sort(na.omit(fechas_perdida))
  fechas_perdida <- 1:20
  
  for( j in fechas_perdida){ # j = 10
    nombre_raster <- paste0(cuenca_dir, '/bosqueFecha_', 2000+j, '.tif' )
    if (!file.exists(nombre_raster)){
      raster_j <- bosque_inicial2000 - raster::Which(capa_perdida <= j)
      # plot(raster_j)
      writeRaster(x = raster_j, filename = nombre_raster)
      print ( paste('  ID: ', id_cuenca, '  -- Guardado fecha ', j ) )
    }
  }
  
  
  estadisticas_ripario_100 <- capture.output(
    gdalUtilities::gdalinfo(bosque_buffer_100_01, stats = TRUE, checksum = TRUE) )
  
  dimensiones_ripario_100 <- as.numeric(
    strsplit(gsub('[[:alpha:]]| ','', 
                  grep('Size is', estadisticas_buffer_100m,
                       value = TRUE) ), ",")[[1]])
  
  celdas_ripario_100 <- Reduce(f = '*', x = dimensiones_ripario_100)
  
  (promedio_ripario_100 <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                                           grep('STATISTICS_MEAN', 
                                                estadisticas_ripario_100, 
                                                value = TRUE)) ) )
  (pixeles_bosques_riparios_100 <- celdas_ripario_100 * promedio_ripario_100)
  
  
  # ><
}
