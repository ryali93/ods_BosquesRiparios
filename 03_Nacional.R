## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- 'C:/temp/Peru_riparios/04_calculoNacional/' # Usar / o \\. Cada uno cambia su ruta
setwd( root ) # asignar ruta de trabajo

list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

outDirs <- c('02_bosques-filtrados', '03_bosques-raster', '04_rios-buffer',
             '05_rios-raster', '06_cruce-bosques-riparios')
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave


## Bosques -----
archivo_bosques <- '../01_DatosOriginales/Nacional/Bosque_No_Bosque_2020.tif'
archivo_perdida <- '../01_DatosOriginales/Nacional/Perdida_2001_2020.tif'
(extent_numerico <- info_bosques@extent[c(1, 3, 2, 4)] ) 

capa_bosque <- raster(archivo_bosques) 
capa_bosque

archivo_perdida_0 <- '02_bosques-filtrados/Bosque_inicial2000.tif'
if (! file.exists(archivo_perdida_0) ){
  bosque_inicial2000 <- Which(capa_bosque == 3 | capa_bosque == 5)
  # plot(bosque_inicial2000)
  writeRaster(x = bosque_inicial2000, filename = archivo_perdida_0)
}


archivo_perdida_0 <- '02_bosques-filtrados/Perdida_2001_2020_llenado0.tif'
if (! file.exists(archivo_perdida_0) ){
  system.time(
    capa_perdida_0 <- gdalUtilities::gdal_translate(a_nodata = 9999,
      stats = TRUE, 
      src_dataset = archivo_perdida, dst_dataset = archivo_perdida_0,
      ot = 'Byte'))
}

# system.time(extent_bosques_rgdal <- rgdal::ogrInfo(archivos_bosques[1])) # 5 mins
# extent_bosques <- extent_bosques_rgdal$extent

## Rios ------
Ruta_rios <- '../01_DatosOriginales/Nacional/Riv_100000_350k.shp'
file.exists( Ruta_rios )

## Buffer rios ------
#archivo_buffer_30 <-  paste0('C:/temp/', 'buffer30.shp')
archivo_buffer_30 <-  paste0('04_rios-buffer/', 'buffer30.shp')

if ( ! file.exists(archivo_buffer_30) ){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = Ruta_rios, 
                           dst_datasource_name = archivo_buffer_30,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 30) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(Ruta_rios)) ))
  ) # 200
}


archivo_buffer_100 <-  paste0('04_rios-buffer/', 'buffer100.shp')

if (!file.exists(archivo_buffer_100)){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = Ruta_rios, 
                           dst_datasource_name = archivo_buffer_100,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 100) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(Ruta_rios)) ))
  ) # 237
}

archivo_raster_buffer_30 <- paste0('05_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('05_rios-raster/', 'buffer100.tif')

if (!file.exists(archivo_raster_buffer_30)){
  system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_30, 
                                  dst_filename = archivo_raster_buffer_30,
                                  ot = 'Byte',
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico)
  ) # 60
}

if (!file.exists(archivo_raster_buffer_100)){
  system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                  dst_filename = archivo_raster_buffer_100, ot = 'Byte',
                                  burn = 1, tr = c(30, 30), init = 0, te =  extent_numerico)
  ) # 60
}

## Extraer estadísticas ----
estadisticas_buffer_30m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_30, stats = TRUE, checksum = TRUE) )

dimensiones_buff_30 <- as.numeric(strsplit(gsub('[[:alpha:]]| ','', 
                            grep('Size is', estadisticas_buffer_30m, value = TRUE) ),
                            ",")[[1]])
celdas_buff_30 <- Reduce(f = '*', x = dimensiones_buff_30)
(promedio_buff_30 <- as.numeric(gsub(  "[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                         grep('STATISTICS_MEAN', 
                              estadisticas_buffer_30m, value = TRUE)) ) )
(pixeles_zonas_riparias_30 <- celdas_buff_30 * promedio_buff_30)


estadisticas_buffer_100m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_100, stats = TRUE, checksum = TRUE) )

dimensiones_buff_100 <- as.numeric(
  strsplit(gsub('[[:alpha:]]| ','', 
                grep('Size is', estadisticas_buffer_100m, value = TRUE) ),
                                           ",")[[1]])

celdas_buff_100 <- Reduce(f = '*', x = dimensiones_buff_100)
(promedio_buff_100 <- as.numeric(
  gsub(  "[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
         grep('STATISTICS_MEAN', 
              estadisticas_buffer_100m, value = TRUE)) ) )

(pixeles_zonas_riparias_100 <- celdas_buff_100 * promedio_buff_100)



# raster_buffer_30 <- raster::raster(archivo_raster_buffer_30)
# system.time(raster::cellStats(raster_buffer_30, sum, na.rm = TRUE) )


# raster_buffer_100 <- raster(archivo_raster_buffer_100)
# raster::cellStats(raster_buffer_100, sum, na.rm = TRUE) # 53383714
# 0.10177180739745 * 21908 * 23943


raster_fechas <- raster(archivo_perdida)
# fechas_unicas <- unique(raster_fechas) # 400 seg
fechas_unicas <- 1:20



fechas_perdida <- unique(capa_perdida[])
fechas_perdida <- sort(na.omit(fechas_perdida))

## Iterar sobre los bosques -----


for( f in 1:length(archivos_bosques)){ # f = 1 # }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
        ' -- ', basename(archivos_bosques[f]) ) )
  
  03_bosques-raster
  
  # for( j in fechas_perdida){ # j = 10
  #   nombre_raster <- paste0(cuenca_dir, '/bosqueFecha_', 2000+j, '.tif' )
  #   if (!file.exists(nombre_raster)){
  #     raster_j <- bosque_inicial2000 - raster::Which(capa_perdida < j)
  #     # plot(raster_j)
  #     writeRaster(x = raster_j, filename = nombre_raster)
  #     print ( paste('  ID: ', id_cuenca, '  -- Guardado fecha ', j ) )
  #   }
    
  
  ## Bosques filtrar polígono -----
  (bosque_archivo <- gsub('v_ff010_cobertura_vegetal_', 'BOSQUE_',
                          gsub('_aPolygon', '', basename(archivos_bosques[f]) ) ))
  (bosque_archivo_ruta <- paste0('02_bosques-filtrados/', bosque_archivo ) )
  (anio <- gsub('\\.|[[:alpha:]]|[[:punct:]]', '', bosque_archivo))
  
  #(cmd <- paste0('ogr2ogr -where "\"cobertura_\" = \"BOSQUE\" AND \"cobertura_\" = \"BOSQUE NATIVO\"" -f "ESRI Shapefile" ',outshp, ' ', forests[f]))
  
  print(bosque_archivo_ruta)
  if ( !file.exists(bosque_archivo_ruta) ) {
    
    tabla_dbf_boque <- read.dbf(gsub('.shp', '.dbf', archivos_bosques[f]), as.is = TRUE)
    # head(tabla_dbf_boque)
    valores_unicos <- unique(tabla_dbf_boque$cobertura_)
    (valores_bosque <- grep('BOS', valores_unicos, value = TRUE))
    
    gdalUtilities::ogr2ogr(src_datasource_name = archivos_bosques[f], 
                           dst_datasource_name = bosque_archivo_ruta, f = "ESRI Shapefile",
                           #where = "cobertura_ = 'BOSQUE' AND cobertura_ = 'BOSQUE NATIVO'"
                           #where = "cobertura_ LIKE 'BOSQUE' AND cobertura_ LIKE 'BOSQUE NATIVO'"
                           where = paste0("cobertura_ LIKE '", valores_bosque,"'")
    )
  }
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- filtro de bosques ',anio) ) 
  
  
  ## Bosques rasterizar -----
  (bosque_rasterizado <- paste0( '03_bosques-raster/', #  
                                 basename(tools::file_path_sans_ext(bosque_archivo_ruta)), '.tif'))
  if ( !file.exists(bosque_rasterizado) ) {
    
    # 5 min
    system.time(
      gdalUtilities::gdal_rasterize(src_datasource = bosque_archivo_ruta, 
                                    dst_filename = bosque_rasterizado, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    te =  extent_numerico)
    )
  }
  
  # bos_rast_r <- raster(bosque_rasterizado)
  # plot(bos_rast_r)
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- rasterizar bosques ',anio) )   
  
  # Bosque en buffers 30m -----
  (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 
                               'bosque_ripario30m_', anio , '.tif'))
  
  (bosque_buffer_30_01 <- paste0( '06_cruce-bosques-riparios/',
                                  'bosque_ripario30m_', anio , '_01.tif'))
  
  #raster_bosque <- raster(bosque_rasterizado)
  stack_capas_30m <- raster::stack(bosque_rasterizado, archivo_raster_buffer_30)
  gdalUtils::gdal_cmd_builder()
  gdalUtilities::
  file.exists(archivo_raster_buffer_30)
  file.exists(bosque_rasterizado)
  
  if (!file.exists(bosque_buffer_30)){
    system.time( # 50 mins
      bosques_riparios_30 <- calc(x = stack_capas_30m, 
                                  forcefun = TRUE, 
                                  fun  = function(stk){
                                    stk[[1]] & stk[[2]]}, 
                                  filename = bosque_buffer_30, 
                                  datatype = 'Byte'))
  }
  
  ## Crear un archivo con 0 y 1 --------
  if (!file.exists(bosque_buffer_30_01)){
    system.time(
      bosques_riparios_30_01 <- gdalUtilities::gdal_translate(
        stats = TRUE,
        src_dataset = bosque_buffer_30, dst_dataset = bosque_buffer_30_01,
        ot = 'Byte'))
  }
  
  #raster_bosque30m01 <- raster(bosque_buffer_30_01); plot(raster_bosque30m01)
  
  
  # Bosque en buffers 100m -----
  (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/',
                               'bosque_ripario100m_', anio , '.tif'))
  
  (bosque_buffer_100_01 <- paste0( '06_cruce-bosques-riparios/', 
                                  'bosque_ripario100m_', anio , '_01.tif'))
  
  #raster_bosque <- raster(bosque_rasterizado)
  stack_capas_100m <- raster::stack(bosque_rasterizado, archivo_raster_buffer_100)
  if (!file.exists(bosque_buffer_100)){
    system.time( # 50 mins
      bosques_riparios_100 <- calc(x = stack_capas_100m, 
                                  forcefun = TRUE, fun  = function(stk){
                                    stk[[1]] & stk[[2]]}, 
                                  filename=bosque_buffer_100, 
                                  datatype = 'LOG1S'))
  }
  
  ## Crear un archivo con 0 y 1 --------
  if (! file.exists(bosque_buffer_100_01) ){
    system.time(
      bosques_riparios_100_01 <- gdalUtilities::gdal_translate(
        stats = TRUE,
        src_dataset = bosque_buffer_100, dst_dataset = bosque_buffer_100_01,
        ot = 'Byte'))
  }
  
  
  #raster_bosque01 <- raster(bosque_rasterizado)
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- Intersectar bosques y zonas riberenas ',anio) )   
  
  ## Estadisticas 30m---
  estadisticas_ripario_30 <- capture.output(
    gdalUtilities::gdalinfo(bosque_buffer_30_01, stats = TRUE, checksum = TRUE) )
  
  dimensiones_ripario_30 <- as.numeric(
    strsplit(gsub('[[:alpha:]]| ','', 
                  grep('Size is', estadisticas_buffer_30m,
                       value = TRUE) ), ",")[[1]])
  
  celdas_ripario_30 <- Reduce(f = '*', x = dimensiones_ripario_30)
  
  (promedio_ripario_30 <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                                          grep('STATISTICS_MEAN', 
                                               estadisticas_ripario_30, 
                                               value = TRUE)) ) )
  (pixeles_bosques_riparios_30 <- celdas_ripario_30 * promedio_ripario_30)
  
  
  
  ## Estadisticas 100m ------
  
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
  
  
  
  tabla_resumen <- data.frame(fecha = anio,
                              pix_rip30m = pixeles_zonas_riparias_30,
                              pix_bosrip30m = pixeles_bosques_riparios_30,
                              propbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30,
                              pix_rip10m = pixeles_zonas_riparias_100,
                              pix_bosrip100m = pixeles_bosques_riparios_100,
                              propbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100
                              )
  write.csv(tabla_resumen, file = paste0( '06_cruce-bosques-riparios/', 
                                   'estadisticas_bosque_riparios', anio , '.csv'))
  
  print(paste(' ---- Capa ', f, ' de ', length(archivos_bosques),
              ' -- extraer estadisticas ',anio) )   
}

## Compilar datos de csv -----
archivos_csv <- list.files(pattern = '[0-9].csv', 
                           path = '06_cruce-bosques-riparios', 
                           full.names = TRUE)
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  tabla_compilada <- rbind(tabla_compilada , read.csv(archivos_csv[i]))
}

write.csv(tabla_compilada, 
          file = '06_cruce-bosques-riparios/estadisticas_bosque_riparios_compiladas.csv')


