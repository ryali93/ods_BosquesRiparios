## Cargar librería ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- 'F:/ods15/riparios/04_calculoANP' # Usar / o \\
dir.create(root)
setwd( root ) # asignar ruta de trabajo
list.dirs(path = '..', recursive = FALSE) # Asegurarse que exisen las carpetas del proyecto (01_DatosOriginales, etc...)


## Corregir ruta si tiene espacios, para usar en gdal_calc
root2 <- strsplit(root, '/')[[1]]
root2[grep(' ', root2)] <- paste0('"', root2[grep(' ', root2)] ,'"') 
root2 <- paste0(paste0(root2, collapse = '/'), '/')


## Crear carpetas intermedias
outDirs <- c('02_bosques-filtrados', '03_bosques-raster', '04_rios-buffer',
             '05_rios-raster', '06_cruce-bosques-riparios') # Rios se mantienen igual no necesitamos crearlos de nuevo
sapply(outDirs, dir.create, recursive = TRUE) # Algunos warnings. No es grave



## Evaluar si tenemos acceso a gdal_calc ----
## Validar si tenemos la libreria gdal_calc
# execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.26.0"\\OSGeo4W.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\OSGeo4W.bat
#                    'py3_env.bat ', # Mantener espacio. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
#                    '&& py3_env.bat ', # Mantener espacio. Repetir linea anterior. Alternativa en 3.22.6: C:\\"Program Files"\\"QGIS 3.22.6"\\bin\\o4w_env.bat
#                    '&& gdal_calc ') # Llamado de gdal_calc

## Instrucción en QGIS 3.22.6
execGDAL <- paste0('C:\\"Program Files"\\"QGIS 3.26.0"\\OSGeo4W.bat ', 
                   'C:\\"Program Files"\\"QGIS 3.26.0"\\bin\\o4w_env.bat && ', 
                   'C:\\"Program Files"\\"QGIS 3.26.0"\\bin\\o4w_env.bat && gdal_calc')

## Mirar la respuesta del sistema a conexion a gdal_calc
(gdalcalc <- capture.output(system(paste0(execGDAL, ' --help'), intern = TRUE)))

## Asignar TRUE si se encuentra respuesta del sistema
(GDAL <- ifelse(any(grep('gdal_calc.py', gdalcalc)), TRUE, FALSE))



## Bosques -----
## Convertir capa de bosques entre 0 y 1 con bosques a 2000
archivo_bosques <- '../01_DatosOriginales/Nacional/Bosque_No_Bosque_2020.tif'
raster_bosques <- raster(archivo_bosques) # Este no se carga completamente en memoria
(extent_numerico <- raster_bosques@extent[c(1, 3, 2, 4)] )  # Extraemos el extent

## Rasterizar la capa de deforestación
archivo_perdida_vect_orig <- '../08_SERNANP/deforestacion.shp'
archivo_perdida_vect_proj <- '../08_SERNANP/deforestacion_proj.shp'
if( ! file.exists(archivo_perdida_vect_proj)){
  capa_perdida_vect_orig <- readOGR(archivo_perdida_vect_orig) # Se carga completamente en memoria
  capa_perdida_vect_orig_proj <- spTransform(capa_perdida_vect_orig, CRSobj = raster_bosques@crs)
  writeOGR(capa_perdida_vect_orig_proj, '../08_SERNANP', 'deforestacion_proj', driver = 'ESRI Shapefile')
}

# Definimos donde guardar la capa final
archivo_perdida <- '02_bosques-filtrados/Perdida_2001_2022.tif' 

if (!file.exists(archivo_perdida)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_perdida_vect_proj, 
                                  dst_filename = archivo_perdida,
                                  a = 'md_anno',
                                  tr = c(30, 30),  
                                  te =  extent_numerico,
                                  co = c("COMPRESS=DEFLATE"))
  )) # 500
}



## Repetir proceso desde GDAL externo. Más demorado
# system.time(extent_bosques_rgdal <- rgdal::ogrInfo(archivos_bosques[1])) # 5 mins
# extent_bosques <- extent_bosques_rgdal$extent 


archivo_inicial_0 <- '02_bosques-filtrados/Bosque_inicial2000.tif'
if (! file.exists(archivo_inicial_0) ){
  if (!GDAL){
    ## Opcion R
    print(system.time(
      if (! file.exists(archivo_inicial_0) ){
        bosque_inicial2000 <- Which(capa_bosque == 3 | capa_bosque == 5)
        # plot(bosque_inicial2000)
        writeRaster(x = bosque_inicial2000, filename = archivo_inicial_0,
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }
    )) # 4356.74seg # 70mins
    
  } else {
    ## Opcion GDAL
    print(system.time(
      system(
        paste0(execGDAL, 
               '--calc="0 + (1*(logical_or(A==3, A==5)))" ',
                '-A ', root2, '\\', archivo_bosques,
               #'-A C:/temp/Peru_riparios/01_DatosOriginales/Nacional/Bosque_No_Bosque_2020.tif', 
               ' --outfile=', root2, '\\', archivo_inicial_0,
               ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
        ))
      ))
  }
}


## Cambiar datos nullos a 0 en capa raster de perdida de bosque
#archivo_perdida_0 <- '02_bosques-filtrados/Perdida_2001_2022_llenado0.tif'
archivo_perdida_0 <- archivo_perdida
if (! file.exists(archivo_perdida_0) ){
  print(system.time(
    capa_perdida_0 <- gdalUtilities::gdal_translate(a_nodata = 999, # dato que se usa como nodata original 
                                                    src_dataset = archivo_perdida, 
                                                    dst_dataset = archivo_perdida_0
                                                    #ot = 'Byte'
                                                    )))
} 



## Cargar capas raster en R en caso que debamos operar en R los cálculos
raster_inicial_0 <- raster(archivo_inicial_0)
raster_perdida_0 <- raster(archivo_perdida_0)



## Rios ------
Ruta_rios <- '../08_SERNANP/hidro_clip_anp.shp'
file.exists( Ruta_rios )
Ruta_rios_proj <- '../08_SERNANP/hidro_clip_anp_proj.shp'
if( ! file.exists(Ruta_rios_proj)){
  capa_rios_vect_orig <- readOGR(Ruta_rios) # Se carga completamente en memoria
  capa_rios_vect_orig_proj <- spTransform(capa_rios_vect_orig, CRSobj = raster_bosques@crs)
  writeOGR(capa_rios_vect_orig_proj, '../08_SERNANP', 'hidro_clip_anp_proj', driver = 'ESRI Shapefile')
}


## Buffer rios ------
## Crear capa vector con buffers de 30 y 10m
archivo_buffer_30 <-  paste0('04_rios-buffer/', 'buffer30.shp')

if ( ! file.exists(archivo_buffer_30) ){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = Ruta_rios_proj, 
                           dst_datasource_name = archivo_buffer_30,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 30) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(Ruta_rios)) ))
  ) # 200
}


archivo_buffer_100 <-  paste0('04_rios-buffer/', 'buffer100.shp')

if (!file.exists(archivo_buffer_100)){
  system.time(
    gdalUtilities::ogr2ogr(src_datasource_name = Ruta_rios_proj, 
                           dst_datasource_name = archivo_buffer_100,
                           f = "ESRI Shapefile", dialect = 'sqlite',
                           sql = paste0('select ST_buffer(geometry, 100) as geometry FROM ', 
                                        basename(tools::file_path_sans_ext(Ruta_rios)) ))
  ) # 237
}


## Crear capa raster de buffers de 30 y 10m

archivo_raster_buffer_30 <- paste0('05_rios-raster/', 'buffer30.tif')
archivo_raster_buffer_100 <- paste0('05_rios-raster/', 'buffer100.tif')

if (!file.exists(archivo_raster_buffer_30)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_30, 
                                  dst_filename = archivo_raster_buffer_30,
                                  ot = 'Byte',
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico,
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 400seg
}

if (!file.exists(archivo_raster_buffer_100)){
  print(system.time(
    gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                  dst_filename = archivo_raster_buffer_100, ot = 'Byte',
                                  burn = 1, tr = c(30, 30), init = 0, 
                                  te =  extent_numerico,
                                  co=c("COMPRESS=DEFLATE", "NBITS=1"))
  )) # 200seg
}


## Extraer estadísticas ----
system.time( estadisticas_buffer_30m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_30, stats = TRUE, checksum = TRUE) ) )

dimensiones_buff_30 <- as.numeric(strsplit(gsub('[[:alpha:]]| ','', 
                            grep('Size is', estadisticas_buffer_30m, value = TRUE) ),
                            ",")[[1]])
celdas_buff_30 <- Reduce(f = '*', x = dimensiones_buff_30)
(promedio_buff_30 <- as.numeric(gsub(  " |STATISTICS_MEAN=", "",
                         grep('STATISTICS_MEAN', 
                              estadisticas_buffer_30m, value = TRUE)) ) )
(pixeles_zonas_riparias_30 <- celdas_buff_30 * promedio_buff_30)


system.time( estadisticas_buffer_100m <- capture.output(
  gdalUtilities::gdalinfo(archivo_raster_buffer_100, stats = TRUE, checksum = TRUE) ))

dimensiones_buff_100 <- as.numeric(
  strsplit(gsub('[[:alpha:]]| ','', 
                grep('Size is', estadisticas_buffer_100m, value = TRUE) ),
                                           ",")[[1]])

celdas_buff_100 <- Reduce(f = '*', x = dimensiones_buff_100)
(promedio_buff_100 <- as.numeric(
  gsub(  " |STATISTICS_MEAN=", "",
         grep('STATISTICS_MEAN',
              estadisticas_buffer_100m, value = TRUE)) ) )

(pixeles_zonas_riparias_100 <- celdas_buff_100 * promedio_buff_100)


## calcular esa estadística desde R interno. Se demora mucho mas
# raster_buffer_30 <- raster::raster(archivo_raster_buffer_30)
# system.time(raster::cellStats(raster_buffer_30, sum, na.rm = TRUE) )


# raster_buffer_100 <- raster(archivo_raster_buffer_100)
# raster::cellStats(raster_buffer_100, sum, na.rm = TRUE) # 53383714
# 0.10177180739745 * 21908 * 23943


## Extraer fechas de perdida de bosque sobre la cual iterar. Dejaremos desde 2000 a 2020
raster_fechas <- raster(archivo_perdida)
#fechas_unicas <- unique(raster_fechas) # 400 seg
system.time(fechas_perdida <- unique(raster_fechas[]))
fechas_perdida <- sort(na.omit(fechas_perdida))

fechas_unicas <- 2000+(0:22)


## Iterar sobre los bosques -----
## Las siguientes intrucciones se repetiran para cada año entre 2000 y 2020
for( f in 1:length(fechas_unicas)){ # f = 19 # }
  
  ## Definir valores en cada paso, como anio y nombres de capas
  (anio <- fechas_unicas[f]) ## entre 2000 y 2020
  (anio2 <- fechas_unicas[f]) ## entre 0 y 20
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas),
        ' -- anio ', anio ) )
  (nombre_raster_bosque <- paste0('03_bosques-raster', '/bosqueFecha_', anio, '.tif' ))
  
  
  ## Crear capa de bosque por anio reclasificando valores de capa de bosque de perdida
  if (!file.exists(nombre_raster_bosque)){
    if (! GDAL){
      ## Hecho en R
      raster_j <- raster_inicial_0 - raster::Which(raster_perdida_0 < anio2)
      writeRaster(x = raster_j, filename = nombre_raster_bosque,
                  datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      
    } else {
      ## Heho con gdal_calc.pys
      print(system.time( 
        system(intern = TRUE,
          paste0('C:\\"Program Files"\\"QGIS 3.16"\\OSGeo4W.bat py3_env.bat && ',
                 'py3_env.bat && gdal_calc ',
                 '--calc="A-(1*(logical_and(B!=0,B<=', anio2, ')))" ',
                 '-A ', root2, '\\', archivo_inicial_0,' -B ', root2, '\\',archivo_perdida_0, 
                 ' --outfile=', root2, '\\', nombre_raster_bosque,
                 ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
                 # 
          ) 
        )
      ))
      
    }
    
    print ( paste('   -- Guardado fecha ', anio ) )
  }
  
  ## Intersectar con zona buffer --------
  ## Cruzar capas de bosques por anio con las areas de zonas buffer
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Intersectar bosques y zonas riberenas ',anio) )   
  
  # Bosque en buffers 30m -----
  (bosque_buffer_30 <- paste0( '06_cruce-bosques-riparios/', 
                               'bosque_ripario30m_', anio , '.tif'))
  
  #raster_bosque <- raster(bosque_rasterizado)
  stack_capas_30m <- raster::stack(nombre_raster_bosque, archivo_raster_buffer_30)
  
  if (!file.exists(bosque_buffer_30)){
    
    if (!GDAL){
      
      ## Opcion R
      print(system.time({ # 50 mins
        bosques_riparios_30 <- Which(stack_capas_30m[[1]] != 0 &  stack_capas_30m[[2]] <= 1);
        writeRaster(bosques_riparios_30, bosque_buffer_30, overwrite = TRUE, 
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
      }))
      
    } else {
      
      ## Opcion GDAL
      print(system.time(
        system(intern = TRUE,
          paste0(execGDAL,
                 ' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                 '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',archivo_raster_buffer_30,
                 ' --outfile=', root2, '\\', bosque_buffer_30,
                 ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
          ))))
    }
  }
  
  
  # Bosque en buffers 100m -----
  (bosque_buffer_100 <- paste0( '06_cruce-bosques-riparios/',
                                'bosque_ripario100m_', anio , '.tif'))
  
  stack_capas_100m <- raster::stack(nombre_raster_bosque, archivo_raster_buffer_100)
  if (!file.exists(bosque_buffer_100)){
    
    if (!GDAL){
      ## Opcion R
      print(system.time({ # 50 mins
        bosques_riparios_100 <- Which(stack_capas_100m[[1]] != 0 &  stack_capas_100m[[2]] <= 1);
        writeRaster(bosques_riparios_100, bosque_buffer_100, overwrite = TRUE,
                    datatype = 'Byte', options=c("COMPRESS=DEFLATE", "NBITS=1"))
        
      }))
      
    } else {
      # Opcion GDAL
      print(system.time(
        system(intern = TRUE,
          paste0(execGDAL, 
                 ' --calc="0 + (1*(logical_and(A==1,B==1)))" ',
                 '-A ', root2, '\\', nombre_raster_bosque,' -B ', root2, '\\',archivo_raster_buffer_100,
                 ' --outfile=', root2, '\\', bosque_buffer_100,
                 ' --type=Byte --NoDataValue=999 --co="NBITS=1" --co="COMPRESS=DEFLATE" --quiet'
          ))))
    }
  }
  
  
  ## Estadisticas ---
  ## Extraer estadisticas usando las funcionalidad de GDAL. Mucho mas rapido que R interno
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- Estadisticas ',anio) )   
  archivo_estadisticas <- paste0( '06_cruce-bosques-riparios/', 'estadisticas_bosque_riparios', anio , '.csv')
  
  ## Calcular estadisticas si el archivo CSV no existe
  if (!file.exists(archivo_estadisticas)){
    
    ## Estadisticas para 30m -----
    (estadisticas_ripario_30 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_30, stats = TRUE, checksum = TRUE)))
    
    dimensiones_ripario_30 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_buffer_30m,
                         value = TRUE) ), ",")[[1]])
    
    celdas_ripario_30 <- Reduce(f = '*', x = dimensiones_ripario_30)
    
    (promedio_ripario_30 <- as.numeric(gsub(" |STATISTICS_MEAN=", "",
                                            grep('STATISTICS_MEAN', 
                                                 estadisticas_ripario_30, 
                                                 value = TRUE)) ) )
    (pixeles_bosques_riparios_30 <- celdas_ripario_30 * promedio_ripario_30)
    
    
    
    ## Estadisticas 100m ------
    (estadisticas_ripario_100 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_100, stats = TRUE, checksum = TRUE) ))
    
    dimensiones_ripario_100 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_buffer_100m,
                         value = TRUE) ), ",")[[1]])
    
    celdas_ripario_100 <- Reduce(f = '*', x = dimensiones_ripario_100)
    
    (promedio_ripario_100 <- as.numeric(gsub(" |STATISTICS_MEAN=", "",
                                             grep('STATISTICS_MEAN', 
                                                  estadisticas_ripario_100, 
                                                  value = TRUE)) ) )
    (pixeles_bosques_riparios_100 <- celdas_ripario_100 * promedio_ripario_100)
    
    
    ## Crear tabla final y escribirla
    tabla_resumen <- data.frame(fecha = anio,
                                pix_rip30m = pixeles_zonas_riparias_30,
                                pix_bosrip30m = pixeles_bosques_riparios_30,
                                propbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30,
                                pix_rip10m = pixeles_zonas_riparias_100,
                                pix_bosrip100m = pixeles_bosques_riparios_100,
                                propbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100
    )
    
    #write.csv(tabla_resumen, file = archivo_estadisticas, row.names = FALSE) # Escribir separado por comas
    write.csv2(tabla_resumen, file = archivo_estadisticas, row.names = FALSE) # Escribir separado por punto y comas
  }
  
  print(paste(' ---- Capa ', f, ' de ', length(fechas_unicas), ' -- extraer estadisticas ',anio) )   
}




## Compilar datos de csv -----
archivos_csv <- list.files(pattern = '[0-9].csv', 
                           path = '06_cruce-bosques-riparios', 
                           full.names = TRUE)
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  tabla_compilada <- rbind(tabla_compilada , read.csv2(archivos_csv[i]) )
}

write.csv(x = tabla_compilada, row.names = FALSE,
          file = paste0('06_cruce-bosques-riparios/',
          'Consolidado_estadisticas_bosque_riparios_compiladas_',
          Sys.Date(), '.csv'))


