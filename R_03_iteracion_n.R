## Cargar libreria ---

library(raster) # Raster
library(rgdal)  # Funciones de GDAL / OGR para R
library(sf)
library(foreign) # Cargar las tablas .dbf de shapefiles
library(gdalUtilities) # Conexion a suit GDAL/OGR para ejecutar externo 


## Definir Ruta de trabajo ----
root <- 'F:/ods15/riparios' # Usar / o \\
setwd( root ) # asignar ruta de trabajo

list.dirs(path = '.', full.names = TRUE, recursive = FALSE)

outDir <- '04_calculoNacional_cuencas'
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
Ruta_cuencas <- '01_DatosOriginales/Nacional/Bas_PE_UH_231_n.shp'
file.exists( Ruta_cuencas )

cuencas <- readOGR(Ruta_cuencas, encoding = 'utf8')
head(cuencas@data)

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
# cuencas <- cuencasFull ## Descomentar esta linea para correr todas las cuencas

plot(cuencasFull, axes = TRUE, main = 'Cuencas') # Graficar sencillo la capa
plot(cuencas, add = TRUE, col = 'red', border = 'blue')

##################################################################################################
calculo_por_cuenca <- function(i){
  (id_cuenca <- cuencas$ID[i])
  
  print( paste0(' --- Cuenca ', i, '-', nrow(cuencas),  '  ID: ', id_cuenca))
  
  (cuenca_dir <- paste0(outDir, '/cuenca_ID', id_cuenca))
  dir.create(cuenca_dir)
  
  library(raster)
  raster(Ruta_bosques)
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
  
  
  ## Cortar rios ---
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
  if ( !file.exists(rio_rasterizado_100) ) {
    system.time(
      gdalUtilities::gdal_rasterize(src_datasource = archivo_buffer_100, 
                                    dst_filename = rio_rasterizado_100, 
                                    ot = 'Byte',
                                    burn = 1, tr = c(30, 30), init = 0,
                                    te =  extent_bosque)
    )
  }
  
  ### Calcular celdas/areas riparias ----
  estadisticas_buffer_30m <- capture.output(
    gdalUtilities::gdalinfo(rio_rasterizado_30, stats = TRUE, checksum = TRUE) )
  
  dimensiones_buff_30 <- as.numeric(strsplit(gsub('[[:alpha:]]| ','', 
                                                  grep('Size is', estadisticas_buffer_30m, value = TRUE) ),
                                             ",")[[1]])
  celdas_buff_30 <- Reduce(f = '*', x = dimensiones_buff_30)
  (promedio_buff_30 <- as.numeric(gsub(  "[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                                         grep('STATISTICS_MEAN', 
                                              estadisticas_buffer_30m, value = TRUE)) ) )
  (pixeles_zonas_riparias_30 <- celdas_buff_30 * promedio_buff_30)
  
  
  estadisticas_buffer_100m <- capture.output(
    gdalUtilities::gdalinfo(rio_rasterizado_100, stats = TRUE, checksum = TRUE) )
  
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
  
  
  
  # raster_buffer_30 <- raster(rio_rasterizado_30)
  # system.time(celdas_30m_raster <- raster::cellStats(raster_buffer_30, sum, na.rm = TRUE))
  # 0.031218244038617 * 21908 * 23943
  
  
  
  
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
  
  
  #Iterar sobre los bosques desde 2000 a la fecha
  (lista_archivos_bosque <- list.files(cuenca_dir, pattern = 'bosqueFecha.+.tif$', full.names = TRUE))
  
  for (f in 1:length(lista_archivos_bosque)){ # f = 1
    capa_i <- lista_archivos_bosque[f]
    (fecha <- gsub('[a-zA-Z]|.tif|_', '',  basename(capa_i)))
    print ( paste (' Cuenca ID:', id_cuenca,'  Estadisticas para la fecha', fecha))
    
    
    ### Cortar bosques riparios
    (bosque_buffer_30m <- paste0(cuenca_dir, #'C:/temp/',
                                 '/bosque_ripario30_', fecha , '.tif'))
    
    #raster_bosque <- raster(bosque_rasterizado)
    stack_capas_30m <- raster::stack(capa_i, rio_rasterizado_30)
    if (! file.exists(bosque_buffer_30m)){
      system.time( 
        bosques_riparios_30 <- stackApply(x = stack_capas_30m, indices = c(1, 1),
                                          fun=function(x, ...){
                                            ((x[[1]]) & (x[[2]]))
                                          }, na.rm = FALSE, filename = bosque_buffer_30m, 
                                          datatype = 'LOG1S')
      )
    }
    
    (bosque_buffer_30m_01 <- paste0(cuenca_dir, #'C:/temp/',
                                    '/bosque_ripario30_', fecha , '_01.tif'))
    ## Crear un archivo con 0 y 1 --------
    if (!file.exists(bosque_buffer_30m_01)){
      system.time(
        bosques_riparios_30_01 <- gdalUtilities::gdal_translate(
          stats = TRUE,
          src_dataset = bosque_buffer_30m, dst_dataset = bosque_buffer_30m_01,
          ot = 'Byte'))
    }
    
    (bosque_buffer_100m <- paste0(cuenca_dir, #'C:/temp/',
                                  '/bosque_ripario100_', fecha , '.tif'))
    
    stack_capas_100m <- raster::stack(capa_i, rio_rasterizado_100)
    if (!file.exists(bosque_buffer_100m)){
      system.time( 
        bosques_riparios_100 <- stackApply(x = stack_capas_100m, indices = c(1, 1),
                                           fun=function(x, ...){
                                             ((x[[1]]) & (x[[2]]))
                                           }, na.rm = FALSE, filename = bosque_buffer_100m, 
                                           datatype = 'LOG1S')
      )
    }
    
    (bosque_buffer_100m_01 <- paste0(cuenca_dir, #'C:/temp/',
                                     '/bosque_ripario100_', fecha , '_01.tif'))
    ## Crear un archivo con 0 y 1 --------
    if (!file.exists(bosque_buffer_100m_01)){
      system.time(
        bosques_riparios_100_01 <- gdalUtilities::gdal_translate(
          stats = TRUE,
          src_dataset = bosque_buffer_100m, dst_dataset = bosque_buffer_100m_01,
          ot = 'Byte'))
    }
    
    
    
    ### Extraer estadísticas
    estadisticas_ripario_30 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_30m_01, stats = TRUE, checksum = TRUE) )
    
    dimensiones_ripario_30 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_ripario_30,
                         value = TRUE) ), ",")[[1]])
    
    (celdas_ripario_30 <- Reduce(f = '*', x = dimensiones_ripario_30))
    (promedio_ripario_30 <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                                            grep('STATISTICS_MEAN', 
                                                 estadisticas_ripario_30, 
                                                 value = TRUE)) ) )
    (pixeles_bosques_riparios_30 <- celdas_ripario_30 * promedio_ripario_30)
    
    ### Extraer estadísticas
    estadisticas_ripario_100 <- capture.output(
      gdalUtilities::gdalinfo(bosque_buffer_100m_01, stats = TRUE, checksum = TRUE) )
    
    dimensiones_ripario_100 <- as.numeric(
      strsplit(gsub('[[:alpha:]]| ','', 
                    grep('Size is', estadisticas_buffer_100m,
                         value = TRUE) ), ",")[[1]])
    
    (celdas_ripario_100 <- Reduce(f = '*', x = dimensiones_ripario_100))
    (promedio_ripario_100 <- as.numeric(gsub("[^[:alnum:]\\-\\.\\s]|[[:alpha:]]", "",
                                             grep('STATISTICS_MEAN', 
                                                  estadisticas_ripario_100, 
                                                  value = TRUE)) ) )
    (pixeles_bosques_riparios_100 <- celdas_ripario_100 * promedio_ripario_100)
    
    (archivo_estadisticas <- paste0(cuenca_dir, 
                                    '/estadisticas_', fecha , '.csv'))
    
    if (!file.exists(archivo_estadisticas)){
      
      
      tabla_resumen <- data.frame(id_cuenca = id_cuenca,
                                  fecha = fecha,
                                  pix_rip30m = pixeles_zonas_riparias_30,
                                  pix_bosrip30m = pixeles_bosques_riparios_30,
                                  propbosrip30m = pixeles_bosques_riparios_30 / pixeles_zonas_riparias_30,
                                  pix_rip100m = pixeles_zonas_riparias_100,
                                  pix_bosrip100m = pixeles_bosques_riparios_100,
                                  propbosrip100m = pixeles_bosques_riparios_100 / pixeles_zonas_riparias_100
      )
      write.csv(tabla_resumen, file = archivo_estadisticas)
    }
    # ><
  }
  mapply(list.files(cuenca_dir, pattern = "*.tif$", full.names = T), FUN=file.remove)
}
##################################################################################################
## Iteracion por cada una de las cuencas del pais
for (i in 1:nrow(cuencas)){ # }
  # i = which(cuencasFull$ID == 159)
  tryCatch(
    calculo_por_cuenca(i),
    error=print(i)
  )
}

## Compilar datos de csv -----
(archivos_csv <- list.files(pattern = 'estadisticas_[0-9].+.csv', 
                           path = '.' , recursive = TRUE, 
                           full.names = TRUE))
tabla_compilada <- NULL
for (i in 1:length(archivos_csv)){
  tabla_compilada <- rbind(tabla_compilada , read.csv(archivos_csv[i]))
}

write.csv(tabla_compilada, 
          file = paste0(outDir, '/estadisticas_cuencas_compiladas_', Sys.Date(), '.csv'))



