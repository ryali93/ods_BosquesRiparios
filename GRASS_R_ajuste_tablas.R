library(rgdal)
library(foreign)

anp <- read.dbf('F:/ods15/riparios/08_SERNANP/anp.dbf', as.is = TRUE)
anp$anpid <- 1:nrow(anp) # Este valor será como el consecutivo "cat" que genera GRASS
head(anp) # 

## Extraer el ID de alguna AP
anp[grep(pattern = 'Bosque Montano de Carpish', anp$anp_nomb),]

##


##
anpbuf <- read.csv('F:/ods15/riparios/04_calculoANP/anpbuf.csv', as.is = TRUE)
dim(anpbuf)
head(anpbuf)

anpbufbos <- read.csv('F:/ods15/riparios/04_calculoANP/anpbufbos.csv', as.is = TRUE)
dim(anpbufbos)
head(anpbufbos)

anpbufbosdef <- read.csv('F:/ods15/riparios/04_calculoANP/anpbufbosdef.csv', as.is = TRUE)
dim(anpbufbosdef)
head(anpbufbosdef)


# ex5a <- anpbuf[anpbuf$a_cat == 5, ]
# ex5b <- (anpbufbos[anpbufbos$a_a_cat == 5, c('a_a_cat', 'a_b_a_buf30d', 'a_basm2')])
# head(ex5a)
# head(ex5b)
# (ex30a <- sum(ex5a$basm2[which(ex5a$b_a_buf30d == 30) ]) )
# (ex30b <- sum(ex5b$a_basm2[which(ex5b$a_b_a_buf30d == 30) ]) )


### Extraer tabla con áreas riparias por ANP, tanto para 30 como 100m
## 30m ----
#uniqueTableAnpRiparian30 <- unique(csv[, c('a_a_b_cat', 'a_a_b_anp_nomb', 'a_a_buf30', 'a_a_buf30id', 'a_a_b30m2')])
uniqueTableAnpRiparian30 <- unique(anpbuf[which(anpbuf$b_a_buf30d == 30), c('a_cat', 'a_anp_nomb', 'b_a_buf30d', 'basm2')])
dim(uniqueTableAnpRiparian30)
head(uniqueTableAnpRiparian30)
uniqueTableAnpRiparian30 <- uniqueTableAnpRiparian30[order(uniqueTableAnpRiparian30$a_cat), ]
head(uniqueTableAnpRiparian30)
# a_b_b_cat == anpID
ripArea30 <- data.frame(b30m2 = xtabs(basm2 ~ a_cat , data = uniqueTableAnpRiparian30), stringsAsFactors = FALSE)
colnames(ripArea30) <- c('anpid', 'rip30m2')
ripArea30$anpid <- as.numeric(as.character(ripArea30$anpid))
head(ripArea30)

## 100m ----
uniqueTableAnpRiparian100 <- unique(anpbuf[which(anpbuf$b_b_bufw100d == 100), c('a_cat', 'a_anp_nomb', 'b_b_bufw100d', 'basm2')])
dim(uniqueTableAnpRiparian100)
uniqueTableAnpRiparian100 <- uniqueTableAnpRiparian100[order(uniqueTableAnpRiparian100$a_cat), ]
dim(uniqueTableAnpRiparian100)
head(uniqueTableAnpRiparian100)

# a_b_b_cat == anpID
ripArea100 <- data.frame(b100m2 = xtabs(basm2 ~ a_cat , data = uniqueTableAnpRiparian100))
colnames(ripArea100) <- c('anpid', 'rip100m2')
ripArea100$anpid <- as.numeric(as.character(ripArea100$anpid))


head(ripArea30)
head(ripArea100)

## Ya tenemos las tablas con el 100% del área riparia para cada ancho de bufer para cada ANP


## Calcular area en bosque de cada zona ANP en zonas riparias
dim(anpbufbos)
head(anpbufbos)

anpbufbos[grep(pattern = 'Bosque Montano de Carpish', anpbufbos$a_a_anp_nomb),]


## Valores unicos de coberturas
unique(anpbufbos$b_Cobertura)


## 30 metros ---- 
areaBos30 <- anpbufbos[ which( (anpbufbos$b_Cobertura %in% c("Perdida 2001-2020", 'Bosque 2020')) & anpbufbos$a_b_a_buf30d == 30), 
                        c('a_a_cat', 'a_a_anp_nomb', 'a_b_a_buf30d', 'b_Cobertura', 'bosm2') ]
head(areaBos30)
areaBos30 <- data.frame(b100m2 = xtabs(bosm2 ~ a_a_cat , data = areaBos30))
colnames(areaBos30) <- c('anpid', 'bos30mm2')
head(areaBos30)

## 100 metros ---- 
areaBos100 <- anpbufbos[ which( (anpbufbos$b_Cobertura %in% c("Perdida 2001-2020", 'Bosque 2020')) & anpbufbos$a_b_b_bufw100d == 100), 
                        c('a_a_cat', 'a_a_anp_nomb', 'a_b_b_bufw100d', 'b_Cobertura', 'bosm2') ]
head(areaBos100)
areaBos100 <- data.frame(b100m2 = xtabs(bosm2 ~ a_a_cat , data = areaBos100))
colnames(areaBos100) <- c('anpid', 'bos100m2')
head(areaBos100)


head(areaBos30)
head(areaBos100)


## Calcular area deforestada de cada zona ANP en zonas riparias

dim(anpbufbosdef)
head(anpbufbosdef)

anpbufbosdef[grep(pattern = 'Bosque Montano de Carpish', anpbufbosdef$a_a_a_anp_nomb),]

## Valores unicos de annos
unique(anpbufbosdef$b_md_anno)
## Valores unicos de ANP ID
unique(anpbufbosdef$a_a_a_cat)


## 30 metros ---- 
areaDef30 <- anpbufbosdef[ which( anpbufbosdef$a_a_b_a_buf30d == 30), 
                        c('a_a_a_cat', 'a_a_a_anp_nomb', 'a_a_b_a_buf30d', 'b_md_anno', 'bosdefm2') ]
head(areaDef30)
areaDef30 <- as.data.frame.matrix(xtabs(bosdefm2 ~ a_a_a_cat + b_md_anno, data = areaDef30))
head(areaDef30)

## 100 metros ---- 
areaDef100 <- anpbufbosdef[ which( anpbufbosdef$a_a_b_b_bufw100d == 100), 
                           c('a_a_a_cat', 'a_a_a_anp_nomb', 'a_a_b_b_bufw100d', 'b_md_anno', 'bosdefm2') ]
head(areaDef100)
areaDef100 <- as.data.frame.matrix(xtabs(bosdefm2 ~ a_a_a_cat + b_md_anno, data = areaDef100))
head(areaDef100)


head(areaDef30)
head(areaDef100)


### Consolidar tablas

head(ripArea30)
head(ripArea100)
dim(ripArea30)
dim(ripArea100)

head(areaBos30)
head(areaBos100)
dim(areaBos30)
dim(areaBos100)


head(areaDef30)
head(areaDef100)
dim(areaDef30)
dim(areaDef100)



## Unir tablas --- Zonas riparias con bosques

result30 <- ripArea30
pos30 <- match(result30$anpid, areaBos30$anpid)
result30[which(!is.na(pos30)), 'bos30m2'] <- areaBos30$bos30mm2[na.omit(pos30)]
head(result30)

result100 <- ripArea100
pos100 <- match(result100$anpid, areaBos100$anpid)
result100[which(!is.na(pos100)), 'bos100m2'] <- areaBos100$bos100m2[na.omit(pos100)]
head(result100)


## Unir tablas --- Zonas riparias y bosques con deforestacion

def30Completa <- as.data.frame(matrix(data = 0, ncol = ncol(areaDef30), nrow = nrow(result30)))
rownames(def30Completa) <- result30$anpid
colnames(def30Completa) <- colnames(areaDef30)

pos30Rows <- match(rownames(def30Completa), rownames(areaDef30))
pos30Cols <- match(colnames(def30Completa), colnames(areaDef30))


## Asegurarnos que ambas tablas tengan mismas dimensiones
dim(def30Completa[which(!is.na(pos30Rows)), which(!is.na(pos30Cols))])
dim(areaDef30[na.omit(pos30Rows), na.omit(pos30Cols)])

## Insertar datos de deforestacion en tabla de todos los poligonos
def30Completa[which(!is.na(pos30Rows)), which(!is.na(pos30Cols))] <- areaDef30[na.omit(pos30Rows), na.omit(pos30Cols)]


(defTable30 <- t(apply(def30Completa, 1,  cumsum)))
pos30 <- match(result30$anpid, rownames(defTable30))
midTable <- cbind("2000" = result30$bos30m2[which(!is.na(pos30))], result30$bos30m2[which(!is.na(pos30))] - defTable30)
colnames(midTable) <- paste0('a30_', colnames(midTable))

result30[which(!is.na(pos30)), colnames(midTable)] <- midTable
result30[, gsub('a30_', 'p30_', colnames(midTable))] <- result30[, colnames(midTable)] / result30$rip30m2


## Capa de 100
def100Completa <- as.data.frame(matrix(data = 0, ncol = ncol(areaDef100), nrow = nrow(result100)))
rownames(def100Completa) <- result100$anpid
colnames(def100Completa) <- colnames(areaDef100)

pos100Rows <- match(rownames(def100Completa), rownames(areaDef100))
pos100Cols <- match(colnames(def100Completa), colnames(areaDef100))


## Asegurarnos que ambas tablas tengan mismas dimensiones
dim(def100Completa[which(!is.na(pos100Rows)), which(!is.na(pos100Cols))])
dim(areaDef100[na.omit(pos100Rows), na.omit(pos100Cols)])

## Insertar datos de deforestacion en tabla de todos los poligonos
def100Completa[which(!is.na(pos100Rows)), which(!is.na(pos100Cols))] <- areaDef100[na.omit(pos100Rows), na.omit(pos100Cols)]


(defTable100 <- t(apply(def100Completa, 1,  cumsum)))
pos100 <- match(result100$anpid, rownames(defTable100))
midTable <- cbind("2000" = result100$bos100m2[which(!is.na(pos100))], result100$bos100m2[which(!is.na(pos100))] - defTable100)
colnames(midTable) <- paste0('a100_', colnames(midTable))

result100[which(!is.na(pos100)), colnames(midTable)] <- midTable
result100[, gsub('a100_', 'p100_', colnames(midTable))] <- result100[, colnames(midTable)] / result100$rip100m2


# pos100 <- match(result100$anpid, rownames(areaDef100))
# (midTable <- t(apply(areaDef100[na.omit(pos100), ], 1,  cumsum)))
# midTable <- cbind("2000" = result100$bos100m2[which(!is.na(pos100))], result100$bos100m2[which(!is.na(pos100))] - midTable)
# colnames(midTable) <- paste0('a100_', colnames(midTable))
# 
# result100[which(!is.na(pos100)), colnames(midTable)] <- midTable
# result100[,gsub('a100_', 'p100_', colnames(midTable))] <- result100[, colnames(midTable)] / result100$rip100m2



## 
ANP <- readOGR('F:/ods15/riparios/08_SERNANP/anp_proj.shp')
ANP$anpid <- 1:nrow(ANP) # Este valor será como el consecutivo "cat" que genera GRASS
ANP$anpid
anp$anpid
identical(ANP$anpid, anp$anpid)



pos30 <- match(ANP$anpid, result30$anpid)
pos100 <- match(ANP$anpid, result100$anpid)


ANP[which(!is.na(pos30)), colnames(result30)] <- result30[na.omit(pos30), ] 
ANP[which(!is.na(pos100)), colnames(result100)] <- result100[na.omit(pos100), ] 


writeOGR(ANP, 'F:/ods15/riparios/04_calculoANP', 
         paste0('Indicador_riparios_ANP_Vector_GRASS_', Sys.Date()), 
         driver = 'ESRI Shapefile', overwrite_layer = TRUE)
