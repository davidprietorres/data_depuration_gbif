#####################################################################
############### MANUAL DE BIOGEOGRAF?A ##############################
####### Practica de adquision de datos biologicos de especies #######
#####Elaborada por: David A. Prieto-Torres (FES-Iztacala, UNAM)######

#############################################################################################
###########PASO I: INTALACION Y ACTIVACION DE PAQUETES NECESARIOS############################
#############################################################################################
##esto solo es necesario instalarlo la primera vez ya luego NO, puede irse a la siguiente seccion.
install.packages ("rgbif")
install.packages ("TeachingDemos")
install.packages ("dismo")
install.packages ("biomod2")
install.packages ("sp")
install.packages ("raster")
install.packages ("usdm")
install.packages ("ENMeval")
install.packages ("foreign")
install.packages ("spocc")
install.packages ("corrplot")
install.packages ("XML")
install.packages ("dplyr")
install.packages ("reshape")
install.packages ("CoordinateCleaner")
install.packages ("sf")
install.packages ("tidyr")
install.packages ("terra")
install.packages("maps")
install.packages("rnaturalearth")

#########################
##Activar los paquetes###
#########################
library(rgbif)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(usdm)
library(foreign)
library(spocc)
library(corrplot)
library(XML)
library(dplyr)
library(reshape)
library(CoordinateCleaner)
library(sf)
library(terra)
library(maps)
library(rnaturalearth)


#############################################################################################
###########DESCARGA DE DATOS ESPECIE DE INTERES##############################################
#############################################################################################
#1.Descarga de datos de la especie 
species1<- gbif("Amazona", "xantholora*", geo=FALSE)##permite vincularse a la pagina GBIF y descargar datos, para ello es necesario escribir el nombre de la especie separando el g?nero del epiteto (el asterisco es para no descartar datos por las "subspecies").
View(species1)##permite visualizar la tabla de datos descargada, conformada por 4233 observaciones con 159 variables de informacion para cada registro segun el estandar de datos de Darwin Core.

#2.Limpieza automatica b?sica de los datos: quitar observaciones con informacion incompleta
data1 <- subset(species1, !is.na(lon) & !is.na(lat))#quita todos los datos que no tienen coordenadas geograficas
data2 <- subset(data1, !is.na(year))#quita todos los datos que no tienen informacion del ano (fecha)
data3 <- subset(data2, !is.na(adm1))#quita todos los datos que no tienen informacion del estado/localidad de colecta


#3.Crear un archivo en formato ".CSV" con la informacion basica necesaria para cada registro, incluyendo: "codigo GBIF", "institucion de procedencia", "numero de catalogo/coleccion", "taxonomia" y "coordenadas geograficas", asi como "ano", "pais" y "region" de colecta. NOTA: para este ejercicio solo se estan seleccionando 15 columnas de las 154 disponibles en GBIF, no obstante esto puede modificarse de acuerdo a la necesidades y preferencias de cada investigador
gbifID<-data3$gbifID###seleciona la informaci?n sobre el "c?digo GBIF" que corresponde a cada dato.
institutionCode<-data3$institutionCode###seleciona la informaci?n sobre el "instituci?n de procedencia" para cada dato.
catalogNumber<-data3$catalogNumber###seleciona la informaci?n sobre el "n?mero de cat?logo" de cada dato.
kingdom<-data3$kingdom###seleciona la informaci?n tax?nomica del "Reino" de mi especie de inter?s.
phylum<-data3$phylum###seleciona la informaci?n tax?nomica del "Phylum" de mi especie de inter?s.
class<-data3$class###seleciona la informaci?n tax?nomica del "Clase" de mi especie de inter?s.
order<-data3$order###seleciona la informaci?n tax?nomica del "Orden" de mi especie de inter?s.
family<-data3$family###seleciona la informaci?n tax?nomica del "Familia" de mi especie de inter?s.
genus<-data3$genus###seleciona la informaci?n tax?nomica del "G?nero" de mi especie de inter?s.
species<-data3$species###seleciona la informaci?n tax?nomica del "Especie" de mi especie de inter?s.
lat<-data3$lat###seleciona la informaci?n sobre la "Latitud" que corresponde a la localidad de muestreo de cada dato.
lon<-data3$lon###seleciona la informaci?n sobre la "Longitud" que corresponde a la localidad de muestreo de cada dato.
year<-data3$year###seleciona la informaci?n sobre el "A?O" de colecta o muestreo de cada dato.
country<-data3$country###seleciona la informaci?n sobre el "pa?s" de la localidad de muestreo de cada dato.
region<-data3$adm1###seleciona la informaci?n sobre el "estado o provincia" de la localidad de muestreo de cada dato.

##Unir toda la informacion en un solo archivo
data_cleaned1 <-data.frame(gbifID,institutionCode,catalogNumber,kingdom,phylum,class,order,family,genus,species,lon,lat,year,country,region)

View(data_cleaned1)##permite visualizar la tabla de datos descargada, conformada por 4006 observaciones con 15 variables de informacion para cada registro segun el estandar de datos de Darwin Core.

##pero podemos seguir limpiando la base de datos, por ejemplo:
data_cleaned2 <- subset(data_cleaned1, !is.na(institutionCode))#quita todos los datos que no tienen informacion de la institucion de procedencia
data_cleaned1 <- subset(data_cleaned2, !is.na(data_cleaned2$catalogNumber))#quita todos los datos que no tienen informacion del numero de catalogo o registro
View(data_cleaned1)##permite visualizar la tabla de datos descargada, conformada por 3936 observaciones con 15 variables de informacion para cada registro segun el estandar de datos de Darwin Core.


##Guardar el archivo creado para los datos datos descargados y filtrados
setwd("D:/proyectos_sigs/biogeografía2025-2/data_depuration_gbif-main/datos_descargados/")##permite indicar el directorio dentro del computador donde queremos guardar el archivo ".csv" contentivo de la informacion
write.csv(data_cleaned1, file = "Amazona_xantholora_gbif_original.csv", row.names = F) ###es el comando utilizado para guardar el archivo de nombre "Amazona_xantholora" con extensi?n ".csv"

###Tabla concatenada de GBIF mas otros museos####
data_cleaned2 <- read.csv2("D:/proyectos_sigs/biogeografía2025-2/data_depuration_gbif-main/datos_descargados/Amazona_xantholora_gbif_original.csv", sep = ",", header = TRUE) 
data_cleaned2$lat <- as.numeric(data_cleaned2$lat)##vuelve los datos de latitud en numeros
data_cleaned2$lon <- as.numeric(data_cleaned2$lon)##vuelve los datos de longitud en numeros

########################################################################################
########### LIMPIEZA ESPACIO-TEMPORAL DE LOS DATOS #####################################
########################################################################################
#1.Separaci?n de los datos de acuerdo a la informaci?n temporal
data4 <- subset(data_cleaned2, year > 1969 & year < 2001)###selecciona los datos de 1970 al 2000 (coincidentes con las capas ambientales que se usaran en los analisis subsecuente de modelado) en este caso son 87 datos
data5 <- subset(data_cleaned2, year > 2000)###selecciona los datos del 2001 al presente (NO coincidentes con las capas ambientales que se usaran en los an?lisis subsecuente de modelado)en este caso son 3701 datos


#2. Visualizacion y georeferenciacion de los datos
summary(data4$lat)###permite obtener los valores m?ximo y m?nimos de la latitud en los datos
summary(data4$lon)###permite obtener los valores m?ximo y m?nimos de la latitud en los datos

#############################
##generar el mapa del mundo##
#############################
windows()##crea una ventana nueva para guardar mapas con mejor resolucion
world <- vect(ne_countries(scale = "medium", returnclass = "sf"))##crea un modelo de mapas para el mundo

plot(world, xlim = c(-93, -85), ylim = c(15,22), col = "lightblue", border = "gray40")###dibujar un mapa estableciendo un acercamiento al area geografica de distribucion de los datos obtenidos para la especie. Los valores en xlim corresponden a las longitudes observadas en los regitros, mientras que ylim corresponde a las latitudes
points(data4$lon, data4$lat, col = "red", pch=20, cex= 2)###colocar los puntos de ocurrencia de mi especie en el mapa

##eliminar datos fuera de los rangos conocidos de la especie
datos6<-data4[(data4$lat>17.4),]#elimina los datos con latitud menor a 17.6?
plot(world, xlim = c(-93, -85), ylim = c(15,22), col = "lightblue", border = "gray40")###me permite ver un mapa con una escala mas cercana a la distribucion de mis datos
points(datos6$lon, datos6$lat, col = "red", pch=20, cex= 2)###colocar los puntos en el mapa

################################################################
#3. Limpieza espacial de los datos: eliminiacion de duplicados##
####las siguientes 4 lineas es una funcion que establece como usar los datos para hacer la limpieza (no modificar)###
clean_dup <- function(data,longitude,latitude,threshold=0.0){  data <- data[!is.na(data[,longitude]),]
dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
return(dat_sp1@data)}

###limpiar los dos set de datos creados: "datos6 = 1970-2000" y "data5 = 2001-2020"
data7 <- clean_dup(datos6,longitude = "lon",latitude = "lat",threshold = 0.041665)###datos 1970-2000 espacialmente limpios. En este caso de los  87 puntos que se tenian originalmente ahora solo quedan 27.
data8 <- clean_dup(data5,longitude = "lon",latitude = "lat",threshold = 0.041665)###datos 2001-2020 espacilamente limpios. En este caso de los  3701 puntos que se tenian originalmente ahora solo quedan 267

####################################################################################
#4. Calcular la distancia buffer para seleccionar datos del 2001-2020###############
## es decir la distancia minima entre datos para saber cuales si son buenos o no####
####################################################################################
###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geograficas
points_occ2000 <- SpatialPointsDataFrame(data7[,11:12],data7)#convertir a un archivo shp de puntos 1970-2000 (es decir los viejos)
points_occ2020 <- SpatialPointsDataFrame(data8[,11:12],data8)##convertir a un archivo shp de puntos 2001-2020 (es decir los nuevos)

points_occ2000_2 <- st_as_sf (points_occ2000)##convierte el archivo de puntos en el formato que se necesita.
points_occ2020_2 <- st_as_sf (points_occ2020)##convierte el archivo de puntos en el formato que se necesita.

###Calcular distancia promedio y el filtro buffer a aplicar entre los puntos
DistanciaPuntos <- st_distance(points_occ2000_2, points_occ2000_2)##calcula la distancia promedio de los puntos de 1970 al 2000. Es decir, nos dice cual es la distancia de un punto a los otros 26, asi de todos con todos
buffer=(mean(DistanciaPuntos)*2)/10 #me da el valor m?ximo observado entre los datos para calcular la distancia que se usara como ?rea BUFFER para seleccionar cuales datos entre 2001-2020 entraran en nuestra matriz. En este caso el valor obtenido es 0.5549 grados, equivalentes a 66.58km


###aplicar la distancia buffer entre los puntos de 1970-2000 para seleccionar los datos del 2001-2020
buffer_points <- st_buffer(points_occ2000_2, dist = buffer)###aplica la distancia buffer a los puntos del 2001-2020. Es decir a cada uno de los 27 puntos viejos le dibuja un circulo con radio de 66km a fin de establecer las zonas buffer donde todo lo que este dentro se considerara como bueno

####visualizaci?n de los datos por separado
plot(world, xlim = c(-93, -85), ylim = c(15,22), col = "lightblue", border = "gray40")###dibujar un mapa estableciendo 
points(points_occ2020, col="black", pch=20, cex= 2)#grafica los puntos de 2001-presente para ver su posicion geografica
points(points_occ2000, col="red", pch=20, cex= 2)#grafica los puntos de 1970-2000 para ver su posici?n geogr?fica
plot(buffer_points, add = TRUE, border = "blue", lwd = 2, col = NA)###grafica el area buffer que fue definida para los datos.

#Seleccionar los puntos del 2001-2020 que entran en el filtro buffer aplicado
data_poly_all <- st_intersects(points_occ2020_2,buffer_points, fn = NULL)###selecciona los puntos que si entran en el analisis
en_poligono_index <- which(!is.na(data_poly_all))
p_en_poligono <- points_occ2020_2[buffer_points,]

####visualizaci?n de los datos por separado
plot(world, xlim = c(-93, -85), ylim = c(15,22), col = "lightblue", border = "gray40")###dibujar un mapa estableciendo 
points(points_occ2020, col="black", pch=20, cex= 2)#grafica los puntos de 2001-2020 para ver su posici?n geogr?fica
points(points_occ2000, col="red", pch=20, cex= 2)#grafica los puntos de 1970-2000 para ver su posici?n geogr?fica
plot(buffer_points, add = TRUE, border = "blue", lwd = 0.9, col = NA)###gr?fica el ?rea buffer que fue definida para los datos.
points(p_en_poligono, col="blue", pch=20, cex= 2)### graf?ca de color verde los puntos del 2001-2020 que si entran en el analisis

selected_data <- data.frame(p_en_poligono)### para guardar el archivo de puntos

##################################################################
############# LIMPIEZA ECOLOGICA DE LOS DATOS ####################
##################################################################
#1.Verificar informacion ambiental en mis localidades de ocurrencia
setwd("D:/proyectos_sigs/biogeografía2025-2/data_depuration_gbif-main/coberturas_presente/")## directorio donde estan las capas climaticas del presente 
pca_path <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack de las variables ambientales
capas_presente<- stack(pca_path)
plot(capas_presente$bio_1)

###Para los siguientes pasos, trabajaremos solo con las 3 columnas que nos interesan: nombre de la especie, longitud y latitud 
species<-points_occ2000$species ## seleccionar la columna del nombre de la especie para los datos 1970-2000
lat<-as.numeric(points_occ2000$lat)## seleccionar la columna del nombre de la latitud para los datos 1970-2000
lon<-as.numeric(points_occ2000$lon)## seleccionar la columna del nombre de la longitud para los datos 1970-2000
datos_2000<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 1970-2000

###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geogr?ficas
points_occ2000 <- SpatialPointsDataFrame(datos_2000[,2:3],datos_2000)#convertir a un archivo shp de puntos 1970-2000

#2. extraemos los valores ambientales para esas localidad 
presencias_clima <- data.frame(raster::extract(capas_presente,points_occ2000[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia
presencias_clima2<-data.frame(points_occ2000,presencias_clima)##crear una tabla de datos con los valores climaticos para mis registros del 1970-2000
presencias_clima3 <- na.omit(presencias_clima2)## omite mis datos de presencia sin valores ambientales.

#3. Calcular los intervalos de referencias para los valores ambientales de las variables m?s importantes en la distribuci?n de mi especie. Aqu? proponemos realizar este calculo a trav?s del valor promedio +/- 2 veces la desviaci?n estandar encontrada en los datos de 1970-2001. Estos valores ser?n utilizados para realizar un filtrado ambiental de los datos del 2001-2020.
hist(presencias_clima3$dem)
dem_min= (mean(presencias_clima3$dem)) - ((sd(presencias_clima3$dem))*2)#valor m?nimo definido para la elevaci?n
dem_max= (mean(presencias_clima3$dem)) + ((sd(presencias_clima3$dem))*2)#valor m?ximo definido para la elevaci?n

bio1_min= (mean(presencias_clima3$bio_1)) - ((sd(presencias_clima3$bio_1))*2)#valor m?nimo definido para bio1
bio1_max= (mean(presencias_clima3$bio_1)) + ((sd(presencias_clima3$bio_1))*2)#valor m?ximo definido para bio1

bio12_min= (mean(presencias_clima3$bio_12)) - ((sd(presencias_clima3$bio_12))*2)#valor m?nimo definido para bio12
bio12_max= (mean(presencias_clima3$bio_12)) + ((sd(presencias_clima3$bio_12))*2)#valor m?ximo definido para bio12

bio15_min= (mean(presencias_clima3$bio_15)) - ((sd(presencias_clima3$bio_15))*2)#valor m?nimo definido para bio15
bio15_max= (mean(presencias_clima3$bio_15)) + ((sd(presencias_clima3$bio_15))*2)#valor m?ximo definido para bio15


#4.realizar el filtrado ambiental de los datos para las localidades obtenidas desde el 2001 al presente
###Creamos un archivo con solo las 3 columnas que nos interesan
species<-points_occ2020$species ##columna del nombre de la especie para los datos "seleccionados" de 2001 al presente
lat<-as.numeric(points_occ2020$lat)##columna del nombre de la latitud para los datos "seleccionados" de 2001 al presente
lon<-as.numeric(points_occ2020$lon)##columna del nombre de la longitud para los datos "seleccionados" de 2001 al presente
datos_2020<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables!

###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geogr?ficas
points_occ2020 <- SpatialPointsDataFrame(datos_2020[,2:3],datos_2020)#convertir a un archivo shp de puntos 1970-2000


##extraemos los valores ambientales para esas localidad y omitimos los datos sin informacion
presencias2020_clima <- data.frame(raster::extract(capas_presente,points_occ2020[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia "seleccionados" de 2001 al presente
presencias2020_clima2<-data.frame(points_occ2020,presencias2020_clima)##crear una tabla de datos con los valores climaticos para mis registros "seleccionados" de 2001 al presente
presencias2020_clima3 <- na.omit(presencias2020_clima2)## omite mis datos de presencia sin valores ambientales


##Limpiar los datos de 2001-2020que estan fuera de los valores ambientales de distribuci?n definidos:
data_dem <- subset(presencias2020_clima3, dem > -95 & dem < 250)#elimina datos con rango altitudinal superior a los 197msnm
data2020_1 <- subset(data_dem, bio_1 > 18.5 & bio_1 < 31.8)#elimina datos con temperatura promedio anual inferior a 18.58°C y superiores a 31.76°C
data2020_12 <- subset(data2020_1, bio_12 > 890 & bio_12 < 2000)#elimina datos con precipitaci?n anual inferior a 898.7mm y superiores a 1999mm
data2020_15 <- subset(data2020_12, bio_15 > 39 & bio_15 < 70)#elimina datos con precipitaci?n estacional inferior a 39.7 y superiores a 69.61

data_1970_2000_fin <- presencias_clima3###corresponde al archivo final de datos para los a?os entre 1970 y 2000 
data_2001_2021_fin <- data2020_15###corresponde al archivo final de datos para los a?os entre 2001 y el presente
data_buffer <- selected_data###corresponde al archivo final de datos para los a?os entre 2001 y el presente

#5. Armar el ?nico archivo .CSV con los registros validados y depurados de la especie 
species<- c(data_1970_2000_fin$species, data_2001_2021_fin$species, data_buffer$species)##selecciona y une las columnas "species" de ambos archivos
lat<-as.numeric(c(data_1970_2000_fin$lat, data_2001_2021_fin$lat, data_buffer$lat))##selecciona y une las columnas "latitud" de ambos archivos
lon<-as.numeric(c(data_1970_2000_fin$lon, data_2001_2021_fin$lon, data_buffer$lon))##selecciona y une las columnas "longitud" de ambos archivos

##Unir toda la informacion en un solo archivo
data_cleaned2 <-data.frame(species,lat,lon)###crea el archivo dataframe concatenado.
data_cleaned2$species_name <- "Amazona_xantholora"#crea una columna/variable colocando el nombre de la especie
data_cleaned3<-select(data_cleaned2, -(species))#elimina la columna "species" ya que solo contiene valores "1"
data_cleaned4 <- data_cleaned3%>%select(species_name,lon,lat)##ordena las columnas en species_name, lon y lat


View(data_cleaned4)##permite visualizar la tabla de datos concatenados: 248 observaciones con 3 variables de informaci?n

#6. Quitar datos duplicados (mismas coordenadas geograficas)
data10 <- clean_dup(data_cleaned4,longitude = "lon",latitude = "lat",threshold = 0.041665)### datos espacialmente limpios

#7.####visualizaci?n de los datos finales
windows()
plot(world, xlim = c(-93, -85), ylim = c(15,22), col = "lightblue", border = "gray40")###me permite ver un mapa con una escala mas cercana a la distribucion de mis datos
points(data10$lon, data10$lat, col = "red", pch=20, cex= 2)###colocar los puntos en el mapa

##Guardar el archivo en el PC: datos limpios
setwd("D:/proyectos_sigs/biogeografía2025-2/data_depuration_gbif-main/datos_descargados/")##carpeta donde guardar el archivo final de los datos
write.csv(data10, file = "Amazona_xantholora_limpios.csv", row.names = F)###es el comando utilizado para guardar el archivo de nombre "Amazona_xantholora" con extension ".csv" En total quedaron 248 datos

#guardemos el archivo en formato shapefile
shape_sf <- st_as_sf(data10, coords = c("lon", "lat"), crs = 4326)#convierte el archivo en formato shapefile de puntos
setwd("D:/proyectos_sigs/biogeografía2025-2/data_depuration_gbif-main/formatos_shapefile/")#directorio donde guardare el archivo
st_write(shape_sf, "Amazona_xantholora.shp", driver = "ESRI Shapefile")#guarda el archivo en formato shapefile de puntos con el nombre entre comillas

##Fin




