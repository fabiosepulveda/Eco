
###################################################
##### Visualización Tipos de Datos Espaciales #####
###################################################


#--------- Usando plot ------------#
library(sp)
data(meuse)
class(meuse)
head(meuse)
?meuse

#convertir a objeto espacial y visualizar
coordinates(meuse) = c("x", "y")
class(meuse)
summary(meuse)
proj4string(meuse) <- CRS("+init=epsg:28992")
plot(meuse, pch=1, main="Puntos") 

cc = coordinates(meuse)
m.sl = SpatialLines(list(Lines(list(Line(cc)), "line1")))
plot(m.sl, main="Líneas") 

data(meuse.riv)
meuse.lst = list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")) 
meuse.pol = SpatialPolygons(meuse.lst) 
plot(meuse.pol, col = "grey",main="Área")

#Combinar varios datos espaciales
plot(meuse,pch=1)
plot(meuse.pol, col = "royalblue", add = TRUE)

#--------- Usando spplot ------------#

library(lattice)
spplot(meuse, c("zinc"), main="Distribución Espacial del Zinc")


#--------- Usando ggplot ------------#

library(ggplot2)
methods(fortify)
m = as(meuse, "data.frame")
class(m)
ggplot(m, aes(x, y)) + geom_point() + coord_equal()


ggplot(m, aes(x,y)) + geom_point(aes(color = zinc), size = 2, alpha = 3/4) +
  ggtitle("Zinc concentration (ppm)") + coord_equal() + theme_bw()


#Usando OpenStreetMap
library(mapview)
mapview(meuse['zinc'])

#Otro dato espacial: Área
library(GWmodel)
?GWmodel
data(DubVoter)
class(Dub.voter)
plot(Dub.voter, main="Área")