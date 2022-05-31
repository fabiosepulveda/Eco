#### Librerías
library(ggfortify)
library(seasonal)
library(plyr)
library(quantmod)
library(tidyverse)
library(gganimate)
library(lubridate)
library(quantmod)
library(highcharter)
library(ggplot2)
library(directlabels)
library(gganimate)
library(tidyverse)
library(tibble)
library(png)
library(gifski)
library(rgdal)
library(leaflet)
library(sf)
library(viridis)
library(leafsync)
library(mapview)
library(xts)
library(dygraphs)


### Lectura de datos
setwd("C:\\Users\\fhsepulveda\\OneDrive - Universidad de Medellin\\Documents\\Movilidad\\Transito\\SesionDocente\\Datos mundiales")
Co = read.csv("Colombia.csv")
Chile = read.csv("Chile.csv")
China = read.csv("China.csv")
USA = read.csv("Estados Unidos.csv")
Japon = read.csv("Japon.csv")
haiti = read.csv("Haiti.csv")

### Función Determinar Índice
I = list()
Grafico = function(x){
  I[[1]]=100*(1+x[1,2]/100)
  for (i in 1:(nrow(x)-1)) {
    I[[i+1]]= I[[i]]*(1+x[i+1,2]/100)
  }
  j=data.frame(cbind(I))
  Indice = data.frame(x,j)
  names(Indice)[3] = "I"
  Indice
}

### Gráficos series países
Colom = data.frame(Año=as.numeric(Grafico(Co)$Año),I=as.numeric(unlist(Grafico(Co)$I)),PIB=Co$PIB)
EstaU = data.frame(Año=as.numeric(Grafico(USA)$Año),I=as.numeric(unlist(Grafico(USA)$I)),PIB=USA$PIB)
Japan = data.frame(Año=as.numeric(Grafico(Japon)$Año),I=as.numeric(unlist(Grafico(Japon)$I)),PIB=Japon$PIB)
chile1 = data.frame(Año=as.numeric(Grafico(Chile)$Año),I=as.numeric(unlist(Grafico(Chile)$I)),PIB=Chile$PIB)
Haiti = data.frame(Año=as.numeric(Grafico(haiti)$Año),I=as.numeric(unlist(Grafico(haiti)$I)),PIB=haiti$PIB)
Colom$País = "Colombia"
EstaU$País = "USA"
Japan$País = "Japón"
chile1$País = "Chile"
Haiti$País = "Haiti"
Datos = rbind(Colom,EstaU,Japan,chile1,Haiti)
head(Datos)

serieT <- ggplot(data=Datos, aes(x=Año, y=PIB, group = País,colour = País))+
  geom_line(size=1) +
  labs(x= "Año", y= "PIB (Tasa)", title = "Serie Histórica del PIB, 1961 - 2021", 
       caption =  "Fuente: Banco Mundial")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
        axis.line = element_line(colour = "grey50", size = 1))

serieT

serie <- ggplot(data=Datos, aes(x=Año, y=I, group = País,colour = País))+
  geom_line(size=1) +
  labs(x= "Año", y= "PIB (Base 100)", title = "Serie Histórica del PIB, 1961 - 2021", 
       caption =  "Fuente: Banco Mundial")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
        axis.line = element_line(colour = "grey50", size = 1))

serie

### Series por pares
Cbia = data.frame(Co,País = rep("Colombia"))
EU = data.frame(USA,País = rep("USA"))
Datos1 = rbind(Cbia,EU)

serieCOUSA <- ggplot(data=Datos1, aes(x=Año, y=PIB, group = País,colour = País))+
  geom_line(size=1) +
  labs(x= "Año", y= "PIB (% anual)", title = "Serie Histórica del PIB: Colombia Vs USA, 1961 - 2021", 
       caption =  "Fuente: Banco Mundial")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
        axis.line = element_line(colour = "grey50", size = 1))

serieCOUSA

Ha = data.frame(haiti,País = rep("Haití"))
Datos2 = rbind(Cbia,Ha)

serieCOHA <- ggplot(data=Datos2, aes(x=Año, y=PIB, group = País,colour = País))+
  geom_line(size=1) +
  labs(x= "Año", y= "PIB (% anual)", title = "Serie Histórica del PIB: Colombia Vs Haití, 1961 - 2021", 
       caption =  "Fuente: Banco Mundial")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
        axis.line = element_line(colour = "grey50", size = 1))

serieCOHA

ColoUSA = data.frame(Año=as.numeric(Co$Año),Colombia = as.numeric(Co$PIB),USA=as.numeric(USA$PIB))
ColoUSA$Año <- as.Date(as.character(ColoUSA$Año), format = "%Y")
ColoUSA$Año = (ColoUSA$Año)+222
qxts1 <- xts(ColoUSA[,-1], order.by=ColoUSA[,1])
dygraph(data =qxts1, main = "Serie Histórica del PIB: Colombia Vs USA, 1961 - 2021", xlab = "Año",ylab = "PIB (% anual)")%>% 
dyOptions(colors = c("red", "blue")) %>% 
dyRangeSelector()

ColoHAi = data.frame(Año=as.numeric(Co$Año),Colombia = as.numeric(Co$PIB),Haití=as.numeric(haiti$PIB))
ColoHAi$Año <- as.Date(as.character(ColoHAi$Año), format = "%Y")
ColoHAi$Año = (ColoHAi$Año)+222
qxts1 <- xts(ColoHAi[,-1], order.by=ColoHAi[,1])
dygraph(data =qxts1, main = "Serie Histórica del PIB: Colombia Vs Haití, 1961 - 2021", xlab = "Año",ylab = "PIB (% anual)")%>% 
dyOptions(colors = c("red", "blue")) %>% 
dyRangeSelector()

### Estadísticos resumen
summary(Co)

matriz = rbind(mean(Co[1:11,2]), mean(Co[12:22,2]),mean(Co[23:34,2]),mean(Co[35:40,2]),mean(Co[41:47,2]),
               mean(Co[48:60,2]))
colnames(matriz) = c("Media")
rownames(matriz) = c("1961-1971","1972-1982","1983-1994","1995-2000","2001-2007","2008-2020")
matriz

summary(USA)
summary(haiti)

Datos3 = rbind(Cbia,Ha, EU)
head(Datos3)
ggplot(Datos3, aes(x = País, y = PIB, fill = País)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  labs(title="Box plot del PIB", caption="Fuente: Banco Mundial",y= "PIB (% anual)")+
  geom_boxplot()+
  geom_jitter()


ggplot(data = Co, aes(x = Año, y = PIB)) +
geom_point(col = "darkgrey")+
geom_line()+
geom_smooth(method = "loess", formula = y ~ x, span = 0.3,color = "orange", se = F)

ColoSuave <- loess(PIB ~ Año, span = 0.3, data = Co)
SuavColo = data.frame(Año=as.numeric(Co$Año),Colombia=as.numeric(Co$PIB),Ajuste=ColoSuave$fitted)
SuavColo$Año <- as.Date(as.character(SuavColo$Año), format = "%Y")
SuavColo$Año = (SuavColo$Año)+222
qxts <- xts(SuavColo[,-1], order.by=SuavColo[,1])
dygraph(data =qxts, main = "Ajuste suave del PIB Para Colombia", xlab = "Año",ylab = "PIB (% anual)")%>% 
dyOptions(colors = c("red", "blue")) %>% 
dyRangeSelector()

USASuave <- loess(PIB ~ Año, span = 0.3, data = USA)
SuavUSA = data.frame(Año=as.numeric(USA$Año),USA=as.numeric(USA$PIB),Ajuste=USASuave$fitted)
SuavUSA$Año <- as.Date(as.character(SuavUSA$Año), format = "%Y")
SuavUSA$Año = (SuavUSA$Año)+222
qxts <- xts(SuavUSA[,-1], order.by=SuavUSA[,1])
dygraph(data =qxts, main = "Ajuste suave del PIB Para USA", xlab = "Año",ylab = "PIB (% anual)")%>% 
dyOptions(colors = c("red", "blue")) %>% 
dyRangeSelector()



### 1. Serie Animada Colombia
rm(Colom)
Colom = data.frame(Año=as.numeric(Grafico(Co)$Año),I=as.numeric(unlist(Grafico(Co)$I)))
Colom$Año <- as.Date(as.character(Colom$Año), format = "%Y")
Colom$Año = (Colom$Año)+228
qxts <- xts(Colom[,-1], order.by=Colom[,1])
hchart(qxts)

### 2. Serie Animada Colombia
rm(Colom)
Colom = data.frame(Año=as.numeric(Grafico(Co)$Año),I=as.numeric(unlist(Grafico(Co)$I)))
Colom$Año <- as.Date(as.character(Colom$Año), format = "%Y")
Colom$Año = (Colom$Año)+228
as_tibble(Colom)

plot_animated_2 <- Colom %>%
  ggplot(aes(x = Año, y = I)) +
  geom_bar(stat= "identity", color = "white", fill = "grey90", show.legend = FALSE) +
  geom_line(color = "orange", size = 1.5) +
  geom_point(color = "orange", size = 4) +
  geom_text(aes(x= min(Año), y = min(I), label = as.factor(Año)),
  hjust = -0.05, vjust = -3, alpha = 0.5, col = "gray80", size = 40)+
  transition_reveal(Año) +
  labs(x= "Año", y= "PIB (Base 100)", title = "Serie Histórica del PIB de Colombia, 1961 - 2021", 
  caption =  "Fuente: Banco Mundial")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
  axis.line = element_line(colour = "grey50", size = 1))

animate(plot_animated_2, nframes= 400, width = 1000, height= 600, fps = 7,renderer = gifski_renderer(),end_pause = 300)
anim_save("election.gif")

### Dispersión
rm(Colom)
Colom = data.frame(Año=as.numeric(Grafico(Co)$Año),I=as.numeric(unlist(Grafico(Co)$I)))
Colom$País = "Colombia"
EstaU$País = "USA"
chile1$País = "Chile"
Haiti$País = "Haiti"
Datos = rbind(Colom,EstaU,chile1,Haiti)
head(Datos)

ggplot(Datos, aes(x = País, y = I, fill = País)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  labs(title="Box plot del PIB", caption="Fuente: Banco Mundial")+
  geom_boxplot()+
  geom_jitter()

### Suavizado para analizar punto de inflexión
rm(Colom)
Colom = data.frame(Año=as.numeric(Grafico(Co)$Año),I=as.numeric(unlist(Grafico(Co)$I)))

modelo.local <- loess(I ~ Año, span = 0.7, data = Colom)
ggplot(data = Colom, aes(x = Año, y = I)) +
geom_smooth(method = "loess", formula = y ~ x, span = 0.1,color = "orange", se = F)

### Distribución espacial Departamentos
D = read.csv("PIB_Depa_Cte.csv")
head(D)
str(D)
summary(D)

X = readOGR(file.choose())
plot(X)
str(X@data)
X@data$DPTO = as.integer(X@data$DPTO)
Dpto = data.frame(DPTO=D$DPTO,Departamento=D$DEPARTAMENTO,X2005=D$X2005/sum(D$X2005),X2006=D$X2006/sum(D$X2006),
X2007=D$X2007/sum(D$X2007),X2008=D$X2008/sum(D$X2008),X2009=D$X2009/sum(D$X2009),X2010=D$X2010/sum(D$X2010),
X2011=D$X2011/sum(D$X2011),X2012=D$X2012/sum(D$X2012),X2013=D$X2013/sum(D$X2013),X2014=D$X2014/sum(D$X2014),
X2015=D$X2015/sum(D$X2015),X2016=D$X2016/sum(D$X2016),X2017=D$X2017/sum(D$X2017),
X2018=D$X2018/sum(D$X2018),X2019=D$X2019/sum(D$X2019),X2020p=D$X2020p/sum(D$X2020p))

SPDF = merge(X, Dpto, by = "DPTO")
head(SPDF@data)

wgs84 = "+proj=longlat +datum=WGS84 +no_defs"
X_wgs84 = spTransform(SPDF,CRS(wgs84))
SS=st_as_sf(SPDF)
at_2006 = lattice::do.breaks(endpoints = c(min(SS$X2006), max(SS$X2006)), nint = 6)
w=mapview(SS, zcol = "X2006", at=at_2006,layer.name = "2006")
at_2010 = lattice::do.breaks(endpoints = c(min(SS$X2010), max(SS$X2010)), nint = 6)
w1=mapview(SS, zcol = "X2010", at=at_2010,layer.name = "2010")
at_2015 = lattice::do.breaks(endpoints = c(min(SS$X2015), max(SS$X2015)), nint = 6)
w2=mapview(SS, zcol = "X2015",at=at_2015,layer.name = "2015")
at_2020 = lattice::do.breaks(endpoints = c(min(SS$X2020p), max(SS$X2020p)), nint = 6)
w3=mapview(SS, zcol = "X2020p",at=at_2020,layer.name = "2020")

u = latticeview(w,w1,w2,w3)
u

spplot(SPDF,"X2015")

### Series departamentos
Año = 2006:2020
Ant = data.frame(Año,PIB=as.numeric(Departa[2,3:17]))
Bogo = data.frame(Año,PIB=as.numeric(Departa[5,3:17]))
Cund = data.frame(Año,PIB=as.numeric(Departa[15,3:17]))
Met = data.frame(Año,PIB=as.numeric(Departa[21,3:17]))
Sant = data.frame(Año,PIB=as.numeric(Departa[28,3:17]))
Vall = data.frame(Año, PIB=as.numeric(Departa[31,3:17]))

Antio = data.frame(Año=as.numeric(Grafico(Ant)$Año),I=as.numeric(unlist(Grafico(Ant)$I)),Dpto = rep("Antioquia"))
Bogta = data.frame(Año=as.numeric(Grafico(Bogo)$Año),I=as.numeric(unlist(Grafico(Bogo)$I)),Dpto = rep("Bogotá"))
Cundi = data.frame(Año=as.numeric(Grafico(Cund)$Año),I=as.numeric(unlist(Grafico(Cund)$I)),Dpto = rep("Cundinamarca"))
Meta = data.frame(Año=as.numeric(Grafico(Met)$Año),I=as.numeric(unlist(Grafico(Met)$I)),Dpto = rep("Meta"))
Santa = data.frame(Año=as.numeric(Grafico(Sant)$Año),I=as.numeric(unlist(Grafico(Sant)$I)),Dpto = rep("Santander"))
Valle = data.frame(Año=as.numeric(Grafico(Vall)$Año),I=as.numeric(unlist(Grafico(Vall)$I)),Dpto = rep("Valle"))

DatosDpto = rbind(Antio,Bogta,Cundi,Meta,Santa,Valle)
head(DatosDpto)
dim(DatosDpto)

serie1 <- ggplot(data=DatosDpto, aes(x=Año, y=I, group = Dpto,colour = Dpto))+
  geom_line(size=1) +
  labs(x= "Año", y= "PIB (Base 100)", title = "Serie Histórica del PIB, 2006 - 2020", 
  caption =  "Fuente: DANE")+
  theme(plot.title=element_text(hjust=0.5,face="bold", color="red",size=rel(2)),
  axis.line = element_line(colour = "grey50", size = 1))

serie1

