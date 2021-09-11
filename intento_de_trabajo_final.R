setwd("C:/Users/jesus/Desktop/progra apendiendo solo/Exposición/Final_Progra")
getwd()
list.files()
#librerías

library(leaflet)
library(sf)
library(tidyverse)
library(tmap)
library(rgeos)
library(rgdal)
install.packages("tmap")
install.packages("rgeos")
pob_vul <- st_read("Poblacion_Vulnerable_mayor_a_60_geogpsperu_SuyoPomalia_931381206.shp")
names(pob_vul)

drop()
install.packages("tmap")
primeros <- head(pob_vul)
view(primeros)

primeros %>%  drop("whatsapp",)

pob_vul2 <- pob_vul[,-c(31,32,33)]
segundos <- head(pob_vul2)
view(segundos)

head(pob_vul) %>% view()

pocos <- pob_vul[1:10000,]
nombre3 <- pob_vul$NOMPROV 
nombre2 <- pob_vul$NOMDEP
proyecto <- pob_vul %>% 
  group_by(NOMDEP) %>% 
  summarise(frequency=n())

pob_vul %>% group_by(NOMDEP)
names(pob_vul)


head(datos_servibles) %>% view()
 
factor(datos_servibles$NOMDEP)
prov <- readOGR(dsn="DEPARTAMENTOS.shp")
datos_servibles <- pob_vul[,-c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,21,22,23,24,25,26,27,28,29,30,31,32,33)]

names(datos_servibles)[2]="DEPARTAMENTO"
names(datos_servibles)[3]="POB_VULNERABLE"
names(datos_servibles)[4]="POB_TOTAL"
names(datos_servibles)[5]="TOTAL_HOMBRES"
names(datos_servibles)[6]="TOTAL_MUJERES"
names(prov)[2]="DEPARTAMENTO"

head(prov)
head(datos_servibles)

ojala <- datos_servibles %>% st_drop_geometry()
ojala2 <- ojala %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise(sum(POB_VULNERABLE),
            sum(POB_TOTAL), 
            sum(TOTAL_HOMBRES), 
            sum(TOTAL_MUJERES)) %>% 
  cbind(PORCENTAJE = 100*ojala2$`sum(POB_VULNERABLE)`/ojala2$`sum(POB_TOTAL)`)

mapa <- merge(prov,ojala2, by="DEPARTAMENTO")
plot(mapa)
qtm(mapa)
qtm(mapa, fill = c("PORCENTAJE"), col = "AQUARINE")
order(ojala2$`sum(POB_VULNERABLE)`, decreasing = T)



view(mapa)
max(ojala2$`sum(POB_VULNERABLE)`)
min(ojala2$`sum(POB_VULNERABLE)`)

view(ojala2$`sum(POB_VULNERABLE)`)
b <- ojala2$`sum(POB_TOTAL)`

max(ojala2$`sum(POB_TOTAL)`)
min(ojala2$`sum(POB_TOTAL)`)

sort(ojala2$`sum(POB_TOTAL)`)

ab <-qtm(mapa,fill=c("sum(POB_TOTAL)"),
    col = "aquamarine",
    title="Población Total por Departamento",
    scale = 0.7 , 
    fill.title="Población",
    title.font=1,
    sill.style ="fixed",
    title.fontface=3,
    fill.breaks=round(c(seq(0, 1900000,length.out = 7),Inf),),0)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top")) + 
  tm_borders()+
  tm_text("DEPARTAMENTO", size = 0.6)

bc <- qtm(mapa,fill=c("sum(POB_VULNERABLE)"),
    col = "aquamarine",
    palette = "blue",
    title="Población Vulnerable por Departamento",
    scale = 0.7 , 
    fill.title="Población",
    title.font=1,
    sill.style ="fixed",
    title.fontface=3,
    fill.breaks=round(c(seq(0, 300000,length.out = 7),Inf),),0)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top")) + 
  tm_borders()+
  tm_text("DEPARTAMENTO", size = 0.6)
max(ojala2$PORCENTAJE)
min(ojala2$PORCENTAJE)

plot(ab,bc)

awa <- st_read("Porcentaje de viviendas sin abastecimiento de agua potable_geogpsperu_SuyoPomalia_931381206.shp")
head(awa) %>% view()
awa[1:20] %>% view()
a <- 2
awa %>% st_drop_geometry()
