setwd("C:/Users/jesus/Desktop/progra apendiendo solo/Exposición/Final_Progra")
getwd()

install.packages("sf")
install.packages("tidyverse")
install.packages("tmap")
install.packages("rgeos")
install.packages("rgdal")
library(sf)
library(tidyverse)
library(tmap)
library(rgeos)
library(rgdal)

pob_vul <- st_read("Poblacion_Vulnerable_mayor_a_60_geogpsperu_SuyoPomalia_931381206.shp")
depart <- readOGR(dsn="DEPARTAMENTOS.shp")

pob_vul2 <- pob_vul[,-c(2:6,8:16,21:33)]
head(pob_vul2) %>%
  view()

names(pob_vul2)[2]="DEPARTAMENTO"
names(pob_vul2)[3]="POB_VULNERABLE"
names(pob_vul2)[4]="POB_TOTAL"
names(pob_vul2)[5]="TOTAL_HOMBRES"
names(pob_vul2)[6]="TOTAL_MUJERES"
names(depart)[2]="DEPARTAMENTO"
head(pob_vul2)
head(depart)

pob_vul2 <- pob_vul2 %>% st_drop_geometry()

pob_vul3 <- pob_vul2 %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise(sum(POB_VULNERABLE),
            sum(POB_TOTAL), 
            sum(TOTAL_HOMBRES), 
            sum(TOTAL_MUJERES))
pob_vul3 <- pob_vul3 %>% 
  cbind(PORCENTAJE = 100*pob_vul3$`sum(POB_VULNERABLE)`/pob_vul3$`sum(POB_TOTAL)`)
head(pob_vul3) %>% 
  view()

mapa <- merge(depart,pob_vul3, by="DEPARTAMENTO")
pobTotal_departa <- qtm(mapa,fill=c("sum(POB_TOTAL)"),
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

pobVulne_departa <- qtm(mapa,fill=c("sum(POB_VULNERABLE)"),
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

pobPocen_depart <- qtm(mapa,fill=c("PORCENTAJE"),
    col = "aquamarine",
    palette = "blue",
    title="Porcentaje de Población Vulnerable por Departamento",
    scale = 0.7 , 
    fill.title="Porcentaje",
    title.font=1,
    sill.style ="fixed",
    title.fontface=3,
    fill.breaks=round(c(seq(0, 17,length.out = 7),Inf),),0)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top")) + 
  tm_borders()+
  tm_text("DEPARTAMENTO", size = 0.6)

pobAgua <- st_read("Porcentaje de viviendas sin abastecimiento de agua potable_geogpsperu_SuyoPomalia_931381206.shp")

pobAgua2 <- pobAgua[,-c(2:10,12:18, 21:28)]
pobAgua2 <- pobAgua2 %>% st_drop_geometry()

names(pobAgua2)[2]="DEPARTAMENTO"

pobAgua2 <- pobAgua2 %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise(sum(VIV_TOTAL),
            sum(AGUA_RED))

pobAgua2 <- pobAgua2 %>% 
  cbind(Viviendas_Sin_Agua = awa2$`sum(VIV_TOTAL)`-awa2$`sum(AGUA_RED)`)

mapa2 <- merge(depart, pobAgua2, by="DEPARTAMENTO")

pobAgua_departamento <- qtm(mapa2,fill=c("Viviendas_Sin_Agua"),
                            col = "aquamarine",
                            title="Viviendas sin agua potable por departamento",
                            scale = 0.7 , 
                            fill.title="Número de viviendas",
                            title.font=1,
                            sill.style ="fixed",
                            title.fontface=3,
                            fill.breaks=round(c(seq(0, 500000,length.out = 7),Inf),),0)+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_scale_bar(position = c("center","bottom"))+
  tm_graticules()+
  tm_compass(position = c("left","top")) + 
  tm_borders()+
  tm_text("DEPARTAMENTO", size = 0.6)



