# F. Construcción

library(data.table)
library(openxlsx)
library(janitor)
library(zoo)
library(lubridate)
library(ggplot2)
library(tidyverse)

# Año base INDEC
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="F",]
base_INDEC <- data.table(factor=c("Mano de obra","Materiales"),
                         VBP=c(base_INDEC$VBP*0.2225,base_INDEC$VBP*0.7775),
                         CI=c(base_INDEC$CI*0.2225,base_INDEC$CI*0.7775),
                         VAB=c(base_INDEC$VAB*0.2225,base_INDEC$VAB*0.7775))

# Método de estimación del ingreso factorial. Dos componentes:
# (1) Mano de obra
# (2) Materiales

# (1)
# Datos de IERIC: Puestos de trabajo.
puestos <- read.xlsx("Letra F/series_F.xlsx", sheet="1. Mano de obra", startRow = 4)
puestos <- na.omit(as.data.table(puestos[,c(1,2,5:7,10,11)]))
colnames(puestos) <- c("anio","trimestre","ocupados_eph","ing_r","ing_nr", "ocupados_ieric","ing_ieric")
puestos[, ocupados_eph:=as.numeric(ocupados_eph)]
puestos[, ':=' (indice_eph=ocupados_eph/mean(ocupados_eph[anio==2004]),
                indice_ieric=ocupados_ieric/mean(ocupados_ieric[anio==2004]))]
puestos[, indice_constante:= indice_eph*0.5+indice_ieric*0.5]
puestos[, indice_corriente:= ing_ieric/mean(ing_ieric[anio==2004])]
puestos <- puestos[, .(anio,trimestre,indice_constante,indice_corriente,factor="Mano de obra")]


# (2)
# Cemento portland y rentas constante
insumos <- read.xlsx("Letra F/series_F.xlsx", sheet="2. Insumos", startRow = 4)
insumos <- as.data.table(insumos[,c(1,2,3,11,12,13)])
insumos <- na.omit(insumos)
colnames(insumos) <- c("anio","trimestre","cemento","rentas","permisos","icc")
insumos[, ':=' (indice_cemento=cemento/mean(cemento[anio==2004]),
                indice_rentas=rentas/mean(rentas[anio==2004]),
                indice_permisos=permisos/mean(permisos[anio==2004]))]
insumos[, indice_constante:= indice_cemento*0.3+indice_rentas*0.4+indice_permisos*0.3]
insumos[, indice_corriente:= indice_constante*(icc/100)]
insumos <- insumos[, .(anio,trimestre,indice_constante,indice_corriente,factor="Materiales")]

# Índices
letra_F <- rbindlist(list(puestos,insumos),use.names=T)
letra_F <- as.data.table(pivot_longer(letra_F, cols=c(3:4),names_to = "tipo", values_to = "indice"))
letra_F[, tipo:= ifelse(tipo=="indice_constante","Precios constantes", "Precios corrientes")]
letra_F <- merge.data.table(letra_F,base_INDEC,by="factor")
letra_F[, ':=' (VBP=VBP*indice, CI=CI*indice, VAB=VAB*indice)]
letra_F[, ':=' (cod_act="45",desc_act="Construcción")]
letra_F[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]

# Guardamos la base final
letra_F <- letra_F[, .(periodo,cod_act,desc_act,factor,tipo,VBP,CI,VAB)]
write.xlsx(letra_F, "Letra F/Letra_F2.xlsx")
