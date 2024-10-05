# I. TRANSPORTE Y COMUNICACIONES

library(openxlsx)
library(data.table)
library(tidyverse)
library(zoo)
library(janitor)

######## TRANSPORTE ########

# 60. Servicios de transporte terrestre (ferroviario,automotor,tuberías)
# 61. Servicios de transporte vía acuática
# 62. Servicio de transporte aéreo
# 63. Servicios anexos al transporte; servicios de agencia de viaje

###### COMUNICACIONES ######

# 641. Servicios de correos
# 642. Servicios de transmisión de radio y televisión
# 643. Servicios de telecomunicaciones 
# 644. Servicios de internet

# Cargamos las series finales para el cálculo de la letra
series <- read.xlsx("Letra I/Bases I.xlsx", sheet = "Series finales",startRow = 2)
series <- pivot_longer(series, cols=3:ncol(series),names_to = "division", values_to = "serie")
series <- setDT(separate(series, col = "division", into = c("cod_act","tipo")))

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx",startRow = 5))
base_INDEC <- base_INDEC[letra=="I",][,c(3:6)]

# Ponderamos con los datos del CNE04
cne04 <- setDT(read.xlsx("Bases/CNE04_Corrientes.xlsx", sheet = "CNE_04", startRow = 7, na.strings = "sd"))
cne04 <- cne04[letra=="I", .(cod_act,VBP_pbasicos)]

# Valuamos las divisiones que nos faltan para poder ponderar
pond <- series[anio==2004 & cod_act %in% c(601,602,620) & tipo=="c", .(VBP_pbasicos=sum(serie)/1000), by = .(cod_act)]
pond <- rbind(pond,cne04)
pond[, desc_act:= ifelse(cod_act %in% c(601,602,620,630), "TRANSPORTE", "COMUNICACIONES")]
pond[, totales:= sum(VBP_pbasicos), by = .(desc_act)]
pond[, part:= VBP_pbasicos/totales]

base <- merge.data.table(base_INDEC, pond, by = "desc_act")
base <- base[, .(cod_act, VBP=VBP*part,CI=CI*part,VAB=VAB*part)]

# Calculamos los índices para mover los montos del año base
series[, indice:=(serie/mean(serie[anio==2004])*100), by = .(cod_act,tipo)]

# Guardamos IPI e IVF para cada serie
indices <- series[tipo %in% c("k","precio"), .(anio,trimestre,cod_act,tipo,indice)]
indices[, tipo:= ifelse(tipo=="k", "IVF", "IPI")]
indices <- pivot_wider(indices, names_from = "tipo", values_from = "indice")

series <- series[tipo %in% c("k","c")]
series <- merge.data.table(series,base,by="cod_act")
series[, ':=' (VBP=VBP*indice/100, CI=CI*indice/100, VAB=VAB*indice/100)]
series[, tipo:= ifelse(tipo=="k","Precios constantes", "Precios corrientes")]
series[, desc_act:= case_when(cod_act==601 ~ "Servicios de transporte terrestre ferroviario",
                             cod_act==602 ~ "Servicios de transporte terrestre automotor",
                             cod_act==620 ~ "Servicio de transporte aéreo",
                             cod_act==630 ~ "Servicios anexos al transporte; servicios de agencia de viaje",
                             cod_act==641 ~ "Servicios de correos",
                             cod_act==642 ~ "Servicios de transmisión de radio y televisión",
                             cod_act==643 ~ "Servicios de  telecomunicaciones",
                             cod_act==644 ~ "Servicios de internet")]
series <- merge.data.table(series,indices,by=c("anio","trimestre","cod_act"))
Letra_I <- series[, .(anio,trimestre,periodo=as.yearqtr(paste0(anio,"-",trimestre)),
                     tipo,cod_act,desc_act,VBP,CI,VAB,IVF,IPI)]


write.xlsx(Letra_I, "Letra I/Letra_I1.xlsx")
