## E. Electricidad, gas y agua

library(data.table)
library(readxl)
library(tidyverse)
library(zoo)
library(openxlsx)

# Año base #########
base_INDEC <- setDT(read_excel("Bases/Base_2004_INDEC.xlsx", skip = 4))[,c(1:6)]
base_INDEC <- base_INDEC[letra=="E",]
base_INDEC$cod_act <- as.character(base_INDEC$cod_act)

# 40. Energía eléctrica #############

# Demanda de energía (CAMMESA)
energia_k <- na.omit(read_excel("Letra E/Demanda energía - CAMMESA.xlsx", skip = 3)[,c(4,5,8)])

# IPIM (Energía)
energia_ipim <- na.omit(read_excel("Letra E/IPIM.xlsx")[,c(8:10)])
colnames(energia_ipim) <- c("anio","trimestre","indice_corriente")

# Unimos las tablas de los índices
energia <- merge.data.frame(energia_k, energia_ipim, by = c("anio","trimestre"))
energia <- as.data.table(energia)
energia[, indice_constante:= indice_constante/100]
energia[, indice_corriente:= (indice_corriente/100)*indice_constante]
energia[, cod_act:= "40"]

# Cruzamos con la tabla del año base y reacomodamos
energia <- merge.data.table(energia, base_INDEC, by = "cod_act")
energia <- as.data.table(pivot_longer(energia, cols=4:5, names_to="tipo", values_to = "indice"))

# Aplicamos los índices sobre los componentes
energia[, ':=' (VBP=VBP*indice, CI=CI*indice, VAB=VAB*indice)]
energia[, tipo:= ifelse(tipo=="indice_corriente","Precios corrientes","Precios constantes")]
energia[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]

# Tabla final energía
energia <- energia[, .(anio, periodo, tipo, cod_act, desc_act, indice, VBP, CI, VAB)]
rm(energia_ipim, energia_k)

# 41. Agua #######################

# Serie constante
# Cargamos período, localidad y consumo total
agua_k <- as.data.table(read_excel("Letra E/Nota 020 - Anexo.xlsx", sheet = "Consumo"))[,c(1,3,15)]
# Filtramos las localidades que figuran todo el período
agua_k <- agua_k[UO_UG %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),]
agua_k$UO_UG=NULL 
colnames(agua_k) <- c("periodo","consumo")
agua_k[, periodo:= ymd(paste0(periodo,"-01"))]
# Sumarizamos por período mensual
agua_k <- agua_k[, .(consumo=sum(consumo,na.rm=T)), by = .(periodo)]

# Serie corriente
# Cargamos período, localidad y facturado total
agua_c <- as.data.table(read_excel("Letra E/Nota 020 - Anexo.xlsx", sheet = "Facturado"))[,c(1,2,3)]
agua_c <- agua_c[substr(`Unidad Operativa`,1,2) %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),]
agua_c$`Unidad Operativa`=NULL
colnames(agua_c) <- c("periodo","facturado")
agua_c[, periodo:= ymd(paste0(periodo,"-01"))]
# Sumarizamos por período mensual
agua_c <- agua_c[, .(facturado=sum(facturado,na.rm=T)), by = .(periodo)]

# Unimos ambas bases
agua <- merge.data.table(agua_k,agua_c,by="periodo")
agua[, periodo:= as.yearqtr(periodo)]

# Agregamos por período trimestral
agua <- agua[, .(consumo=sum(consumo,na.rm=T), facturado=sum(facturado,na.rm=T)),
             by = .(periodo)]
# Reordenamos
agua <- as.data.table(pivot_longer(agua, cols = 2:3, names_to = "tipo", values_to = "valor"))
agua[, tipo:= ifelse(tipo=="consumo","Precios constantes", "Precios corrientes")]

# Creamos los índices
agua[, base:= mean(valor[year(periodo)==2004]), by = .(tipo)]
agua[, indice:= valor/base, by = .(tipo)]

# Variable para cruce con el año base
agua[, cod_act:= "41"]
agua <- merge.data.table(agua, base_INDEC, by = "cod_act")

# Aplicamos los índices sobre los componentes
agua[, ':=' (VBP=VBP*indice, CI=CI*indice, VAB=VAB*indice)]
agua[, anio:= year(periodo)]

# Tabla final Agua
agua <- agua[, .(anio, periodo, tipo, cod_act, desc_act, indice, VBP, CI, VAB)]

# Tabla final #########
# Unimos las tablas finales y exportamos
letra_E <- rbindlist(list(energia, agua), use.names=T)
write.xlsx(letra_E, "Letra E/Letra_E.xlsx")
