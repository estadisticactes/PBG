# J. INTERMEDIACIÓN FINANCIERA #

# Librerías
library(data.table)
library(openxlsx)
library(janitor)
library(tidyverse)
library(lubridate)

# Limpiamos memoria
rm(list=ls())

# Divisiones de actividad a dos dígitos:
# 65. Intermediación financiera y otros servicios financieros
# 66. Servicios de seguros
# 67. Servicios auxiliares a la actividad financiera

# CARGA DE LOS DATOS DEL AÑO BASE
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="J",]


#### 65. INTERMEDIACIÓN FINANCIERA Y OTROS SERVICIOS FINANCIEROS ####

# Carga de las distintas series
montos <- setDT(read.xlsx("Letra J/J. Series.xlsx", sheet = "Mensuales"))
montos[, periodo:= excel_numeric_to_date(periodo)]
montos[, ':=' (prest_ext_privpub = prest_ext_priv + prest_ext_pub,
               dep_ext_privpub = dep_ext_priv + dep_ext_pub)]
columnas <- c("prest_ext_priv","prest_ext_pub","dep_ext_priv","dep_ext_pub")
montos[, (columnas) := lapply(.SD, function(x) NULL), .SDcols=columnas]
montos <- setDT(pivot_longer(montos, cols = 2:7, names_to = "variable", values_to = "monto"))
montos[, monto:= monto*1000000] # Pasamos a millones de pesos y dólares

# Agrupamos las series por moneda para luego deflactarla con el índice que corresponda:
montos <- as.data.table(separate(montos, col="variable", into=c("tipo", "moneda", "sector"), sep="_"))
montos[, ':=' (tipo= ifelse(tipo=="prest","Préstamo","Depósito"),
               moneda= ifelse(moneda=="nac", "Nacional", "Extranjera"),
               sector= ifelse(sector=="pub","Público","Privado"))]
montos <- montos[, .(monto=sum(monto,na.rm=T)), by = .(periodo, tipo, moneda, sector)]

# Cargamos tipo de cambio e IPC como deflactores
tipo_cambio <- setDT(read.xlsx("Letra J/J. Series.xlsx", sheet="Tipo de cambio"))
tipo_cambio[, periodo:= excel_numeric_to_date(periodo)]
tipo_cambio[, periodo:= ymd(paste0(year(periodo),"-",month(periodo),"-01"))]
tipo_cambio <- tipo_cambio[, .(tipo_cambio=mean(tipo_cambio)), by = .(periodo)]
tipo_cambio$moneda <- "Extranjera"

# Cruzamos con la tabla de préstamos y depósitos:
int_fin <- merge.data.table(montos, tipo_cambio, by = c("periodo","moneda"),all.x=T)
int_fin[, corriente:= ifelse(moneda=="Extranjera", monto*tipo_cambio, monto)]
int_fin[, ':=' (anio=year(periodo),trimestre=quarter(periodo))]

rm(tipo_cambio, columnas, montos) # limpiamos memoria

# Aplicamos el coeficiente que corresponde a Corrientes por cada serie:
coeficientes <- setDT(read.xlsx("Letra J/J. Series.xlsx", sheet = "Trimestrales"))
columnas <- names(coeficientes)[3:8]
coeficientes[, (columnas) := lapply(.SD, function(x) x=x/100), .SDcols= columnas]
coeficientes <- setDT(pivot_longer(coeficientes, cols = 3:8, names_to = "variable", values_to = "coeficiente"))
coeficientes <- separate(coeficientes, col="variable", into=c("tipo", "moneda", "sector"), sep="_")
coeficientes <- as.data.table(coeficientes)
coeficientes[, ':=' (tipo= ifelse(tipo=="prest","Préstamo","Depósito"),
                     moneda= ifelse(moneda=="nac", "Nacional", "Extranjera"),
                     sector= ifelse(sector=="pub","Público","Privado"))]

int_fin <- merge.data.table(int_fin, coeficientes, by = c("anio", "trimestre","tipo","moneda","sector"))
int_fin[, corriente:= corriente*coeficiente]

# Cargamos la base del IPC para deflactar
ipc <- setDT(read.xlsx("Bases/IPC NEA.xlsx", sheet = "IPC Finales"))[,1:2]
colnames(ipc) <- c("periodo", "ipc")
ipc[, periodo:= excel_numeric_to_date(periodo)]

# Cruzamos con la base de la división 65.
int_fin <- setDT(merge.data.table(int_fin, ipc, by = "periodo"))
int_fin[, constante:= corriente/ipc*100]
int_fin <- int_fin[, .(corriente=mean(corriente), constante=mean(constante)), by = .(anio, trimestre)]
int_fin <- as.data.table(pivot_longer(int_fin, 3:4, names_to="tipo", values_to = "serie"))

int_fin[, base:=mean(serie[anio==2004]), by = "tipo"]
int_fin[, indice:= serie/base, by = "tipo"]

d65 <- int_fin[, .(anio, trimestre, cod_act="65", tipo, indice)]

#### 66. SERVICIOS DE SEGUROS ####
seguros <- read.xlsx("Letra J/J. Series.xlsx", sheet = "Seguros")
VAB <- read.xlsx("Letra J/J. Series.xlsx", sheet = "VAB Nacion")
seguros <- merge.data.table(seguros, VAB[,c(1,2,4,7)], by = "anio")
seguros <- pivot_longer(seguros, cols=4:5, names_to = "variable", values_to = "VAB")
seguros <- setDT(separate(seguros, col="variable", into=c("cod_act","tipo"), sep="_"))
seguros[, VAB:= VAB*coef_provctes]
bases <- seguros[anio==2004, .(base=mean(VAB)), by = .(cod_act, tipo)]
seguros <- merge.data.table(seguros, bases, by = c("cod_act","tipo"))
seguros[, indice:= VAB/base, by = .(cod_act, tipo)]

# Guardamos la tabla de la división 66:
d66 <- seguros[, .(anio, trimestre, cod_act, tipo, indice)]


#### 67. SERVICIOS AUXILIARES DE LA ACTIVIDAD FINANCIERA ####
aux <- read.xlsx("Letra J/J. Series.xlsx", sheet = "Seguros")
VAB <- read.xlsx("Letra J/J. Series.xlsx", sheet = "VAB Nacion")
aux <- merge.data.table(aux, VAB[,c(1,2,5,8)], by = "anio")
aux <- pivot_longer(aux, cols=4:5, names_to = "variable", values_to = "VAB")
aux <- setDT(separate(aux, col="variable", into=c("cod_act","tipo"), sep="_"))
aux[, VAB:= VAB*coef_provctes]
aux[, base:=mean(VAB[anio==2004]), by = c("cod_act","tipo")]
aux[, indice:= VAB/base, by = .(cod_act, tipo)]

# Guardamos la tabla final de la división 67
d67 <- aux[, .(anio, trimestre, cod_act, tipo, indice)]

## Anexamos todas las divisiones
Letra_J <- rbindlist(list(d65,d66,d67),use.names=T)
Letra_J <- as.data.table(merge.data.frame(Letra_J, base_INDEC[,-1],by="cod_act"))
Letra_J[, ':=' (VAB=indice*VAB, CI=indice*CI, VBP=indice*VBP)]
Letra_J[, tipo:= ifelse(tipo=="corriente","Precios corrientes","Precios constantes")]
Letra_J[, periodo:= paste0(anio,"0",trimestre)]
Letra_J <- Letra_J[, .(anio, trimestre, periodo, tipo, cod_act, desc_act, VBP, CI, VAB)]

# Guardamos la tabla final de la letra
write.xlsx(Letra_J, "Letra J/Letra_J1.xlsx")
