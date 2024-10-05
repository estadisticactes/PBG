# D. INDUSTRIA MANUFACTURERA

# Librerías
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)
library(zoo)
library(janitor)
library(lubridate)

# Limpiamos memoria
rm(list=ls())

# Cargamos los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx",startRow = 5))
base_INDEC <- base_INDEC[letra=="D",][,-1]

# Ajustamos el año base de las 3 actividades industriales principales (basado en EIM)
base_INDEC[, coef:= VAB/VBP]
base_INDEC[1,5] <- 216261.00 # VAB (15)
base_INDEC[3,5] <- 97006.20  # VAB (17)
base_INDEC[6,5] <- 103770.68 # VAB (20)

# Ajustamos los montos de VBP y CI
base_INDEC[, VBP:= ifelse(cod_act %in% c("15","17","20"), VAB/coef, VBP)]
base_INDEC[, CI:= ifelse(cod_act %in% c("15","17","20"), VBP-VAB, CI)]
base_INDEC$coef = NULL

# Cargamos las listas de empresas con cuit para cruzar
cuits_eim <- as.data.table(read.xlsx("Letra D/Empresas.xlsx", sheet = "Empresas EIM"))
cuits_ipicorr <- as.data.table(read.xlsx("Letra D/Empresas.xlsx", sheet = "Empresas IPICORR"))

# Cargamos la EIM y cruzamos para asignar los cuits
eim <- as.data.table(read.xlsx("Letra D/EIM 2004-2022.xlsx"))
eim <- merge.data.table(eim,cuits_eim, by = c("nlocal","razon","lrama"),all.x=T)
eim[, periodo:= ymd(paste0(anio,"-",mes,"-01"))]
eim <- eim[, .(nlocal, razon, lrama, cuit, periodo, serie_c=m59)]
eim[, periodo:= as.yearqtr(periodo)]
eim[, cod_act:= substr(lrama,1,2)]

# Dividimos en dos períodos para quitar lo que figura en ipicorr y no duplicar posteriormente
eim1 <- eim[periodo<="2020 Q4"]              # Período con EIM
eim2 <- eim[periodo>="2020 Q4"]                # Período con EIM + IPICORR
eim2 <- eim2[!(cuit %in% cuits_ipicorr$cuit),] 

# Cargamos los datos del IPI e IVF de nación
ipi <- read.xlsx("Letra D/Bases_D.xlsx", sheet="IPI Nación", startRow = 4)[,-c(1,3)]
ipi <- as.data.table(pivot_longer(ipi, cols = 2:ncol(ipi), names_to = "periodo", values_to = "ipi"))
ivf <- read.xlsx("Letra D/Bases_D.xlsx", sheet="IVF Nación", startRow = 4)[,-c(1,3)]
ivf <- as.data.table(pivot_longer(ivf, cols = 2:ncol(ivf), names_to = "periodo", values_to = "ivf"))
indices <- merge.data.table(ipi, ivf, by = c("periodo","cod_act"))
indices[, periodo:= as.yearqtr(periodo)]
indices <- indices[, .(ipi=mean(ipi), ivf=mean(ivf)), by = .(periodo,cod_act)]
rm(ivf, ipi)

# Cruzamos con la EIM para deflactar por sector
# EIM Período 1
eim1 <- merge.data.table(eim1, indices, by=c("periodo","cod_act"), all.x = T)
eim1[, serie_k:=serie_c/ipi*100]
eim1 <- eim1[, .(serie_k=sum(serie_k,na.rm=T), serie_c=sum(serie_c,na.rm=T)), by = .(periodo,cod_act)]

eim1[, indice_c:= serie_c/mean(serie_c[lubridate::year( periodo)==2004]), by = .(cod_act)] # indice corriente

eim1[, indice_k:= serie_k/mean(serie_k[lubridate::year(periodo)==2004]), by = .(cod_act)] # indice constante

# EIM Período 2
eim2 <- merge.data.table(eim2, indices, by = c("periodo","cod_act"), all.x = T)
eim2[, ipi:= ipi/ipi[periodo=="2020 Q4"], by = .(cod_act,cuit)]
eim2[, serie_k:= serie_c/ipi]
eim2 <- eim2[, .(serie_k=sum(serie_k,na.rm=T), serie_c=sum(serie_c,na.rm=T)), by = .(periodo,cod_act)]

# IPICORR (Septiembre 2020 en adelante) ###########
# Cantidades
# Le damos formato a la base para poder trabajar
ipicorr <- read.xlsx("Letra D/Cálculo IVF por SAE.xlsx", sheet = "2.b Cantidades Final")
ipicorr <- t(ipicorr)
colnames(ipicorr) <- ipicorr[1,]
row.names(ipicorr) <- 1:nrow(ipicorr)

ipicorr <- as.data.table(ipicorr[-1,-1])
ipicorr <- ipicorr %>% 
  mutate(Empresa= ifelse(is.na(Empresa), na.locf(Empresa, na.rm = T), Empresa)) %>% 
  mutate(Empresa= trimws(Empresa)) %>% 
  pivot_longer(cols = 4:ncol(ipicorr), names_to = "periodo", values_to="cantidad") %>% 
  mutate(periodo=excel_numeric_to_date(as.numeric(periodo)),
         cantidad=as.numeric(cantidad)) %>% 
  mutate(periodo= ymd(paste0(year(periodo),"-",month(periodo), "-01")))
ipicorr <- as.data.table(ipicorr)

# Precios del año base
precio_base <- read.xlsx("Letra D/Cálculo IVF por SAE.xlsx", sheet = "2.a Precios Final")
precio_base <- t(precio_base[2:5,])
colnames(precio_base) <- c(precio_base[1,1:3],"precio_base")
row.names(precio_base) <- 1:nrow(precio_base)
precio_base <- as.data.table(precio_base[-1,-3])
precio_base <- precio_base %>% 
  mutate(Empresa= ifelse(is.na(Empresa), na.locf(Empresa,na.rm=T), Empresa)) %>% 
  mutate(Empresa= trimws(Empresa), precio_base=as.numeric(precio_base)) %>% 
  mutate(precio_base= case_when(Empresa=="Zeni" & Productos=="Molduras" ~ 14847,
                                Empresa=="Zeni" & Productos=="Tableros" ~ 14847,
                                Empresa=="Zeni" & Productos=="Pellets" ~ 7424,
                                TRUE ~ precio_base)) %>% 
  mutate(Productos=ifelse(Productos=="Pisingallo pack 20x400gr", "Pisingallo pack 20x400 gr", Productos)) %>% 
  mutate(Productos=ifelse(Productos=="Tejidos de algodón ", "Tejidos de algodón",Productos))

# Cruzamos para asignar los precios base
ipicorr <- merge.data.table(ipicorr, precio_base, by = c("Empresa","Productos"),all.x=T)
ipicorr[, serie_k:= cantidad*precio_base]

# Asignamos las subclasificaciones y los cuits a las empresas
ipicorr <- merge.data.table(ipicorr, cuits_ipicorr, by.x = "Empresa", by.y="razon", all.x=T)
ipicorr[, periodo:= as.yearqtr(periodo)]
ipicorr[, cod_act:= substr(as.character(lrama),1,2)]
ipicorr <- ipicorr[periodo>"2020 Q3", .(serie_k=sum(serie_k,na.rm=T)), by = .(periodo, cod_act)]

# Cruzamos con el IPI para hallar la serie a precios corrientes
ipicorr <- merge.data.table(ipicorr,indices[,-4],by = c("periodo","cod_act"), all.x=T)
ipicorr[, ipi:= ipi/ipi[periodo=="2020 Q4"], by = .(cod_act)]
ipicorr[, serie_c:= serie_k*ipi]
ipicorr <- ipicorr[, .(periodo,cod_act,serie_k,serie_c)]

# Unimos las bases de la EIM y el IPICORR para el período del 2020 Q4 en adelante
periodo2 <- rbindlist(list(ipicorr,eim2), use.names = T, fill = T)
periodo2 <- periodo2[, .(serie_k=sum(serie_k,na.rm=T), serie_c=sum(serie_c,na.rm=T)), 
                     by = .(periodo,cod_act)]

# Calculamos los índices para empalmar
periodo2[, ':=' (indice_k=serie_k/serie_k[periodo=="2020 Q4"],
                 indice_c=serie_c/serie_c[periodo=="2020 Q4"]),
         by = .(cod_act)]
periodo2 <- periodo2[, .(periodo,cod_act,indice_k,indice_c)]

# Creamos una base de período 1
periodo1 <- eim1[, .(periodo,cod_act,indice_k,indice_c)]

# Unimos las bases
base_empalm <- merge.data.table(periodo1,periodo2,by=c("periodo","cod_act"),all=T)
base_empalm <- base_empalm[!(cod_act %in% c("16","26","28")),]
base_empalm[, ':=' (indice_c=ifelse(is.na(indice_c.x), 
                                   indice_c.x[periodo=="2020 Q4"]*indice_c.y,
                                   indice_c.x),
                    indice_k=ifelse(is.na(indice_k.x),
                                    indice_k.x[periodo=="2020 Q4"]*indice_k.y,
                                    indice_k.x)), 
            by = .(cod_act)]

div_15_17 <- base_empalm[, .(periodo,cod_act,indice_k,indice_c)]
div_15_17 <- div_15_17[cod_act!="22",]
div_15_17 <- div_15_17[cod_act!="20",]  

# Para las divisiones 26 y 28 movemos con empleo
# A partir de 2020 Q4 movemos 26 y 28 con IPICORR
div_26_28 <- indices[cod_act %in% c("26","28"), 
                     .(indice_k=ivf/100, indice_c=(ivf/100)*(ipi/100)),
                     by = .(periodo,cod_act)]

# Empalme de los índices para completar los casos de la 26 y 28
div_26_28 <- merge.data.table(div_26_28, periodo2[periodo2$cod_act %in% c("26","28")],
                                by = c("periodo","cod_act"),all = T)
div_26_28[, indice_k.x:= ifelse(is.na(indice_k.y)==F,
                                  indice_k.x[periodo=="2020 Q4"]*indice_k.y,
                                  indice_k.x), by = .(cod_act)]
div_26_28[, indice_c.x:= ifelse(is.na(indice_c.y)==F,
                                  indice_c.x[periodo=="2020 Q4"]*indice_c.y,
                                  indice_c.x), by = .(cod_act)]
div_26_28 <- div_26_28[, .(periodo,cod_act,indice_k=indice_k.x,indice_c=indice_c.x)]

# División 16
div_16 <- eim[cod_act %in% c("16","20"),]
div_16 <- merge.data.table(div_16, indices, by=c("periodo","cod_act"), all.x = T)
div_16[, serie_k:=serie_c/ipi*100]
div_16 <- div_16[, .(serie_k=sum(serie_k,na.rm=T), serie_c=sum(serie_c,na.rm=T)), by = .(periodo,cod_act)]

div_16[, indice_c:= serie_c/mean(serie_c[lubridate::year(periodo)==2004]), by = .(cod_act)] # indice corriente

div_16[, indice_k:= serie_k/mean(serie_k[lubridate::year(periodo)==2004]), by = .(cod_act)] # indice constante
div_16 <- div_16[, .(periodo,cod_act,indice_k,indice_c)]

# División 20
#OEDE_Ctes <- fread("Bases/OEDE_Corrientes.csv.gz")
#eliminar los datos en cod_acto con menos de 4 caracteres
#OEDE_Ctes <- OEDE_Ctes[nchar(cod_act)==4,]
#quiero que la columna cod_act tenga solo los primeros dos caracteres
#OEDE_Ctes[, cod_act:= substr(cod_act,1,2)]
#OEDE_Ctes <- OEDE_Ctes[cod_act==20,]
#eliminar columna desc_act
#OEDE_Ctes <- OEDE_Ctes[, desc_act:=NULL]
#eliminar filas con NA
#OEDE_Ctes <- OEDE_Ctes[!is.na(empleo),]
#sumar empleo y hacer promedio de remuner
#OEDE_Ctes <- OEDE_Ctes[, .(empleo=sum(empleo,na.rm=T), remuner=mean(remuner,na.rm=T)), by = .(anio, trimestre, cod_act)]
#calcular la masa_salarial multiplicando empleo y remuner
#OEDE_Ctes <- OEDE_Ctes[, masa_salarial:= empleo*remuner]
#crear una columna periodo con el formato yyyy-qq basado en anio y trimestre
#OEDE_Ctes <- OEDE_Ctes[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]
#eliminar columna anio y trimestre
#OEDE_Ctes <- OEDE_Ctes[, c("anio","trimestre"):=NULL]
#reordenar las columnas en el siguiente orden  "periodo"       "cod_act"       "empleo"        "remuner"       "masa_salarial"
#OEDE_Ctes <- OEDE_Ctes[, .(periodo,cod_act,empleo,remuner,masa_salarial)]
#OEDE_Ctes[, periodo := as.yearqtr(periodo)]
#OEDE_Ctes[, cod_act:= as.numeric (cod_act)]

div_20 <- as.data.table(read.xlsx("Letra D/Bases_D.xlsx", sheet = "Empleo"))
div_20 <- div_20[cod_act=="20",]
div_20[, periodo:= as.yearqtr(periodo)]
div_20[, indice_c:= masa_salarial/mean(masa_salarial[lubridate::year(periodo)==2004]), by = .(cod_act)] # índice corriente
div_20[, indice_k:= empleo/mean(empleo[lubridate::year(periodo)==2004]), by = .(cod_act)]
div_20 <- div_20[, .(periodo,cod_act,indice_k,indice_c)]
div_20 <- merge.data.table(div_20, div_16[cod_act=="20",], by = "periodo")
div_20[, indice_k:= indice_k.x*0.6+indice_k.y*0.4]
div_20[, indice_c:= indice_c.x*0.6+indice_c.y*0.4]
div_20 <- div_20[, .(periodo,cod_act=cod_act.x, indice_k, indice_c)]

# Eliminamos la 20 de la tabla anterior
div_16 <- div_16[cod_act=="16",]

# Unimos las bases de oede y la empalmada con el ipicorr de 24, 26 y 28
div_resto <- indices[!(cod_act %in% c("15","16","17","20","26","28")), 
                     .(indice_k=ivf/100, indice_c=(ivf/100)*(ipi/100)),
                     by = .(periodo,cod_act)]

# Unimos todas las bases de las divisiones para mover el año base 2004
indices <- rbindlist(list(div_15_17,div_20,div_16,div_26_28,div_resto),use.names=T)

# Empalmamos los índices con las variaciones de empleo
empleo <- as.data.table(read.xlsx("Letra D/Bases_D.xlsx", sheet = "Empleo"))
empleo[, periodo:= as.yearqtr(periodo)]
empleo[, indice_c:= masa_salarial/mean(masa_salarial[lubridate::year(periodo)==2004]), by = .(cod_act)]
empleo[, indice_k:= empleo/mean(empleo[lubridate::year(periodo)==2004]), by = .(cod_act)]
empleo[, cod_act:= as.character(cod_act)]
empleo <- empleo[, .(periodo, cod_act, indice_k, indice_c)]


indices <- merge.data.table(indices, empleo, by = c("periodo","cod_act"), all.x=T)
indices[, indice_k:= ifelse(is.na(indice_k.y)|cod_act=="15"|cod_act=="26", indice_k.x, indice_k.x*0.6+indice_k.y*0.4)]
indices[, indice_c:= ifelse(is.na(indice_c.y)|cod_act=="15"|cod_act=="26", indice_c.x, indice_c.x*0.6+indice_c.y*0.4)]
indices <- indices[, .(periodo, cod_act, indice_k, indice_c)]

# Cruzamos con el año base por sector
indices <- merge.data.table(indices,base_INDEC,by="cod_act")

# Calculamos los componentes
letra_D <- indices[, .(VBP_k=VBP*indice_k, VBP_c=VBP*indice_c,
                       CI_k=CI*indice_k, CI_c=CI*indice_c,
                       VAB_k=VAB*indice_k, VAB_c=VAB*indice_c), 
                   by = .(periodo,cod_act,desc_act)]

# Ajustamos la tabla para exportar
letra_D <- pivot_longer(letra_D, cols = 4:9, names_sep = "_", names_to = c("variable","tipo"), values_to = "valor")
letra_D <- pivot_wider(letra_D, names_from = "variable", values_from = "valor")
letra_D <- as.data.table(letra_D)
letra_D[, tipo:= ifelse(tipo=="k", "Precios constantes","Precios corrientes")]

# Guardamos la base de datos final
letra_D <- letra_D[periodo!="2023 Q4",]
letra_D <- letra_D[periodo!="2023 Q3",]




write.xlsx(letra_D, "Letra D/Letra_D5.xlsx")

