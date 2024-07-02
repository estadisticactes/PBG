# P. SERVICIO DOMÉSTICO #

library(data.table)
library(openxlsx)
library(eph)
library(tempdisagg)
library(dplyr)
library(tidyr)

rm(list=ls())

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="P",]

# Serie EPH histórica 2004 1T a 2021 4T
EPH_hist <- read.xlsx("Letra P/Series_P.xlsx", sheet="EPH_Svdomestico")

# Carga de la EPH 2022
EPH <- get_microdata(year = 2022, trimester = 4, type="individual")
EPH <- as.data.table(EPH)
EPH <- EPH[ESTADO==1 & AGLOMERADO==12,]
EPH[, P21:= ifelse(P21==-9, NA, P21)]
EPH[, P21:= ifelse(is.na(PONDIIO)==T,P21,P21*PONDIIO)]
EPH[, PP04B1:= ifelse(PP04B1==1, 1, 0)]
EPH[, ocupados_svdom:= PP04B1*PONDERA]
EPH[, salarios_svdom:= ifelse(ANO4>=2016, PP04B1*P21, PP04B1*PONDERA*P21)]
EPH <- EPH[, .(ocupados_svdom=sum(ocupados_svdom, na.rm=T), 
               ocupados_total=sum(PONDERA, na.rm=T),
               salarios_svdom=sum(salarios_svdom, na.rm=T),
               salarios_total=sum(ifelse(ANO4>=2016, P21, P21*PONDERA), na.rm=T)), 
           by = .(anio=ANO4, trimestre=TRIMESTRE)]
setorder(EPH, anio, trimestre)
# Carga de la EPH 2023
EPH1 <- get_microdata(year = 2023, trimester = 1, type="individual")
EPH1 <- as.data.table(EPH1)
EPH1 <- EPH1[ESTADO==1 & AGLOMERADO==12,]
EPH1[, P21:= ifelse(P21==-9, NA, P21)]
EPH1[, P21:= ifelse(is.na(PONDIIO)==T,P21,P21*PONDIIO)]
EPH1[, PP04B1:= ifelse(PP04B1==1, 1, 0)]
EPH1[, ocupados_svdom:= PP04B1*PONDERA]
EPH1[, salarios_svdom:= ifelse(ANO4>=2016, PP04B1*P21, PP04B1*PONDERA*P21)]
EPH1 <- EPH1[, .(ocupados_svdom=sum(ocupados_svdom, na.rm=T), 
               ocupados_total=sum(PONDERA, na.rm=T),
               salarios_svdom=sum(salarios_svdom, na.rm=T),
               salarios_total=sum(ifelse(ANO4>=2016, P21, P21*PONDERA), na.rm=T)), 
           by = .(anio=ANO4, trimestre=TRIMESTRE)]
setorder(EPH1, anio, trimestre)

# Carga de la EPH 2023
EPH2 <- get_microdata(year = 2023, trimester = 2, type="individual")
EPH2 <- as.data.table(EPH2)
EPH2 <- EPH2[ESTADO==1 & AGLOMERADO==12,]
EPH2[, P21:= ifelse(P21==-9, NA, P21)]
EPH2[, P21:= ifelse(is.na(PONDIIO)==T,P21,P21*PONDIIO)]
EPH2[, PP04B1:= ifelse(PP04B1==1, 1, 0)]
EPH2[, ocupados_svdom:= PP04B1*PONDERA]
EPH2[, salarios_svdom:= ifelse(ANO4>=2016, PP04B1*P21, PP04B1*PONDERA*P21)]
EPH2 <- EPH2[, .(ocupados_svdom=sum(ocupados_svdom, na.rm=T), 
                 ocupados_total=sum(PONDERA, na.rm=T),
                 salarios_svdom=sum(salarios_svdom, na.rm=T),
                 salarios_total=sum(ifelse(ANO4>=2016, P21, P21*PONDERA), na.rm=T)), 
             by = .(anio=ANO4, trimestre=TRIMESTRE)]
setorder(EPH2, anio, trimestre)




# Unimos las tablas

EPH <- rbindlist(list(EPH_hist, EPH, EPH1, EPH2), use.names = T)


# Cargamos la serie de población en provincia y aglomerado Ctes en base a los datos censales
poblacion <- setDT(read.xlsx("Letra P/Series_P.xlsx", sheet="Población", startRow = 2, cols = c(1,4,5)))
poblacion <- poblacion[anio>=2004,]

# Trimestralizamos las series anuales con Denton
provincia <- ts(poblacion$provincia, start = 2004)
aglomerado <- ts(poblacion$aglomerado, start=2004)
provincia <- round(predict(td(provincia ~ 1, to="quarterly"))*4,0)
aglomerado <- round(predict(td(aglomerado~1, to="quarterly"))*4,0)

poblacion <- data.table(anio=rep(2004:2023,each=4),
                        trimestre=rep(1:4,length(2004:2013)),
                        provincia=c(provincia),
                        aglomerado=c(aglomerado))

# Calculamos la proporción del aglomerado sobre la provincia
poblacion[, proporcion:= aglomerado/provincia]

# Cruzamos con la base de la EPH
EPH <- merge.data.table(EPH, poblacion, by = c("anio", "trimestre"), all.x = T)

# SERIE A PRECIOS CONSTANTES
dato_censo2010 <- 30084 # Población ocupada en servicio doméstico para Ctes provincia (CNP 2010)
prop <- mean(EPH$ocupados_svdom[EPH$anio==2010])/dato_censo2010 # Proporción Aglomerado Ctes/Provincia para Sv doméstico
serie <- EPH

serie[, prop_svdom:= ifelse(anio==2010, proporcion/mean(proporcion[anio==2010]), NA)]
serie[, prop_svdom:= ifelse(anio==2010, prop_svdom*prop, NA)]
serie[, prop_svdom:= case_when(anio<2010 ~ prop_svdom[anio==2010&trimestre==1]*(proporcion[anio==2010&trimestre==1]/proporcion),
                               anio>2010 ~ prop_svdom[anio==2010&trimestre==4]*(proporcion[anio==2010&trimestre==4]/proporcion),
                               TRUE ~ prop_svdom)] # Empalme proporción Aglomerado Ctes/Provincia para Sv doméstico

# La serie de ocupados en sv doméstico a nivel provincial se mueve por dos índices:
# - Según como evoluciona la proporción del aglomerado Corrientes en la provincia.
# - Según como evoluciona la serie de ocupados en sv doméstivo en el aglomerado Corrientes.

serie[, indice1:= prop_svdom/mean(prop_svdom[anio==2010])]
serie[, indice2:= ocupados_svdom/mean(ocupados_svdom[anio==2010])]
serie[, serie_k:= dato_censo2010*indice1*indice2]

serie <- serie[, .(anio, trimestre, serie_k)]

# SERIE A PRECIOS CORRIENTES
EPH[, salario_prom_svdom:= salarios_svdom/ocupados_svdom]
serie <- merge.data.table(serie, EPH[,c("anio","trimestre","salario_prom_svdom")],
                          by = c("anio", "trimestre"))
serie[, serie_c:= serie_k*salario_prom_svdom]
serie <- serie[, .(anio, trimestre, serie_c, serie_k)]
serie <- setDT(pivot_longer(serie, cols = 3:4, names_to = "tipo", values_to = "indice"))
serie[, tipo:= ifelse(tipo=="serie_k", "Precios constantes", "Precios corrientes")]
serie[, indice:= indice/mean(indice[anio==2004]), by = tipo]

# Aplicamos los índices sobre los datos del año base:
serie[, ':=' (VBP=base_INDEC$VBP*indice, CI=base_INDEC$CI*indice, VAB=base_INDEC$VAB*indice)]

# Guardamos la base final de la letra P
Letra_P <- serie[, .(anio, trimestre, periodo=paste0(anio,"0",trimestre), tipo, cod_act="95",
                     desc_act = "Servicios de hogares privados que contratan servicio doméstico",
                     VBP, CI, VAB)]
write.xlsx(Letra_P, "Letra P/Letra_P2.xlsx")
