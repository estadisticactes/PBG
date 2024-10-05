## K. Actividades inmobiliarias, empresariales y de alquiler

# 700. Servicios inmobiliarios
# 710. Alquiler de maquinaria y equipos 
# 720. Servicios informáticos y actividades conexas 
# 730. Investigación y desarrollo
# 740. Servicios empresariales n.c.p. 

library(openxlsx)
library(readxl)
library(data.table)
library(tempdisagg)
library(lubridate)
library(zoo)
library(forecast)
library(tidyr)
library(dplyr)

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="K",]

## División 730	Investigación y desarrollo ##

# Parte corriente
# voy a usar una serie de inta e inti representan gastos nacionales
##cargo la serie
Serie730 <- data.table(read_excel("Letra K/Serie presupuesto INTA - INTI.xlsx", sheet = "Hoja 1", range = "A1:B100"))
##Ahora elimino las filas con NA
Serie730 <- na.omit(Serie730)
##Ahora la voy a trimestralizar con interpolacion
# hago la interpolacion y sigo ordenando la tabla
serie <- td(Serie730$Serie ~ 1, to = 4, method = "denton-cholette")
serie <- c(serie$values)
fechas2 <- seq(as.Date("2004-01-01"), by = "3 month", length.out = length(Serie730$Serie)*4)
fechas <- paste0(format(fechas2, format = "%Y"), " ", quarters(seq(as.Date("2004-01-01"), by = "3 month", length.out = length(Serie730$Serie)*4)))
Serie730 <- data.table(anio = year(fechas2), 
                       trimestre = quarter(fechas2),
                       periodo = as.yearqtr(fechas),
                       tipo = "Precios corrientes",
                       serie = serie,
                       desc_act = "Resto")

# Parte constante
# Deflactamos con el índice de salarios de INDEC
deflactor <- setDT(read_excel("Letra K/Serie presupuesto INTA - INTI.xlsx", sheet = "Deflactor", skip = 1))
deflactor[, periodo:= as.yearqtr(paste0(year(periodo),"-",quarter(periodo)))]
deflactor <- deflactor[, .(indice_salarios=mean(indice_salarios)), by = .(periodo)]
Serie730 <- merge.data.table(Serie730,deflactor,by="periodo")
Serie730[, serie.x:= serie/indice_salarios*100]

# Anexamos la serie a precios constantes a la base
Serie730 <- rbind(Serie730[,c(1:6)],                       # Tabla a precios corrientes
                  data.table(periodo = as.yearqtr(fechas),
                             anio = year(fechas2), 
                             trimestre = quarter(fechas2),
                             tipo = "Precios constantes",
                             serie = c(Serie730$serie.x),
                             desc_act = "Resto"))         # Tabla a precios constantes
# Eliminamos el último dato
Serie730 <- Serie730[periodo!="2023 Q4",]

# Dividimos la serie constante por el salario promedio en 2004 para hallar un aproximado de empleo
Serie730[, serie:= ifelse(tipo=="Precios constantes", round(serie/8559), serie)]


## Divisiones 710, 720 y 740 ####
# OEDE Empleo para la parte constante
# OEDE Masa salarial para la parte corriente
OEDE_Ctes <- as.data.table(read.xlsx("Letra K/Empleo OEDE.xlsx", sheet = "Base"))
OEDE_Ctes[, periodo:= as.yearqtr(periodo)]


# Calculamos masa salarial y pivoteamos la tabla
OEDE_Ctes <- OEDE_Ctes[, .(anio=year(periodo), trimestre=quarter(periodo), periodo, cod_act, empleo, masa_salarial=empleo*remuner)]
OEDE_Ctes <- setDT(pivot_longer(OEDE_Ctes, cols = 5:6, names_to = "tipo", values_to = "serie"))
OEDE_Ctes[, tipo:= ifelse(tipo=="empleo", "Precios constantes", "Precios corrientes")]
OEDE_Ctes[, desc_act:= "Resto"]
Serie_710_720_740 <- OEDE_Ctes[, .(serie=sum(serie)), by = .(anio,trimestre,periodo,tipo,desc_act)]

Serie_Resto <- rbind(Serie730,Serie_710_720_740)
Serie_Resto <- Serie_Resto[, .(serie=sum(serie)), by = .(anio,trimestre,periodo,tipo,desc_act)]
 
# Limpiamos memoria
rm(Serie_710_720_740, Serie730, OEDE_Ctes, deflactor)

# División 710 Servicios inmobiliarios y de alquiler ####

# Trimestralizamos la serie de viviendas y la guardamos en "Sv. inmobiliarios.xlsx"
# viviendas <- read.xlsx("Letra K/Sv inmobiliarios.xlsx", sheet="Viviendas", cols = c(1,4))
# viviendas <- ts(viviendas$Viviendas.proyectadas, start=2001, frequency=1)
# viviendas <- td(viviendas ~ 1, to="quarterly", method="denton-cholette")
# viviendas <- as.data.table(predict(viviendas)*4)

viviendas <- setDT(read.xlsx("Letra K/Sv inmobiliarios.xlsx", sheet="Viviendas", cols = c(6:8)))
names(viviendas) <- c("anio","trimestre","viviendas")
viviendas <- viviendas[anio>=2004,]

alquiler <- setDT(read.xlsx("Letra K/Sv inmobiliarios.xlsx", sheet="Alquiler", cols = c(5:7)))
Serie_Vivienda <- merge.data.table(viviendas,alquiler,by=c("anio","trimestre"))
Serie_Vivienda <- Serie_Vivienda[, .(anio, trimestre, periodo=as.yearqtr(paste0(anio,"-",trimestre)),
                                     constante=viviendas, corriente=viviendas*Alquiler)]
Serie_Vivienda <- pivot_longer(Serie_Vivienda, cols = 4:5, names_to = "tipo", values_to = "serie")
Serie_Vivienda <- as.data.table(Serie_Vivienda)
Serie_Vivienda[, tipo:= ifelse(tipo=="constante", "Precios constantes", "Precios corrientes")]
Serie_Vivienda[, desc_act:= "Propiedad de la vivienda"]

# Unimos las tablas
Letra_K <- rbindlist(list(Serie_Vivienda, Serie_Resto), use.names=T)
Letra_K[, indice:= serie/mean(serie[year(periodo)==2004]), by = .(tipo,desc_act)]
Letra_K <- merge.data.table(Letra_K, base_INDEC, by="desc_act")
Letra_K[, ':=' (VBP=VBP*indice,
                CI=CI*indice,
                VAB=VAB*indice)]

Letra_K <- Letra_K[, .(anio,trimestre,periodo,tipo,desc_act,indice,VBP,CI,VAB)]
write.xlsx(Letra_K, "Letra K/Letra_K1.xlsx")
