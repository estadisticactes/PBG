# N. Salud #

library(openxlsx)
library(data.table)
library(forecast)
library(zoo)
library(tidyr)
library(R.utils)
library(ymd)
library(readxl)

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="N",]
base_INDEC$cod_act <- NULL

#### Salud PRIVADA ####
# Serie OEDE
OEDE_Ctes <- fread("Bases/OEDE_Corrientes.csv.gz")
OEDE_Ctes <- OEDE_Ctes[cod_act%in%c("8510", "8520", "8530")] # Filtramos por salud humana,Servicios veterinarios y sociales
OEDE_Ctes[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]
OEDE_Ctes$desc_act=NULL

#SOLO SI ES NECESARIO. SI NO ES NECESARIO ANDA A LA LINEA 68

# Estimación para períodos faltantes
periodo_a_act <- as.yearqtr(rstudioapi::showPrompt("Escriba el período a actualizar", "En formato yyyy-q (por ejemplo 2022-4):"))
cant_periodos <- c((periodo_a_act-max(OEDE_Ctes$periodo))*360/90)

# Guardamos los períodos a proyectar
periodos <- c()
for (i in 1:cant_periodos){periodos[i] <- c(max(OEDE_Ctes$periodo)+((90*i)/360))}
periodos <- as.yearqtr(periodos)

# Creamos una tabla vacía donde guardar las proyecciones
tabla <- data.frame()

for (i in unique(OEDE_Ctes$cod_act)){
  
  # Proyecciones de series de empleo
  serie_empleo <- OEDE_Ctes[cod_act==i, .(empleo)]
  serie_empleo <- ts(serie_empleo$empleo, start = c(2003,1), frequency = 4)
  modelo_empleo <- auto.arima(serie_empleo)
  proyeccion_empleo <- forecast(modelo_empleo, cant_periodos)
  plot(proyeccion_empleo)
  
  # Proyecciones de series de remuneraciones
  serie_remuner <- OEDE_Ctes[cod_act==i, .(remuner)]
  serie_remuner <- ts(serie_remuner$remuner, start = c(2003,1), frequency = 4)
  modelo_remuner <- auto.arima(serie_remuner)
  proyeccion_remuner <- forecast(modelo_remuner, cant_periodos)
  plot(proyeccion_remuner)
  
  proyeccion <- data.frame(anio=year(periodos), trimestre=quarter(periodos), cod_act=i, 
                           empleo=round(as.numeric(proyeccion_empleo$mean),0), 
                           remuner=as.numeric(proyeccion_remuner$mean))
  
  tabla <- rbind(tabla, proyeccion)
}

# Anexamos a la tabla de OEDE las proyecciones
tabla <- as.data.table(tabla)
tabla[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]
OEDE_Ctes <- rbindlist(list(OEDE_Ctes, tabla), use.names=T)

#SEGUI A PARTIR DE ACA SI NO PROYECTASTE

# Calculamos masa salarial y pivoteamos la tabla
OEDE_Ctes <- OEDE_Ctes[anio>=2004, .(anio, trimestre, periodo, cod_act, empleo, masa_salarial=empleo*remuner)]
OEDE_Ctes <- setDT(pivot_longer(OEDE_Ctes, cols = 5:6, names_to = "tipo", values_to = "serie"))
OEDE_Ctes[, tipo:= ifelse(tipo=="empleo", "Precios constantes", "Precios corrientes")]
OEDE_Ctes[, desc_act:= "SALUD PRIVADA"]
OEDE_Ctes[, cod_act:=85]


#### ENSEÑANZA PÚBLICA ####

# Serie empleo público
empleo_pub <- read_xlsx("Bases/planilla_de_personal_2004_feb2023.xlsx")
#empleo_pub <- read.csv ("Bases/planilla_de_personal_2004_feb2023.csv", sep = "@")
colnames(empleo_pub) <- c("cod_liq", "desc_liq", "anio", "mes", "cod_jur", "desc_jur", "cod_tipo", "desc_tipo", names(empleo_pub)[9:18])
empleo_pub <- as.data.table(empleo_pub)




# Filtramos por:
# - Salud Publica (4)
# - Instituto de Cardiología (34)

empleo_pub <- empleo_pub[cod_jur%in%c(34,4),]

# Filtramos por los conceptos de sueldos únicamente para no duplicar empleo
empleo_pub <- empleo_pub[grepl("S.A.C|SAC|Sac|sac|Liq.|liq.|Plus|Ayuda|plus|Bono",desc_liq)==F,]
empleo_pub <- empleo_pub[, .(empleo= sum(total_gral),
                             masa_salarial= sum(importe_gral)), by = .(anio, mes)]
empleo_pub[, periodo:= ymd(paste0(anio,"-",mes,"-01"))]
empleo_pub[, trimestre:=quarter(periodo)]
empleo_pub[, periodo:=as.yearqtr(periodo, "%Y-%q")]
empleo_pub <- empleo_pub[, .(empleo=mean(empleo),
                             masa_salarial=mean(masa_salarial)), by = .(periodo)]

empleo_pub <- setDT(pivot_longer(empleo_pub, cols=2:3, names_to = "tipo", values_to = "serie"))
empleo_pub[, tipo:= ifelse(tipo=="empleo", "Precios constantes", "Precios corrientes")]

empleo_pub <- empleo_pub[, .(anio=year(as.Date(periodo)), trimestre=quarter(as.Date(periodo)), periodo,
                             cod_act=85, desc_act="SALUD PUBLICA", tipo, serie)]



# Unimos las tablas
Letra_N <- rbindlist(list(OEDE_Ctes, empleo_pub), use.names=T)
Letra_N <- merge.data.table(Letra_N, base_INDEC, by = c("desc_act"))
Letra_N[, base:= mean(serie[anio==2004]), by = .(tipo,desc_act)]
Letra_N[, indice:= serie/base, by = .(tipo, desc_act)]
Letra_N[, ':=' (VBP=VBP*indice, CI=CI*indice, VAB=VAB*indice)]

# Guardamos la tabla final
Letra_N <- Letra_N[, .(anio, trimestre, tipo, cod_act, desc_act, VBP, CI, VAB)]
write.xlsx(Letra_N, "Letra N/Letra_N.xlsx")

