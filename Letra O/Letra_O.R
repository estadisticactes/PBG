# O. SERVICIOS COMUNITARIOS, SOCIALES Y PERSONALES #

library(data.table)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)

rm(list=ls())

# Divisiones de actividad a 2 dígitos:
# 90. Eliminación de desperdicios y aguas residuales, saneamiento y servicios similares
# 91. Servicios de asociaciones
# 92. Servicios culturales y deportivos
# 93. Servicios n.c.p.

# Cargamos los datos del CNE04 para usarlos como base:
CNE04 <- setDT(read.xlsx("Bases/CNE04_Corrientes.xlsx", startRow = 7, cols = c(1:6), na.strings = "sd", sheet = "CNE_04"))
CNE04 <- CNE04[letra=="O", .(cod_act=substr(cod_act,1,2), VBP = VBP_pbasicos, CI, VAB = VA_pbasicos)]

# Ajuste por Economía no observada (ENO)
CNE04[, ':=' (VBP=VBP/(1-0.445), CI=CI/(1-0.445), VAB=VAB/(1-0.445))]
CNE04[, desc_act:= case_when(cod_act=="90" ~ "Eliminación de desperdicios y aguas residuales, saneamiento y servicios similares",
                             cod_act=="91" ~ "Servicios de asociaciones",
                             cod_act=="92" ~ "Servicios culturales y deportivos",
                             cod_act=="93" ~ "Servicios n.c.p.")]

# Cargamos los datos de SIPA (Observatorio de empleo y dinámica empresarial)
OEDE_Ctes <- fread("Bases/OEDE_Corrientes.csv.gz")
OEDE_Ctes <- OEDE_Ctes[substr(cod_act,1,2) %in% c("90", "91", "92", "93"),]
OEDE_Ctes[, cod_act:= substr(cod_act,1,2)]
OEDE_Ctes <- OEDE_Ctes %>% na.omit()
OEDE_Ctes <- OEDE_Ctes[anio>=2004,
                       .(empleo=sum(empleo, na.rm=T), 
                         remuner=sum(remuner, na.rm=T)),
                       by = .(anio, trimestre, cod_act)]
OEDE_Ctes[, periodo:= as.yearqtr(paste0(anio,"-",trimestre))]

# Proyecciones para los períodos faltantes
#SOLO SI ES NECESARIO. SI NO VAS A PROYECTAR ANDA A LA LINEA 86
# Calculamos la cantidad de periodos a actualizar
periodo_a_act <- as.yearqtr(rstudioapi::showPrompt("Escriba el período a actualizar", "En formato yyyy-q (por ejemplo 2022-4):"))
cant_periodos <- c((periodo_a_act-max(OEDE_Ctes$periodo))*360/90)

# Guardamos los periodos a proyectar
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

#SI NO PROYECTASETE SEGUI A PARTIR DE ACA

# Calculamos masa salarial y pivoteamos la tabla
OEDE_Ctes <- OEDE_Ctes[anio>=2004, .(anio, trimestre, periodo, cod_act, empleo, masa_salarial=empleo*remuner)]
OEDE_Ctes <- setDT(pivot_longer(OEDE_Ctes, cols = 5:6, names_to = "tipo", values_to = "serie"))
OEDE_Ctes[, tipo:= ifelse(tipo=="empleo", "Precios constantes", "Precios corrientes")]


# Calculamos los índices para mover las series a precios corrientes y constantes
bases <- OEDE_Ctes[anio==2004, .(base = mean(serie)), by = .(cod_act, tipo)]
indices <- merge.data.table(OEDE_Ctes, bases, by = c("cod_act","tipo"))
indices[, ':=' (indice = serie/base), by = .(cod_act, tipo)]

# Anexamos los datos del año base para mover las series:
Letra_O <- merge.data.table(indices, CNE04, by = "cod_act")

Letra_O[, ':=' (VAB= VAB*indice, CI=CI*indice, VBP=VBP*indice)]
Letra_O[, periodo:= paste0(anio, "0", trimestre)]
Letra_O <- Letra_O[, .(anio, trimestre, periodo, tipo, cod_act, desc_act, VBP, CI, VAB)]

# Limpiamos memoria
rm(bases, CNE04, indices, OEDE_Ctes)

# Guardamos la base final de la letra
write.xlsx(Letra_O, "Letra O/Letra_O.xlsx")


