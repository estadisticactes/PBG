# G. COMERCIO MINORISTA Y MAYORISTA

library(data.table)
library(openxlsx)
library(janitor)
library(zoo)
library(tidyverse)

# Con datos de DNRPA y Secretaría de energía
# 501. Venta de vehículos automotores, excepto motocicletas
# 504. Venta, mantenimiento y reparación de motocicletas y de sus partes, piezas y accesorios
# 505. Venta al por menor de combustible para vehículos automotores y motocicletas

# Con datos de rentas las demás divisiones

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx",startRow = 5))
base_INDEC <- base_INDEC[letra=="G",][,c(4:6)]

# Ponderamos con los datos del CNE04
cne04 <- setDT(read.xlsx("Bases/CNE04_Corrientes.xlsx", sheet = "CNE_04", startRow = 7, na.strings = "sd"))
cne04 <- cne04[letra=="G", .(cod_act,VBP_pbasicos,VA_pbasicos)]

cne04[, ':=' (p_vbp=VBP_pbasicos/sum(cne04$VBP_pbasicos),
              p_vab=VA_pbasicos/sum(cne04$VA_pbasicos))]

# Año base 2004
base <- cbind(cne04,base_INDEC)
base[, ':=' (VBP=VBP*p_vbp, VAB=VAB*p_vab)]
base[, CI:=VBP-VAB]
base <- base[, .(cod_act=as.character(cod_act), VBP, CI, VAB)]


# Datos de rentas
rentas <- setDT(read.xlsx("Letra G/Bases G.xlsx", sheet = "Rentas"))
rentas[, periodo_m:= excel_numeric_to_date(periodo_m)]
rentas[, periodo_t:= as.yearqtr(periodo_t)]

# Deflactamos los datos de rentas
rentas <- rentas[, .(base_imponible=sum(base_imponible,na.rm=T),
                     base_imp_defl=sum(base_imp_defl,na.rm=T)),
                 by = .(periodo_t,cod_actividad,division)]
rentas[, ':=' (base_c = mean(base_imponible[year(periodo_t)==2004]),
               base_k = mean(base_imp_defl[year(periodo_t)==2004])),
       by = .(cod_actividad,division)]
rentas[, ':=' (indice_c= base_imponible/base_c*100,
               indice_k= base_imp_defl/base_k*100),
       by = .(periodo_t,cod_actividad,division)]
rentas[, ':=' (base_c=NULL, base_k=NULL)]

# rentas <- rentas[!(cod_actividad %in% c("501","504","505"))]

# Datos para las divisiones 501,504 y 505
series <- setDT(read.xlsx("Letra G/Bases G.xlsx", sheet = "Otras series"))
series[, periodo:= excel_numeric_to_date(periodo)]
series[, ':=' (d501=autos_patent+autos_transf,
               d504=motos_patent+motos_transf,
               d505=combustibles)]
series <- series[, .(d501=sum(d501),d504=sum(d504),d505=sum(d505)),
                 by = .(periodo_t=as.yearqtr(periodo))]
series <- setDT(pivot_longer(series, cols=c(2:4), names_to = "cod_actividad", values_to = "serie"))
series[, cod_actividad:= substr(cod_actividad,2,4)]
series[, base:= mean(serie[year(periodo_t)==2004]), by = .(cod_actividad)]
series[, indice_k:= serie/base*100, by = .(cod_actividad)]
series[, ':=' (base=NULL,serie=NULL)]

# Anexamos los datos anteriores a la tabla general con los índices
rentas <- merge.data.table(rentas,series, by = c("periodo_t","cod_actividad"), all = T)
rentas[, indice_k:= ifelse(is.na(indice_k.y),indice_k.x,indice_k.y)]
rentas[, ':=' (indice_k.x=NULL, indice_k.y=NULL)]

componentes <- merge.data.table(rentas[,-c(4,5)], base,
                                by.x = "cod_actividad", by.y = "cod_act")
componentes[, ':=' (VBP_k=VBP*indice_k/100, VBP_c=VBP*indice_c/100,
                    CI_k=CI*indice_k/100, CI_c=CI*indice_c/100,
                    VAB_k=VAB*indice_k/100, VAB_c=VAB*indice_c/100)]
componentes <- pivot_longer(componentes[,-c(4:8)], cols=4:9, 
                            names_to = "tipo", values_to = "serie")
componentes <- separate(componentes, col="tipo", into=c("componente","tipo"), sep = "_")
componentes <- pivot_wider(componentes, names_from = "componente", values_from = "serie")
componentes <- as.data.table(componentes)
componentes[, tipo:= ifelse(tipo=="k","Precios constantes","Precios corrientes")]


# Guardamos la tabla final
write.xlsx(componentes, "Letra G/Letra_G.xlsx")
