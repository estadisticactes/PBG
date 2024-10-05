# H. HOTELERÍA Y RESTAURANTES

library(data.table)
library(openxlsx)
library(zoo)
library(janitor)

# Carga de los datos del año base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx",startRow = 5))
base_INDEC <- base_INDEC[letra=="H",]
base_INDEC[, cod_act:=as.character(cod_act)]

# Cargamos los datos de rentas trimestralizados a precios constantes
datos <- as.data.table(read.xlsx("Letra H/Rentas H.xlsx", sheet = "Rentas Directo"))
datos[, periodo_t:= as.yearqtr(periodo_t)]
datos <- datos[, .(base_imponible=sum(base_imponible), base_imp_defl=sum(base_imp_defl)),
               by = .(periodo_t, cod_actividad,division)]

# Pivoteamos para crear los índices
datos <- setDT(pivot_longer(datos, cols=c(4,5), names_to = "tipo", values_to = "serie"))
datos[, tipo:= ifelse(tipo=="base_imponible","Precios corrientes","Precios constantes")]
datos[, indice:= serie/mean(serie[year(periodo_t)==2004])*100,
      by = .(cod_actividad,division,tipo)]

datos <-  merge.data.table(datos, base_INDEC[,c(2,4:6)], by.x = "cod_actividad", by.y="cod_act")
datos[, ':=' (VBP=VBP*indice/100, CI=CI*indice/100, VAB=VAB*indice/100)]

Letra_H <- datos[, .(periodo_t, tipo, cod_act=cod_actividad, desc_act=division, VBP, CI, VAB)]
write.xlsx(Letra_H, "Letra H/Letra_H1.xlsx")
