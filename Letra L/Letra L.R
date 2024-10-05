# L. GASTO PUBLICO #
library(openxlsx)
library(data.table)
library(forecast)
library(tempdisagg)
library(lubridate)
library(DBI)
library(RMySQL)
library(dplyr)

# Carga de los datos del anioo base
base_INDEC <- setDT(read.xlsx("Bases/Base_2004_INDEC.xlsx", startRow = 5, cols = c(1:6)))
base_INDEC <- base_INDEC[letra=="L",]

#conecto al sql
database<- dbConnect(MySQL(), user="estadistica", host="54.94.131.196", password="Estadistica2024!!", dbname="datalake_economico")


##Corriente

##Cargo Base de Gasto Publico
Base_Gasto_Publico <- setDT((read.xlsx("Letra L/Base Gasto Publico.xlsx", sheet = "Base")))

Base_nueva<- setDT(dbGetQuery(database, statement = "select * from pbg_presupuesto_ejecutado"))

rm(database)

## ELIMINO TODO LO QUE NO SEA 100, 200 O 300

Base_Gasto_Publico <- Base_Gasto_Publico[, head(names(Base_Gasto_Publico), 5), with = FALSE]

#Base_Gasto_Publico[, Direccion := NULL]
Base_nueva <- Base_nueva[, head(names(Base_nueva), 6), with = FALSE]
#calculo la sumatoria por anio 
#Base_nueva <- Base_nueva %>%
#  group_by(jurisdiccion, año) %>%
#  summarise(
#    gastos_en_personal = sum(gastos_en_personal),
#    bienes_de_consumo = sum(bienes_de_consumo),
#    servicios_no_personales = sum(servicios_no_personales)
#  ) %>%
#  ungroup()

#cambio los nombres de las columnas
Base_nueva <- Base_nueva %>%
  rename(
    Direccion = jurisdiccion,
     anio = año,
    mes = mes,
    "VAB" = gastos_en_personal,
    "CI1" = bienes_de_consumo,
    "CI2" = servicios_no_personales
  )
# re ordeno
Base_nueva <- Base_nueva %>%
  select(anio,mes,  everything())


###Filtro
## 3	Ministerio de Educacion
## 4	Ministerio de Salud Publica
## 21	Instituto de Loteria y Casinos
## 23	Instituto de Cardiologia de Corrientes
## 41	Instituto de Prevision Social
## 42	Instituto de Obra Social de Corrientes
## 51	Direccion Provincial de Energia de Corrientes


Base_Gasto_Publico <- Base_Gasto_Publico[!(Base_Gasto_Publico$Direccion %in% as.integer(c(3, 4, 21, 23, 41, 42, 51)))]
Base_nueva <- Base_nueva[!("Direccion" %in% c(3, 4, 21, 23, 41, 42, 51))]



##Reordeno
setnames(Base_Gasto_Publico, "año" , "anio")
setnames(Base_Gasto_Publico, "100" , "VAB")
setnames(Base_Gasto_Publico, "200" , "CI1")
setnames(Base_Gasto_Publico, "300" , "CI2")
Base_Gasto_Publico <- as.data.table(Base_Gasto_Publico)
Letra_L <- Base_Gasto_Publico[, .(VAB = sum(VAB),
                                  CI = sum(CI1 , CI2, NA, na.rm = TRUE), 
                                  VBP = sum(VAB, CI1 , CI2, NA, na.rm = TRUE)), 
                              by = (anio)]


setorder(Letra_L,anio)

# hago la interpolacion y sigo ordenando la tabla
m1 <- td(Letra_L$VAB ~ 1, to = 12, method = "denton-cholette")
m2 <- td(Letra_L$CI ~ 1, to = 12, method = "denton-cholette")
m3 <- td(Letra_L$VBP ~ 1, to = 12, method = "denton-cholette")
serie1 <- c(m1$values)
serie2 <- c(m2$values)
serie3 <- c(m3$values)
fechas <- seq(as.Date("2004-01-01"), by = "month", length.out = (length(Letra_L$VAB)*12))

Letra_L <- data.table(anio = year(fechas), mes = month(fechas) , periodo = fechas, cod_act = "70", tipo = "Precios corrientes", serie1 = serie1, serie2 = serie2, serie3 = serie3, desc_act = "Administración Pública y Defensa; Planes de Seguridad Social de Afiliación Obligatoria")

# uno las dos bases
Base_nueva[, periodo := as.Date(paste(anio, mes, "01", sep = "-"))]
Base_nueva <- Base_nueva %>%
  select(anio,mes,periodo,  everything())

Base_nueva <- Base_nueva[, .(serie1 = sum(VAB),
                                  serie2 = sum(CI1 , CI2, NA, na.rm = TRUE), 
                                  serie3 = sum(VAB, CI1 , CI2, NA, na.rm = TRUE)), 
                              by = .(anio, mes)]
Base_nueva[, periodo := as.Date(paste(anio, mes, "01", sep = "-"))]
Base_nueva <- Base_nueva %>%
  select(anio,mes,periodo,  everything())

Base_nueva <- data.table(
  anio = Base_nueva$anio,
  mes = Base_nueva$mes,
  periodo = Base_nueva$periodo,
  cod_act = "70",
  tipo = "Precios corrientes",
  serie1 = Base_nueva$serie1,
  serie2 = Base_nueva$serie2,
  serie3 = Base_nueva$serie3,
  desc_act = "Administración Pública y Defensa; Planes de Seguridad Social de Afiliación Obligatoria"
)

Letra_L <- rbind(Base_nueva, Letra_L)
setorder(Letra_L,anio)
objetos_a_mantener <- c("base_INDEC", "Letra_L")
objetos_a_eliminar <- setdiff(ls(), objetos_a_mantener)
rm(list = objetos_a_eliminar)

## Constante

# Serie empleo público
empleo_pub <- read.xlsx ("Bases/planilla_de_personal_2004_feb2023.xlsx")
colnames(empleo_pub) <- c("cod_liq", "desc_liq", "anio", "mes", "cod_jur", "desc_jur", "cod_tipo", "desc_tipo", names(empleo_pub)[9:18])
empleo_pub <- as.data.table(empleo_pub)
empleo_pub <- empleo_pub[!cod_jur%in%c(4 , 3 , 33 ,37),]

##Filtro los conceptos que me duplican empleados

empleo_pub <- empleo_pub[grepl("S.A.C|SAC|Sac|sac|Liq.|liq.|Plus|Ayuda|plus|Bono",desc_liq)==F,]
empleo_pub <- empleo_pub[, .(serie= sum(total_gral)), by = .(anio, mes)]
empleo_pub[, periodo:= ymd(paste0(anio,"-",mes,"-01"))]
empleo_pub [, tipo := "Precios Constantes"] 
empleo_pub <- empleo_pub[, .(anio=year(periodo), mes=month(periodo), periodo,
                             cod_act=70, desc_act="Administración Pública y Defensa; Planes de Seguridad Social de Afiliación Obligatoria", tipo, serie)]



## Unimos
#Corriente
Letra_L[, cod_act := as.numeric(cod_act)]
base_INDEC[, cod_act := as.numeric(cod_act)]
Letra_L <- merge.data.table(Letra_L, base_INDEC, by = c("cod_act", "desc_act"))
Letra_L[, base1:= mean(serie1[anio==2004]), by = .(tipo,desc_act)]
Letra_L[, base2:= mean(serie2[anio==2004]), by = .(tipo,desc_act)]
Letra_L[, base3:= mean(serie3[anio==2004]), by = .(tipo,desc_act)]
Letra_L[, indice1:= serie1/base1, by = .(tipo, desc_act)]
Letra_L[, indice2:= serie2/base2, by = .(tipo, desc_act)]
Letra_L[, indice3:= serie3/base3, by = .(tipo, desc_act)]
Letra_L[, ':=' (VBP=VBP*indice3, CI=CI*indice2, VAB=VAB*indice1)]
#Constante
empleo_pub[, cod_act := as.numeric(cod_act)]
Letra_Lc <- merge.data.table(empleo_pub, base_INDEC, by = c("cod_act", "desc_act"))
Letra_Lc[, basec:= mean(serie[anio==2004]), by = .(tipo,desc_act)]
Letra_Lc[, indicec:= serie/basec, by = .(tipo, desc_act)]
Letra_Lc[, ':=' (VBP=VBP*indicec, CI=CI*indicec, VAB=VAB*indicec)]

#merge
Letra_L <- Letra_L[, .(anio, mes, tipo, cod_act, desc_act, VBP, CI, VAB)]
Letra_Lc <- Letra_Lc[, .(anio, mes, tipo, cod_act, desc_act, VBP, CI, VAB)]
Letra_L <- rbindlist(list(Letra_L, Letra_Lc), use.names=T)

# Guardamos la tabla final
write.xlsx(Letra_L, "Letra L/Letra_L.xlsx")




