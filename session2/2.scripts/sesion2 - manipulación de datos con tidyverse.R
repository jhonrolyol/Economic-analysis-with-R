
#------------------------------------------------------------------------------#
# Sesion 2: Manipulación de datos con tidyverse                                #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#


# Introducción a Tidyverse -----------------------------------------------------

rm(list=ls())
library(tidyverse)

# Importar datos de internet
browseURL("https://www.datosabiertos.gob.pe/dataset/vigilancia-epidemiol%C3%B3gica-de-dengue")

vdengue <- read.csv("https://www.datosabiertos.gob.pe/sites/default/files/datos_abiertos_vigilancia_dengue.csv")
View(vcm)

# Revisar nombres de variables
names(vdengue)

# Estructura de la base de datos
str(vdengue)

# Número de filas
ncol(vdengue)

# Número de columnas
nrow(vdengue)

# Resumen de las variables
summary(vdengue)

# Buscar missing values
library(naniar)
any_na(vdengue)

# Variables con missing values
apply(vdengue, 2, FUN = function(x) any(is.na(x)))

# Verificar filas que contienen missing values
which(is.na(vdengue$diresa))
which(is.na(vdengue$edad))

apply(vdengue, 2, FUN = function(x) any(x == "", na.rm=T))
apply(vdengue, 2, FUN = function(x) sum(x == "", na.rm=T))
temp_l1 <- which(apply(vdengue, 2, FUN = function(x) sum(x == "", na.rm=T)) == 8)

for (x in temp_l1){
  print(which(is.na(vdengue[,x]) | vdengue[,x]==""))
}

vdengue[which(is.na(vdengue[,x]) | vdengue[,x]==""),]

# Eliminar esas observaciones
vdengue <- vdengue[-which(is.na(vdengue[,x]) | vdengue[,x]==""),]


# Método alternativo: usando subset
vdengue <- read.csv("https://www.datosabiertos.gob.pe/sites/default/files/datos_abiertos_vigilancia_dengue.csv")

vdengue1 <- subset(vdenge, !is.na(edad))
nrow(vdengue)

# Operador pipe
vdengue2 <- vdengue |> subset(!is.na(edad))

# Operador pipe de magrittr
vdengue3 <- vdengue %>% subset(!is.na(edad))

rm(list = c("vdengue1","vdengue2","vdengue3"))

# ¿Ventaja? Permite concatenar funciones
vdengue |> 
  subset(!is.na(edad)) |>
  subset(enfermedad != "DENGUE SIN SEÑALES DE ALARMA") |> 
  nrow()

vdengue %>% 
  subset(!is.na(edad)) %>% 
  subset(enfermedad != "DENGUE SIN SEÑALES DE ALARMA") %>% 
  nrow

vdengue %>% 
  subset(!is.na(edad)) %>% 
  subset(enfermedad != "DENGUE SIN SEÑALES DE ALARMA") %>% 
  .$edad

vdengue |> 
  subset(!is.na(edad)) |>
  subset(enfermedad != "DENGUE SIN SEÑALES DE ALARMA") |> 
  dplyr::pull(edad)

## Funciones esenciales para manipulación de datos ----

# Crear / modificar variable
vdengue %>% mutate(fecha = as.Date(paste0(ano,"-01-01"))) %>% head(10)
vdengue %>% transmute(fecha = as.Date(paste0(ano,"-01-01"))) %>% head(10)

# Resumen
vdengue %>% summarise(min_año = min(ano),
                      max_año = max(ano),
                      edad_promedio = mean(edad),
                      edad_promedio2 = mean(edad[tipo_edad=="A"]))

# Ordenar
vdengue %>% arrange(ano) %>% head(10)
vdengue %>% arrange(desc(ano)) %>% head(10)

# Agrupar
vdengue %>% group_by(departamento) %>% head(10)
vdengue %>% group_by(departamento) %>% 
  summarise(casos = n())

vdengue %>% rename(año = ano) %>% head(10)
vdengue %>% setNames(str_to_upper(names(.)))

# Seleccionar variables
vdengue %>% select(departamento, ano, edad, sexo) %>% head(10)
vdengue %>% select(-fecha) %>% head(10)


# Procedemos a manipular la data en un solo proceso con pipe
tictoc::tic()
vdengue %>% 
  subset(!is.na(edad)) %>% 
  mutate(fecha = as.Date(paste0(ano,"-01-01")),
         fecha = fecha %m+% weeks(semana)) %>% 
  mutate(mes = month(fecha)) %>% 
  mutate(edad = ifelse(tipo_edad == "M", floor(edad/12),
                       ifelse(tipo_edad == "D", 0, edad))) %>% 
  rename(año = ano) %>% 
  group_by(departamento,año,mes) %>% 
  summarise(casos = n(),
            edad_promedio = mean(edad),
            ratio_fem = sum(sexo == "F")/n()) %>% 
  ungroup() %>% 
  arrange(desc(año))
tictoc::toc()

tictoc::tic()
vdengue %>% 
  subset(!is.na(edad)) %>% 
  mutate(fecha = as.Date(paste0(ano,"-01-01")),
         fecha = fecha %m+% weeks(semana)) %>% 
  mutate(mes = month(fecha)) %>% 
  mutate(edad = ifelse(tipo_edad == M, floor(edad/12),
                       ifelse(tipo_edad == D, 0, edad))) %>% 
  rename(año = ano) %>% 
  group_by(departamento,año,mes) %>% 
  select(departamento,año,mes,edad,sexo) %>% 
  summarise(casos = n(),
            casos_graves = ,
            edad_promedio = mean(edad),
            ratio_fem = sum(sexo == "F")/n()) %>% 
  ungroup() %>% 
  arrange(desc(año))
tictoc::toc()


## CASO PRÁCTICO: REVISAR Y MANIPULAR DATA DEL SINADEF -------------------------

browseURL("https://www.datosabiertos.gob.pe/dataset/informaci%C3%B3n-de-fallecidos-del-sistema-inform%C3%A1tico-nacional-de-defunciones-sinadef-ministerio")

setwd("D:/Trabajo/Otros/Dictado TuDesarrollo/sesion1")

sinadef <- read.csv("data/fallecidos_sinadef.csv", sep = "|")

# Ejercicios:
# 1. Generar tabla con 10 principales causas de muerte
# 2. Presentar base de datos a nivel de distrito de domicilio con información solo del 2023.
#    Debe incluir las siguientes variables:
#    - Departamento
#    - Provincia
#    - Distrito
#    - Número de fallecidos
#    - Principal causa de muerte
#    - Número de muertes violentas
#    - Edad promedio
#    - Ratio de personas de sexo femenino
#    - Porcentaje de muerte con necropsia realizada



# Formatos de descarga ---------------------------------------------------------

# CSV
sinadef <- read.csv("data/fallecidos_sinadef.csv", sep = "|")
sinadef2 <- read.delim("data/fallecidos_sinadef.csv", sep = "|")

# Excel
browseURL("https://www.datosabiertos.gob.pe/dataset/resumen-de-hogares-afiliados-y-abonados-por-ubigeo-2024-programa-juntos")
afiliados_juntos <- readxl::read_excel("data/Resumen_bimestre_I_2024_SUBIDO_0.xlsx")

# DTA
enaho2 <- haven::read_dta("data/enaho/enaho01-2020-200.dta")

# SAV / SPSS
enaho1 <- haven::read_spss("data/enaho/Enaho01-2020-100.sav")
enaho1b <- haven::read_sav("data/enaho/Enaho01-2020-100.sav")
enaho1c <- foreign::read.spss("data/enaho/Enaho01-2020-100.sav", to.data.frame = TRUE)
enaho1c <- data.frame(enaho1c)

class(enaho1)
identical(enaho1, enaho1b)
identical(enaho1, enaho1c)

table(as.character(lapply(enaho1, function(x) class(x))))
table(as.character(lapply(enaho1b, function(x) class(x))))
table(as.character(lapply(enaho1c, function(x) class(x))))
object.size(enaho1) > object.size(enaho1c)

# DBF
endes0 <- foreign::read.dbf("data/endes/REC0.dbf")

# Base de datos txt 
icen <- read.table("http://met.igp.gob.pe/datos/icen.txt", skip = 17, sep = "")


# Tratamiento de missing values ------------------------------------------------

library(naniar)

## Análisis descriptivo ----

# Verificar si base de datos cuenta con missing values
any_na(enaho2)

# Número de missing values
n_miss(enaho2)

# Porcentaje de missing values (puede ser por variable)
pct_miss(enaho2)

# Descripción por variable
print(miss_var_summary(enaho2), n=43)

# Tabulación de variables por número de missing values
miss_var_table(enaho2)

# Descripción por casos
miss_case_summary(enaho2)

# Tabulación de casos por número de missing values
miss_case_table(enaho2)


## Análisis gráfico ----

# Gráfico de missing values en la base datos
vis_miss(dplyr::slice_sample(enaho2, n = 1000))

# Gráfico de número de casos por número de missing values
gg_miss_case(enaho2)

# Interacciones entre valores perdidos
gg_miss_upset(enaho2)

# Visualización de las combinaciones de missing values
library(VIM)
aggr(enaho2,numbers=T, labels = names(enaho2), cex.axis=.5)

png(file="aggr_plot1.jpeg",width=1920,height=1080) # check the parameters
aggr(enaho2,numbers=F, sortComb=TRUE, labels = names(enaho2), cex.axis=.9, oma = c(7,0,1,0))
dev.off()


library(sjlabelled)
get_label(enaho2$codtarea)
get_label(enaho2$codtiempo)
get_label(enaho2$p217)
get_label(enaho2$p206)
get_label(enaho2$p208b)

get_label(enaho2$p205)
table(enaho2$p205, enaho2$p206, useNA = "always")


## Imputación de valores ----

get_label(enaho2$p203b)
enaho2 <- enaho2 %>% subset(!is.na(p203b))

get_label(enaho2$p209)

library(DMwR2)
enaho2_ci <- centralImputation(enaho2)

temp_enaho2 <- enaho2 %>% mutate(p209 = as.character(p209))
enaho2_ci <- centralImputation(temp_enaho2)


library(simputation)
enaho2_imp <- impute_lm(enaho2, p208b + p209 ~ 1)

enaho2_imp2 <- impute_lm(enaho2, p208b + p209 ~ 1 | ubigeo)

enaho_knn <- VIM::kNN(data = enaho2, variable = c("p208b","p209"), k = 5)      # Función de la librería VIM


# Métodos de cruces de bases ---------------------------------------------------

browseURL("https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf")

endes_an <- haven::read_sav("data/endes/REC44.sav") %>% 
  mutate(HHID = substr(CASEID,1,15))

# Composición de código de hogar:
# HHID = HV001 (conglomerado) + HV002 (vivienda) + HV002A (hogar)
# Composición de código de caso:
# CASEID = HHID + " " (espacio) + QSNUMERO (número de orden de entrevistado)

endes_hog <- read_sav("data/endes/RECH0.sav")


dbase <- left_join(get(paste0("endes",substr(year,3,4))), dbase2, by = "HHID")
assign(paste0("endes",substr(year,3,4)), dbase)


left_join(dbase, dbase2, by = "HHID")
right_join(dbase, dbase2, by = "HHID")
inner_join(dbase, dbase2, by = "HHID")
full_join(dbase, dbase2, by = "HHID")

semi_join(dbase, dbase2, by = "HHID")
anti_join(dbase, dbase2, by = "HHID")

# Apilar filas
endes_2021 <- read_sav("data/endes/2021/REC44.sav")
endes_2022 <- read_sav("data/endes/2022/REC44.sav")

endes <- rbind(endes_an, endes_2021)
endes <- dplyr::bind_rows(endes_an, endes_2021)

endes <- do.call(rbind, list(endes_an, endes_2021, endes_2022)) # Más de 2 bases al mismo tiempo


# Apilar columnas
cbind(endes_an[,1:20],endes_an[,21:33])
