
#------------------------------------------------------------------------------#
# Sesion 3: Missing values, cruces de datos y generación de gráficos           #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#

rm(list=ls())

setwd("D:/Trabajo/Otros/Dictado TuDesarrollo/semana2")

enaho2 <- haven::read_dta("data/enaho/enaho01-2020-200.dta")

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
pct_miss(enaho2$p203b)
enaho2 <- enaho2 %>% subset(!is.na(p203b))

get_label(enaho2$p209)
get_labels(enaho2$p209)

table(enaho2$p203b, enaho2$p209, useNA = "always")
table(enaho2$p208a[enaho2$p203b==3], enaho2$p209[enaho2$p203b==3], useNA = "always")
enaho2$p209[which(enaho2$p203b==3 & is.na(enaho2$p209))] <- 6

enaho2 %>% mutate(ID = paste0(ubigeo,conglome,vivienda,hogar)) %>% 
  mutate(interes = ifelse(p203b==1 & is.na(p209), 1, 0)) %>% 
  group_by(ID) %>% 
  mutate(pareja = any(p203b == 2),
         hijos = any(p203b == 3)) %>% ungroup() %>% 
  mutate(pareja_hijos = pareja & hijos) %>% 
  subset(interes == 1) %>% 
  summarise(si_pareja = sum(pareja),
            no_pareja = sum(!pareja),
            `si_hijo y pareja` = sum(pareja_hijos),
            `no_hijo y pareja` = sum(!pareja_hijos)) %>% 
  pivot_longer(everything(),
               names_to = c(".value"," "),
               names_sep = "_")


library(DMwR2)
enaho2_ci <- centralImputation(enaho2)

temp_enaho2 <- enaho2 %>% mutate(p209 = as.character(p209))
enaho2_ci <- centralImputation(temp_enaho2)


library(simputation)
enaho2_imp <- impute_lm(enaho2, p208b + p209 ~ 1)

enaho2_imp2 <- impute_lm(enaho2, p208b + p209 ~ 1 | ubigeo)

#enaho_knn <- VIM::kNN(data = enaho2, variable = c("p208b","p209"), k = 5)      # Función de la librería VIM


# Métodos de cruces de bases ---------------------------------------------------

browseURL("https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf")

endes_an <- haven::read_sav("data/endes/2020/REC44.sav") %>% 
  mutate(HHID = substr(CASEID,1,15))

# Composición de código de hogar:
# HHID = HV001 (conglomerado) + HV002 (vivienda) + HV002A (hogar)
# Composición de código de caso:
# CASEID = HHID + " " (espacio) + QSNUMERO (número de orden de entrevistado)

endes_hog <- haven::read_sav("data/endes/2020/RECH0.sav")

left_join(endes_an, endes_hog, by = "HHID") %>% dim()
right_join(endes_an, endes_hog, by = "HHID") %>% dim()
inner_join(endes_an, endes_hog, by = "HHID") %>% dim()
full_join(endes_an, endes_hog, by = "HHID") %>% dim()

semi_join(endes_an, endes_hog, by = "HHID") %>% dim()
anti_join(endes_an, endes_hog, by = "HHID") %>% dim()

# Apilar filas
endes_2021 <- read_sav("data/endes/2021/REC44.sav")
endes_2022 <- read_sav("data/endes/2022/REC44.sav")

endes <- rbind(endes_an, endes_2021)
endes <- dplyr::bind_rows(endes_an, endes_2021)

endes <- do.call(rbind, list(endes_an, endes_2021, endes_2022)) # Más de 2 bases al mismo tiempo


# Apilar columnas
cbind(endes_an[,1:20],endes_an[,21:33])

# Para años del 2016 al 2022
for (year in 2016:2022){
  # Cargar base de datos con información de anemia
  dbase <- haven::read_sav(paste0("data/endes/",year,"/REC44.sav")) %>% 
  # Crear variables de ID faltantes
    mutate(ID1 = year,
           HHID = substr(CASEID,1,15))
  
  # Guardar la base con nombre genérico a nombre específico
  assign(paste0("endes",substr(year,3,4)), dbase)
}

# Solo para años del 2019 al 2022, años previos tienen otra estructura
for (year in 2019:2022){
  # Cargar base de datos con información de hogares
  dbase2 <- haven::read_sav(paste0("data/endes/",year,"/RECH0.sav")) %>% 
  # Seleccionar variables de interés
    select(HHID,UBIGEO,CODCCPP,NOMCCPP,HV009,HV022,HV025,HV040)
  
  # Unir base de datos de anemia y de hogares
  dbase <- left_join(get(paste0("endes",substr(year,3,4))), dbase2, by = "HHID")
  
  # Guardar la base con nombre genérico a nombre específico
  assign(paste0("endes",substr(year,3,4)), dbase)
}


names(haven::read_sav(paste0("data/endes/",2016,"/RECH0.sav")))
# No está el ubigeo y algunas variables tienen otro nombre
# ¿Cómo recuperar el ubigeo?

library(sf)
# Cargar shapefile de distritos
browseURL("https://www.datosabiertos.gob.pe/dataset/limites-departamentales/resource/a43e17c8-fa37-463d-aa7e-2ce2a272491b")
distritos.sh <- read_sf("data/DISTRITOS") %>% select(IDDIST)

for (year in 2016:2018){
  # Cargar base de datos con información de hogares
  dbase2 <- haven::read_sav(paste0("data/endes/",year,"/RECH0.sav")) %>% 
  # Corregir nombres
    setNames(mgsub::mgsub(str_to_upper(names(.)),c("^LONG.*","^LAT.*"), c("LONGITUD","LATITUD"))) %>% 
  # Seleccionar variables de interés
    select(HHID,CODCCPP,NOMCCPP,LONGITUD,LATITUD,HV009,HV022,HV025,HV040)
  
  # Unir base de datos de anemia y hogares
  dbase <- left_join(get(paste0("endes",substr(year,3,4))), dbase2, by = "HHID") %>% 
  # Convertir dataframe a formato simple feature
    sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = 4326)
  
  # Unir endes con shapefile de distritos
  dbase <- st_join(dbase, distritos.sh) %>% 
  # Regresar a formato dataframe
    st_drop_geometry() %>% 
  # Cambiar nombre
    rename("UBIGEO" = "IDDIST")
  
  # Guardar la base con nombre genérico a nombre específico
  assign(paste0("endes",substr(year,3,4)), dbase)
}

# Apilar las endes
endes <- do.call(rbind,list(endes16,endes17,endes18,endes19,endes20,endes21,endes22))

# Borrar bases de datos intermedias
rm(list = c("endes16","endes17","endes18","endes19","endes20","endes21","endes22", "dbase", "dbase2", "year"))

endes <- endes %>% 
  mutate(es_lima = ifelse(substr(UBIGEO,1,2) == "15", "Lima", "No Lima"),
         mas1000m = ifelse(HV040 >= 1000, "Mas de 1000 metros",
                           ifelse(HV040 < 1000, "Menos de 1000 metros", NA)),
         area = ifelse(HV025 == 1, "Urbano", 
                       ifelse(HV025 == 2, "Rural", NA))) %>% 
  sjlabelled::var_labels(es_lima = "¿Se encuentra ubicado en Lima?",
                         mas1000m = "¿Se encuentra a más de 1000 metros de altitud?",
                         area = "¿En qué tipo de área se encuentra?")


sjlabelled::get_label(endes)


# Gráficos con R ---------------------------------------------------------------

## R Base ----

### Scatterplot ----
sample_endes <- endes %>% slice_sample(n=2000)
attach(sample_endes)

# Básico
plot(HW1, HW3)

# Con etiquetas
plot(HW1, HW3,
     main = "Altura de menores de 5 años por edad\n2016-2022",
     xlab = "Edad (meses)", ylab = "Altura (centímetros)")

detach()


### Dotchart ----
endes_altura_edad <- endes %>% 
  group_by(HW1) %>% 
  summarise(HW3 = mean(HW3, na.rm=T)) #%>% subset(HW1%%2 == 0)

attach(endes_altura_edad)

dotchart(HW3, labels = HW1, main = "Altura promedio por edad", xlab = "Altura", ylab = "Edad")

detach()


### Gráfico de líneas ----
attach(endes_altura_edad)

plot(HW1, HW3, main = "Altura promedio por edad", xlab = "Altura", ylab = "Edad", type = "l")

detach()


# Gráfico de barras ----

barplot(table(endes$ID1), main = "Tamaño de muestra por año")


### Boxplot ----

# Estándar
boxplot(endes$HW2, main = "Distribución del peso en gramos")

# Horizontal
boxplot(endes$HW2, main = "Distribución del peso en gramos", horizontal = T)

# Por tipo de area
boxplot(endes$HW2 ~ endes$area, main = "Distribución del peso en gramos")


### Histograma ----

# En frecuencia
hist(endes$HW53, main = "Distribución del nivel de hemoglobina")

# En densidad
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", freq = F)

# Cambiar numero de breaks
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", breaks = 30)


