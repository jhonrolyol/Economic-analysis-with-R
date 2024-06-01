
#------------------------------------------------------------------------------#
# Sesion 6: Muestreo y estimación de parámetros                                #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#

rm(list=ls())

library(tidyverse)
library(survey)

setwd("D:/Trabajo/Otros/Dictado TuDesarrollo/semana3")

censo1 <- foreign::read.dbf("data/censo_universitario/01_CENAUN_CED01_PRE-GRADO_01.dbf")

# Revsión de missing values en base de interés
naniar::any_na(censo1$TDESHOR)
naniar::any_na(censo1$TDESMIN)
naniar::any_na(censo1$INGMONE)
naniar::any_na(censo1$DISCAPA_6)

# Codigode de universidad
sort(table(censo1$CAP1_CODUN))

censo1 <- censo1 %>% mutate(TVIAJE = TDESHOR*60 + TDESMIN)

mu <- mean(censo1$TVIAJE)


# ¿Cómo calcular el tamaño de la muestra?
e <- 5
N <- nrow(censo1)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
sigma <- sd(censo1$TVIAJE)
#sigma <- range(censo1$TVIAJE)[2]/6
n <- (z*sigma^2*N)/(z*sigma^2 + e^2*N) + 5000

set.seed(1234)
seleccionados <- sample(1:N,ceiling(n), replace = F)

# Generar muestra
muestra <- censo1[seleccionados,]
# Agregar dato de tamaño de población
muestra$N <- N
muestra$INGMONE <- as.character(muestra$INGMONE)
muestra$DISCAPACIDAD <- ifelse(as.character(muestra$DISCAPA_6)=="1",0,1)

MAS <- svydesign(id=~1, fpc=~N, data=muestra)

# Cálculo de la media (variable continua)
svymean(~TVIAJE, MAS)
mean(muestra$TVIAJE)
# Cálculo de la proporción (variable discontinua)
svymean(~INGMONE, MAS)
# Cálculo del total
svytotal(~DISCAPACIDAD, MAS)


# Para contraste
prop.table(table(censo1$INGMONE))
table(censo1$DISCAPA_6) # 1 = No tiene discapacidad






