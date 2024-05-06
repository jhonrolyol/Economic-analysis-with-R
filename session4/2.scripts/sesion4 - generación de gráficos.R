
#------------------------------------------------------------------------------#
# Sesion 4: Generación de gráficos con r base y ggplot2                        #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#

rm(list=ls())

library(tidyverse)

setwd("D:/Trabajo/Otros/Dictado TuDesarrollo/semana2")


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
datos_por_edad <- endes %>% 
  group_by(HW1) %>% 
  summarise(HW2 = mean(HW2, na.rm=T),
            HW3 = mean(HW3, na.rm=T)) #%>% subset(HW1%%2 == 0)

attach(datos_por_edad)

dotchart(HW2, labels = HW1, main = "Peso promedio por edad", xlab = "Peso", ylab = "Edad")

detach()


### Gráfico de líneas ----
attach(datos_por_edad)

plot(HW1, HW3, main = "Altura promedio por edad", xlab = "Altura", ylab = "Edad", type = "l")

detach()


# Gráfico de barras ----

# Simple a partir de un table
barplot(table(endes$ID1), main = "Tamaño de muestra por año")

# Generar datos para gráfico por departamento
distritos.sh <- read_sf("data/DISTRITOS") %>% select(IDDIST, DEPARTAMEN, PROVINCIA, DISTRITO)
endes <- left_join(endes, st_drop_geometry(distritos.sh), by = c("UBIGEO" = "IDDIST"))

imc_departamen <- endes %>% 
  group_by(DEPARTAMEN) %>% 
  summarise(IMC = mean(IMC, na.rm = T))

# Gráfico de barras por departamento
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN)

# Gráfico de barras por departamento girar etiquetas
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN, las = 2)

# Gráfico de barras por departamento girar etiquetas y ampliar margen
par(mar=c(11,4,4,4))
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN, las = 2)

# Gráfico de barras por departamento horizontal
par(mar=c(4,11,4,4))
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN, las = 2, horiz = T)
dev.off()

# Agregar línea y otros parámetros
par(mar=c(4,11,4,4))
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN, las = 2, horiz = T, 
        col = alpha("navy", alpha = 0.6), # color con función alpha para transparentar
        main = "IMC promedio por departamento")
lines(rep(mean(endes$IMC, na.rm=T),2), c(0,100), col = "red", 
      lty = 2, # tipo de linea
      lwd = 3) # ancho de linea
dev.off()


### Boxplot ----

# Estándar
boxplot(endes$HW53, main = "Distribución de la anemia")

# Horizontal
boxplot(endes$HW53, main = "Distribución de la anemia", horizontal = T)

# Por tipo de area
boxplot(endes$HW53 ~ endes$area, main = "Distribución de la anemia")


### Histograma ----

# En frecuencia
hist(endes$HW53, main = "Distribución del nivel de hemoglobina")

# En densidad
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", freq = F)

# Cambiar numero de breaks
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", breaks = 30)

# Grafico de densidad
plot(density(endes$HW53, na.rm = T), main = "Distribución del nivel de hemoglobina")

# Superposición
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", freq = F, xlab = "Hemoglobina")
lines(density(endes$HW53, na.rm = T))


### Múltiples gráficos ----
par(mfrow = c(2,2),
    mar = c(4,8,4,4)) # indicar número de filas y columnas

# 1
plot(sample_endes$HW1, sample_endes$HW3,
     main = "Altura de menores de 5 años por edad\n2016-2022",
     xlab = "Edad (meses)", ylab = "Altura (centímetros)")
lines(endes_altura_edad$HW1, endes_altura_edad$HW3, main = "Altura promedio por edad", 
      xlab = "Altura", ylab = "Edad", type = "l", col = "red", lwd = 2.5)

# 2
dotchart(datos_por_edad$HW2, labels = ifelse(endes_altura_edad$HW1 %% 3 != 0, "", endes_altura_edad$HW1), 
         main = "Peso promedio por edad", xlab = "Peso", ylab = "Edad")

# 3
barplot(imc_departamen$IMC, names.arg = imc_departamen$DEPARTAMEN, las = 2, horiz = T, 
        col = alpha("navy", alpha = 0.6), # color con función alpha para transparentar
        main = "IMC promedio por departamento")
lines(rep(mean(endes$IMC, na.rm=T),2), c(0,100), col = "red", 
      lty = 2, # tipo de linea
      lwd = 3) # ancho de linea

# 4
hist(endes$HW53, main = "Distribución del nivel de hemoglobina", freq = F, xlab = "Hemoglobina")
lines(density(endes$HW53, na.rm = T))

dev.off()



## Paquete ggplot2 -------------------------------------------------------------

browseURL("https://diegokoz.github.io/intro_ds/fuentes/ggplot2-cheatsheet-2.1-Spanish.pdf")

### Scatterplot ----
ggplot(endes) + 
  geom_point(aes(x = HW1, y = HW3)) +
  xlab("Edad") + ylab("Altura") + 
  ggtitle("Altura de menores de 5 años por edad", subtitle = "2016-2022")

# Con jitter (dispersión aleatoria)
ggplot(endes) + 
  geom_jitter(aes(x = HW1, y = HW3), width = 0.6) + 
  xlab("Edad") + ylab("Altura") + 
  ggtitle("Altura de menores de 5 años por edad", subtitle = "2016-2022")


### Dotchart ----
ggplot(datos_por_edad) + 
  geom_point(aes(x = HW2, y = HW1)) + 
  scale_y_continuous(breaks = seq(0,60,2)) +
  xlab("Peso") + ylab("Edad (en meses)") + 
  ggtitle("Peso de menores de 5 años por edad", subtitle = "2016-2022")

# Con rango de valores máximo y mínimo
endes %>% 
  subset(!is.na(HW1)) %>% 
  group_by(HW1) %>% 
  summarise(MIN = min(HW2, na.rm=T),
            MAX = max(HW2, na.rm=T),
            MEAN = mean(HW2, na.rm=T)) %>% 
  pivot_longer(cols = c("MIN","MAX"), names_to = "Dato", values_to = "HW2") %>% 
  ggplot() +
  geom_line(aes(x = HW2, y = HW1, group = HW1)) +
  geom_point(aes(x = HW2, y = HW1, colour = Dato)) +
  geom_point(aes(x = MEAN, y = HW1), colour = "black", fill = "white", shape = 21, size = 3) +
  xlab("Peso") + ylab("Edad (en meses)") + 
  ggtitle("Peso de menores de 5 años por edad", subtitle = "2016-2022")



### Gráfico de línea ----
ggplot(datos_por_edad) + 
  geom_line(aes(x = HW1, y = HW3)) + 
  xlab("Edad") + ylab("Altura") + 
  ggtitle("Altura de menores de 5 años por edad", subtitle = "2016-2022")

# Combinación línea con scatter plot
ggplot() +
  geom_jitter(data = endes, aes(x = HW1, y = HW3), width = 0.6) + 
  geom_line(data = datos_por_edad, aes(x = HW1, y = HW3), colour = "red", size = 2.5) + 
  xlab("Edad") + ylab("Altura") + 
  ggtitle("Altura de menores de 5 años por edad", subtitle = "2016-2022")



### Barplot ----

# Función que realiza el conteo
ggplot(endes) + 
  geom_bar(aes(x = ID1), colour = "black", fill = "grey30") + 
  scale_x_continuous(n.breaks = 7) +
  xlab("Año") + ylab("Número de casos") +
  ggtitle("Tamaño de muestra por año")


# Función que toma los datos como de una tabla
ggplot(imc_departamen) + 
  geom_col(aes(x = IMC, y = DEPARTAMEN)) + 
  xlab("IMC") + ylab("Departamento") +
  ggtitle("IMC promedio por departamento")

# Con línea vertical
ggplot(imc_departamen) + 
  geom_col(aes(x = IMC, y = DEPARTAMEN)) + 
  geom_vline(xintercept = mean(endes$IMC, na.rm=T), color = "red", linewidth = 1.5, linetype = "dashed") +
  xlab("IMC") + ylab("Departamento") +
  ggtitle("IMC promedio por departamento")


### Boxplot ----
ggplot(endes) + 
  geom_boxplot(aes(x = HW53, y = area)) +
  xlab("Hemoglobina") + ylab("") +
  ggtitle("Distribución de la anemia")

# Girado
ggplot(endes) + 
  geom_boxplot(aes(x = HW53, y = area)) +
  coord_flip() +
  xlab("Hemoglobina") + ylab("") +
  ggtitle("Distribución de la anemia")



### Histograma ----
ggplot(endes) + 
  geom_histogram(aes(x = HW53)) +
  xlab("Hemoglobina") + ylab("Conteo") +
  ggtitle("Distribución del nivel de hemoglobina")

# Con cambio de ancho de los bins
FD_val <- 2*IQR(endes$HW53, na.rm = T)/(length(endes$HW53[!is.na(endes$HW53)])^(1/3))
S_val <- 1 + log(length(endes$HW53[!is.na(endes$HW53)]),2)

ggplot(endes) + 
  geom_histogram(aes(x = HW53, y = ..density..), binwidth = FD_val) +
  xlab("Hemoglobina") + ylab("Porcentaje") +
  ggtitle("Distribución del nivel de hemoglobina")

# Density plot
ggplot(endes) + 
  geom_density(aes(x = HW53, y = ..density..)) +
  xlab("Hemoglobina") + ylab("Porcentaje") +
  ggtitle("Distribución del nivel de hemoglobina")


# Unión de ambos
ggplot(endes) + 
  geom_histogram(aes(x = HW53, y = ..density..), position="identity", bins = 30) +
  geom_density(aes(x = HW53, y = ..density..), colour = "blue") +
  xlab("Hemoglobina") + ylab("Porcentaje") +
  ggtitle("Distribución del nivel de hemoglobina")


### Función propia -------------------------------------------------------------

plot.dist <- function(m, v, stitle = "", hfill = "#50115F", hcol = "grey10", halpha = 0.5, ttsize = 24, atxsize = 18, atsize = 12, stsize = 16){
  m <- m[!is.na(m[[v]]),]
  bsize <- 2*IQR(m[[v]])/length(m[[v]])^(1/3)
  meanx <- mean(m[[v]])
  ttitle <- paste(unique(c(min(m$HW1, na.rm = T), max(m$HW1, na.rm = T))), collapse = " a ")
  stitle <- ifelse(stitle=="", paste0("De ",ttitle," meses"), paste0("De ",ttitle," meses - ",stitle))
  stsize <- ifelse(stitle == "", 0, stsize)
  ggplot(m, aes(x = get(v))) +
    geom_segment(aes(x = meanx, xend = meanx, y = 0, yend = Inf), linetype = 2, colour = hfill, size = 1) +
    geom_histogram(aes(y = ..density..), binwidth = bsize, fill = hfill, colour = hcol, alpha = halpha, size = 1) +
    geom_density(size = 1) +
    scale_x_continuous(breaks = seq(70,170,10)) +
    xlab("Hemoglobina") + ylab("") +
    ggtitle(get_label(m[[v]]), subtitle = stitle) +
    ggpubr::theme_pubclean() +
    theme(axis.text = element_text(size = atsize, colour = "black"),
          axis.title.x = element_text(size = atxsize),
          title = element_text(size = ttsize),
          plot.subtitle = element_text(size = stsize))
}


# Bases para probar la función
endes6_8 <- subset(endes, HW1 >= 6 & HW1 <= 8) # Subgrupo de 6 a 8 meses
endes9_11 <- subset(endes, HW1 >= 9 & HW1 <= 11) # Subgrupo de 9 a 11 meses
endes12_14 <- subset(endes, HW1 >= 12 & HW1 <= 14) # Subgrupo de 12 a 14 meses
endes6_14 <- subset(endes, HW1 >= 6 & HW1 <= 14) # Subgrupo de 6 a 14 meses

# Generación de gráfico
plot.dist(endes6_8, "HW53", ttsize = 12, stsize = 10, atxsize = 10)

# Almacenar gráficos
g1 <- plot.dist(endes6_8, "HW56", ttsize = 12, stsize = 10, atxsize = 10)
g2 <- plot.dist(endes9_11, "HW56", ttsize = 12, stsize = 10, atxsize = 10)
g3 <- plot.dist(endes12_14, "HW56", ttsize = 12, stsize = 10, atxsize = 10)
g4 <- plot.dist(endes6_14, "HW56", ttsize = 12, stsize = 10, atxsize = 10)

# Plotear gráficos juntos
gridExtra::grid.arrange(g1,g2,g3,g4,ncol=2)

