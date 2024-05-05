
#------------------------------------------------------------------------------#
# Sesion 5: Gráficos en 3D                                                     #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#


rm(list=ls())

library(rgl)
library(plot3D)
library(tidyverse)
library(sjlabelled)
library(rvest)

setwd("D:/Trabajo/Otros/Dictado TuDesarrollo/semana3")

# Grafico con datos del BCRP
url <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/anuales/resultados/PM05197PA/html"

inflacion <- read_html(url) %>% 
  html_element("table") %>% 
  html_element("table") %>% 
  html_table() %>% 
  setNames(c("Periodo", "Inflacion")) %>% 
  mutate(Inflacion = as.numeric(gsub(",",".",Inflacion))) %>% 
  mutate(Fecha = as.Date(paste0(Periodo,"-01-01")))

inflacion %>% subset(Periodo >= 1995) %>% 
  mutate(superior = ifelse(2 > Inflacion, 2, Inflacion),
         inferior = ifelse(2 < Inflacion, 2, Inflacion)) %>% 
ggplot(aes(x=Fecha,y=Inflacion))+
  geom_rect(inherit.aes=F, aes(xmin=as.Date("2002-01-01"), xmax=as.Date("2023-01-01"), ymin=-Inf, ymax=+Inf), fill='grey80', alpha=1) +
  geom_line(color="black") + 
  geom_line(linetype=2,aes(y=rep(2,29))) + 
  geom_ribbon(aes(ymin=rep(2,29),ymax=superior),fill="#d73027",alpha=0.5) +
  geom_ribbon(aes(ymin=rep(2,29),ymax=inferior),fill="#4575b4",alpha=0.5) +
  scale_x_date(date_breaks="5 years",date_labels="%Y") +
  ggpubr::theme_pubr() +
  ggtitle("Inflación anual a final del periodo", subtitle = "1995-2023")




# Graficos en 3D
data <- data.frame(x = rnorm(1000), 
                   y = runif(1000,-4,4),
                   z = rpois(1000, 1))

plot3d(data$x, data$y, data$z)


# Graficando un modelo de inferencia
# Cargar la ENAHO
enaho <- haven::read_dta("data/enaho/enaho01a-2019-500.dta")

# Estudiaremos la ecuación de Mincer:
# Salario = f(Nivel de escolaridad, Experiencia profesional)

# Variable de nivel educativo
sjlabelled::get_labels(enaho$p301a)
table(enaho$p301a)

# Creación del conjunto de datos
set.seed(1234)
data <- enaho %>% select(p524a1, p301a, p208a) %>% 
  subset(!is.na(p524a1)) %>% 
  subset(!is.na(p301a)) %>% 
  subset(p301a != 12) %>% 
  subset(p208a >= 20) %>% 
  mutate(S = case_when(p301a == 1 ~ 0,
                       p301a == 2 ~ sample(1:3,n(),replace=T,prob=c(0.15,0.35,0.5)),
                       p301a == 3 ~ sample(4:7,n(),replace=T),
                       p301a == 4 ~ 8,
                       p301a == 5 ~ sample(9:13,n(),replace=T),
                       p301a == 6 ~ 14,
                       p301a == 7 ~ sample(15:16,n(),replace=T),
                       p301a == 8 ~ 17,
                       p301a == 9 ~ sample(15:18,n(),replace=T),
                       p301a == 10 ~ 19,
                       p301a == 11 ~ sample(20:25,n(),replace=T,prob=c(0.30,0.60,rep(0.025,4))))) %>% 
  setNames(c("W", "N", "X", "S")) %>% 
  mutate(X2 = X^2,
         S2 = S^2)

# Observemos los resultados
table(data$S)
table(data$X)


# Algunos modelos:

# Modelo lineal univariado Salario vs. Escolaridad
model1 <- lm(W ~ S, data = data)
plot(W ~ S, data=data,pch=19,cex=0.1)
j <- order(data$S)
lines(data$S[j],model1$fitted[j],col="red",lwd=3)

# Modelo lineal univariado Salario vs. Escolaridad
model2 <- lm(W ~ X, data = data)
plot(W ~ X, data=data,pch=19,cex=0.1)
j <- order(data$X)
lines(data$X[j],model2$fitted[j],col="red",lwd=3)

# Modelo lineal multivariado Salario vs. Escolaridad + Experiencia
model3 <- lm(W ~ S + X, data=data)
plot(W ~ S, data=data,pch=19,cex=0.1)
j <- order(data$S)
lines(data$S[j],model3$fitted[j],col="red",lwd=3)
fitted3 <- as.numeric(matrix(c(rep(1,nrow(data)), data$S, rep(mean(data$X),nrow(data))),nrow=nrow(data)) %*% matrix(model3$coefficients, nrow=3))
lines(data$S[j],fitted3[j],col="green",lwd=3)

# Ploteo en 3d
grid.lines = 15
x.pred <- seq(min(data$S), max(data$S), length.out = grid.lines)
y.pred <- seq(min(data$X), max(data$X), length.out = grid.lines)
X_mat <- expand.grid(S = x.pred, X = y.pred)
z.pred <- matrix(predict(model3, newdata = X_mat), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(data$S, data$X, data$W,
          pch = 19, cex = 0.5, 
          theta = 60, phi = 30,
          ticktype = "detailed",
          xlab = "S", ylab = "X", zlab = "W",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA), main = "Ecuación Mincer")

plot3d(model3)


# Modelo lineal multivariado Salario vs. Experiencia + Experiencia^2
model4 <- lm(W ~ X + X2, data=data)
plot(W ~ X, data=data,pch=19,cex=0.1)
j <- order(data$X)
lines(data$X[j],model4$fitted[j],col="red",lwd=3)

# Propuesta para no linearidad
# Modelo LOESS univariado Salario vs. Escolaridad
model11 <- loess(W ~ S,data=data)
plot(W ~ S, data=data,pch=19,cex=0.1)
j <- order(data$S)
lines(data$S[j],model11$fitted[j],col="red",lwd=3)

# Modelo LOESS univariado Salario vs. Escolaridad
model22 <- loess(W ~ X,data=data)
plot(W ~ X, data=data,pch=19,cex=0.1)
j <- order(data$X)
lines(data$X[j],model22$fitted[j],col="red",lwd=3)

# Modelo lineal multivariado Salario vs. Escolaridad + Experiencia + Experiencia^2
model5 <- lm(W ~ S + X + X2, data=data)
plot(W ~ X, data=data,pch=19,cex=0.1)
j <- order(data$X)
lines(data$X[j],model5$fitted[j],col="red",lwd=3)
fitted5 <- as.numeric(matrix(c(rep(1,nrow(data)), rep(mean(data$S),nrow(data)), data$X, data$X2),nrow=nrow(data)) %*% matrix(model5$coefficients, nrow=4))
lines(data$X[j],fitted5[j],col="green",lwd=3)

# Ploteo en 3d
grid.lines = 15
x.pred <- seq(min(data$S), max(data$S), length.out = grid.lines)
y.pred <- seq(min(data$X), max(data$X), length.out = grid.lines)
X_mat <- expand.grid(S = x.pred, X = y.pred)
X_mat <- X_mat %>% mutate(X2 = X^2)
z.pred <- matrix(predict(model5, newdata = X_mat), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(data$S, data$X, data$W,
          pch = 19, cex = 0.5, 
          theta = 65, phi = 0,
          ticktype = "detailed",
          xlab = "S", ylab = "X", zlab = "W",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA), main = "Ecuación Mincer")

model51 <- lm(W ~ S + X + I(X^2), data=data)
plot3d(model51, col = "red", plane.col = "yellow", plane.alpha = 1)
title3d("Modelo multivariado")
play3d(spin3d(axis = c(0,0,1)), duration = 10)


model6 <- loess(W ~ S + X,data=data)
j <- order(data$S)
i <- order(data$X)
fitted6S <- predict(model6, newdata = matrix(c(data$S, rep(mean(data$X),nrow(data))),nrow=nrow(data)))
fitted6X <- predict(model6, newdata = matrix(c(rep(mean(data$S),nrow(data)), data$X),nrow=nrow(data)))

par(mfrow=c(1,2))
plot(W ~ S, data=data,pch=19,cex=0.1)
lines(data$S[j],fitted6S[j],col="red",lwd=3)
plot(W ~ X, data=data,pch=19,cex=0.1)
lines(data$X[j],fitted6X[j],col="red",lwd=3)


mod.grid <- matrix(c(S = seq(min(data$S),max(data$S),len=20),
                     X = seq(min(data$X),max(data$X),len=20)), nrow=20)
fitted6W <- matrix(predict(model6, newdata = mod.grid),20,20)

plot3d(data$S,data$X,data$W, type="p", col="red", xlab="S", ylab="X", zlab="W")
surface3d(mod.grid[,1], mod.grid[,2], fitted6W, alpha = 0.6, col = "black", back = "lines", front = "lines")
surface3d(mod.grid[,1], mod.grid[,2], fitted6W, alpha = 0.6, col = "yellow")


# Niveles
model7 <- lm(W ~ X + N, data=data)
plot(W ~ X, data=data,pch=19,cex=0.1)
j <- order(data$X)
lines(data$X[j],model7$fitted[j],col="red",lwd=3)
fitted7 <- as.numeric(matrix(c(rep(1,nrow(data)), data$X, rep(mean(data$N),nrow(data))),nrow=nrow(data)) %*% matrix(model7$coefficients, nrow=3))
lines(data$X[j],fitted7[j],col="green",lwd=3)
for(x in unique(data$N)){
  fitted7 <- as.numeric(matrix(c(rep(1,nrow(data)), data$X, rep(x,nrow(data))),nrow=nrow(data)) %*% matrix(model7$coefficients, nrow=3))
  lines(data$X[j],fitted7[j],col=rainbow(length(unique(data$N)))[x],lwd=3)
}
legend(legend=get_labels(enaho$p301a)[1:11],
       col=rainbow(length(unique(data$N))),
       x = c(65, 89.5), 
       y = c(16000, 26000),
       lty = 1,
       y.intersp = 0.2, x.intersp = 0.2)


# Histograma en 3d
range(data$X)
ndata <- data %>% mutate(X = case_when(X <= 30 ~ "20-30",
                                       X > 30 & X <= 45 ~ "31-45",
                                       X > 45 & X <= 60 ~ "46-60",
                                       X > 60 & X >= 75 ~ "61-75",
                                       X > 75 ~ "76-87",)) %>% 
  mutate(N = case_when(N <= 3 ~ "Inicial",
                       N > 3 & N <= 5 ~ "Primaria",
                       N %in% c(6,7,9) ~ "Secundaria",
                       N %in% c(8,10,11) ~ "Superior"))

ndata <- prop.table(table(ndata$X, ndata$N), margin = 1)*100

hist3D(x = 1:4, y = 1:4, z = ndata,
        bty = "g", phi = 30,  theta = -30,
        xlab = "", ylab = "", zlab = "", main = "Edad x Nivel educativo",
        border = "black", shade = 0.8,
        ticktype = "detailed", space = 0.15, cex.axis = 1e-9)
text3D(x = 1:4, y = rep(0.3, 4), z = rep(3, 4),
       labels = rownames(ndata),
       add = TRUE, adj = 0)
# Use text3D to label y axis
text3D(x = rep(0, 4),   y = 1:4, z = rep(0, 4),
       labels  = colnames(ndata),
       add = TRUE, adj = 1)



