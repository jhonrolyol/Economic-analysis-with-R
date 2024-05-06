
#------------------------------------------------------------------------------#
# Sesion 1: Introducción al manejo de datos en R                               #
# Profesor: Miguel Zambrano                                                    #
# Tudesarollo Consultores                                                      #
#------------------------------------------------------------------------------#


# Estructuras en R --------------------------------------------------------

## Tipos de objetos ----

# Entero / Integer
a <- 1
class(a)
a <- as.integer(a)

# Numerico / Numeric
b <- 1.0
class(b)

# Caracter / Character
c <- "uno"
class(c)

# Factor
d <- factor(1)
d + 2 # ¿Se puede operar?
class(d)

# Logico / Logical
e <- T
e + 2 # ¿Se puede operar?
class(e)

# Complejo / Complex
f <- 1i
class(f)

# Nulo / Null
g <- NULL
class(g)

# Valor perdido / Missing value
h <- NA
class(h)


## Tipos de estructuras ----

browseURL("https://www.researchgate.net/profile/Luis-Lopez-72/publication/348730217/figure/fig3/AS:983458872573957@1611486275916/Figura-9-Estructuras-de-datos-representadas-como-bloques.ppm")

# Vector
v1 <- c(1,2,3,5,7,11)                   # Listado de valores
v2 <- seq(from = 1, to = 11, by = 2)    # Secuencia de valores
v3 <- 1:10                              # Secuencia simple
v4 <- c(c(1,3,5,7),c(2,4,6,8))          # Combinacion de dos vectores
v5 <- c("uno", "dos", "tres", "cuatro") # Puede contener cualquier tipo
v6 <- c(T,F,T,T,F)
v7 <- c(1,2,3,5,T,11)                   # Si combinamos tipo, R coerciona al resto de objetos
v8 <- c(1,2,3,5,"siete",11)
v9 <- c(1,2,3,5,7+2i,11)
v10 <- c(1,2,3,5,NULL,11)               # ¿Qué pasa con NA y NULL?
v11 <- c(1,2,3,5,NA,11)


# Matriz
m1 <- matrix(data = 1:9, nrow = 3)              # A partir de un vector
m2 <- matrix(data = 1:9, nrow = 3, byrow = T)   # Creación por filas
a0 <- 1:3
b0 <- 4:6
c0 <- 7:9
m3 <- matrix(c(a0,b0,c0), nrow = 3)             # A partir de vectores
m4 <- matrix(c(v1,v8), nrow = 6)                # También coerciona el tipo de elementos


# Array
a1 <- array(1:27, c(3,3,3))   # Tres dimensiones: cubo
a2 <- array(1:27, c(3,3,4))   # Prisma rectangular


# Data Frame
d1 <- data.frame(m1)    # A partir de una matriz
# Construir matriz a partir de variables
set.seed(1234) # Semilla
d2 <- data.frame(id = gsub("\\.", "", format(c(1:10)/1000, digits = 3)),
                 altura = round(runif(10, min = 1.4, max = 1.9),2),
                 edad = as.integer(round(runif(10, min = 15, max = 80))),
                 est_civil = sample(c("SOLTERO/A","CASADO/A"), size = 10, replace = T),
                 empleado = sample(c(T,F), size = 10, replace = T))


# Lista
l1 <- list(m1,m2,m3)
l2 <- list(m1,a1,d1)
l3 <- list(d1,NULL)


## Operadores importantes ----
a <- 7

# Matemáticos
a + 5   # Suma
a - 5   # Resta
a * 5   # Multiplicación
a / 5   # División
a ^ 5   # Potencia
a %% 5  # Obtener residuo
a %/% 5 # Obtener cociente

# ¿Qué pasa si se aplica a una matriz?
m1 + 5
m1 * 5
m1 %% 5
m1 * m2
m1 ^ 2

# Operadores matemáticos de matrices
m1 %*% m2 # Producto de matrices
m1 %x% m2 # Producto de Kronecker
m1 %o% m2 # Producto de externo

# A partir de otros paquetes
#install.packages("expm")
library(expm)
m1 %^% 2 # Potencia de matriz

# ¿Otras operaciones?
t(m1)                     # Transpuesta
det(m1)                   # Determinante
solve(m1)                 # Inversa
diag(m1)                  # Diagonal
eigen(m1)$values          # Valor propio / eigenvalues
Matrix::rankMatrix(m1)    # Rango

# Operadores de comparación
a <- 6
b <- c(3,8,10)

a == b    # Igual
a != b    # No igual
a > b     # Mayor
a >= b    # Mayor igual
a < b     # Menor
a <= b    # Menor igual
a %in% b  # Contenido en

# Operadores lógicos
a <- c(T,T,F,T)
b <- c(F,T,T,F)

!a        # Negación
a & b     # Y
a | b     # O
a && b    # Y (solo escalares)
a || b    # O (solo escalares)



# Operaciones con data frames --------------------------------------------------

# Observar variables
d2$id       # Usando $
d2[["id"]]  # Usando [[]]
d2[,"id"]   # Como índice
d2[,1]      # Como índice numérico


# Crear nueva variable
d2$area <- sample(c("urbano","rural"), size = 10, replace = T)
d2[["educacion"]] <- sample(c("primaria","secundaria","superior"), size = 10, replace = T)
d2[,"peso"] <- round(runif(10, min = 44, max = 90),1)

# Eliminar variable
d2[,"area"] <- NULL
d2 <- d2[,-7]
d2 <- d2[,1:5]

# Cambiar nombre de variable
names(d2)                 # Visualizar lista de nombres
names(d2)[2]              # Seleccionar nombre específico
names(d2)[2] <- "talla"   # Asignar nuevo nombre


# Cambiar valores
d2[2,"est_civil"] <- "SOLTERO/A"        # Selección arbitraria por índice
d2[d2$edad > 65, "empleado"] <- FALSE   # Selección por condición lógica


# Cambiar orden de las variables
d2 <- d2[,c(1,3,2,8,4,7,6,5)]
d2[,sort(names(d2))]


# Operaciones con loops
for (x in 1:ncol(d2)){    # A partir de un vector de numeros
  print(class(d2[,x]))
}

for (x in names(d2)){    # A partir de un vector de caracteres
  print(class(d2[[x]]))
}

# Funcion apply
apply(d2, 2, FUN = class)
lapply(d2, function(x) class(x))


# If else statements
x <- 9
if (x > 10){
  print("Sí es mayor")
}

if (x > 10){
  print("Sí es mayor")
} else {
  print("Es menor o igual")
}

x <- c(1,2,3,4)
if (any(is.na(x))){
  print("No hacer nada")
} else {
  print(sum(x))
}


# Modificar variables usando loops y condicionales
for (x in 1:nrow(d2)){
  if (d2[x,"edad"] < 35 & d2[x,"est_civil"] == "CASADO/A"){
    d2[x,"aplica_bono"] <- TRUE
  } else {
    d2[x,"aplica_bono"] <- FALSE
  }
}


# Crear funciones
imc <- function(data){
  val <- data[["peso"]]/(data[["talla"]]^2)
  return(val)
}

tiene.infrapeso <- function(data){
  imc(data) <=18.49
}

d2$IMC <- imc(d2)
d2$infrapeso <- tiene.infrapeso(d2)




