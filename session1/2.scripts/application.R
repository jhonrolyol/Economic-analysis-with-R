#------------------------------------------------------------------------------#
#                        ECONOMIC ANALYSIS WITH R
#
#                  Introduction to data management in R.
#
#                           Jhon R. Ordoñez 
#                           April 21, 2024
#
#------------------------------------------------------------------------------#

# We clean the work environment -------------------------------------------
  rm(list = ls())

# Shortcuts ---------------------------------------------------------------
  # ctrl + s: save
  # ctrl + shift + r: new section
  # ctrl + shift + p: show command palette
  # Alt + shift + k: keyboard Shortcuts Help
  # ...

# Install packages --------------------------------------------------------
  #install.packages("readxl")
  #install.packages("ggplot2")
  #install.packages("deSolve")
  #install.packages("nortest")
  #install.packages("tseries")
  #install.packages("FinTS")
  #install.packages("RcmdrMisc")
  #install.packages("pastecs")
  #install.packages("xts")

# load libraries ----------------------------------------------------------
  library(readxl)
  library(ggplot2)
  library(deSolve)
  library(nortest)
  library(tseries)
  library(FinTS)
  library(RcmdrMisc)
  library(pastecs)
  library(xts)

# Create folders from Rstudio ---------------------------------------------
  dir.create("1.slides")
  dir.create("2.scripts")
  dir.create("3.raw_data")
  dir.create("4.processed_data")
  dir.create("5.results")
    dir.create("5.results/5.1.tables")
    dir.create("5.results/5.2.figures")
  dir.create("6.analysis")
 

# Structures in R ---------------------------------------------------------
  # types of objects
    # Integer
      a <- 1
      class(a)
      a <- as.integer(a)
      class(a)
    # Numeric
      b <- 1.0
      class(b)
    # Character
      c <- "one"
      class(c)
    # Factor
      d <- factor(1)
      d + 2 # Can it be operated?
      class(d)
    # Logical
      e <- T
      e + 2 # Can it be operated?
      class(e)
    # Complex
      f <- 1i
      class(f)
    # Null
      g <- NULL
      class(g)
    # Missing value
      h <- NA
      class(h)
      
  # Types of structures
    # Vector
      v1 <- c(1,2,3,5,7,11)                   
      v2 <- seq(from = 1, to = 11, by = 2)   
      v3 <- 1:10                            
      v4 <- c(c(1,3,5,7),c(2,4,6,8))          
      v5 <- c("uno", "dos", "tres", "cuatro") 
      v6 <- c(T,F,T,T,F)
      v7 <- c(1,2,3,5,T,11)                  
      v8 <- c(1,2,3,5,"siete",11)
      v9 <- c(1,2,3,5,7+2i,11)
      v10 <- c(1,2,3,5,NULL,11)       
      v11 <- c(1,2,3,5,NA,11)
    # Matrix
      m1 <- matrix(data = 1:9, nrow = 3)            
      m2 <- matrix(data = 1:9, nrow = 3, byrow = T)
      a0 <- 1:3
      b0 <- 4:6
      c0 <- 7:9
      m3 <- matrix(c(a0,b0,c0), nrow = 3)       
      m4 <- matrix(c(v1,v8), nrow = 6)
    # Array
      a1 <- array(1:27, c(3,3,3)) 
      a2 <- array(1:27, c(3,3,4))
    # Data frame
      d1 <- data.frame(m1)
      set.seed(1234)
      d2 <- data.frame(id = gsub("\\.", "", format(c(1:10)/1000, digits = 3)),
                       altura = round(runif(10, min = 1.4, max = 1.9),2),
                       edad = as.integer(round(runif(10, min = 15, max = 80))),
                       est_civil = sample(c("SOLTERO/A","CASADO/A"), 
                                          size = 10, replace = T),
                       empleado = sample(c(T,F), size = 10, replace = T))
    # Lists
      l1 <- list(m1,m2,m3)
      l2 <- list(m1,a1,d1)
      l3 <- list(d1,NULL)
      
  # Important operators
    
  
