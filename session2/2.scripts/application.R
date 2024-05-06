#------------------------------------------------------------------------------#
#                        ECONOMIC ANALYSIS WITH R
#
#                     Introduction to programming in R
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
 
  





















