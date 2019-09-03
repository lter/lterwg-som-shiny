library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)



# #load tarball rds
tarball <- readRDS("somCompositeData_2019-08-27.rds")

#load control only function
source('ext_ftns/control_filter.R', chdir=T)