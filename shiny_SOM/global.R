library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)

library(httr) # to connect with github api for comments
library(jsonlite) # to write issues into json

library(shinyjs)

# #load tarball rds
tarball <- readRDS("somCompositeData_2019-09-14.rds")

#load control only function
source('ext_ftns/control_filter.R', chdir=T)


# link to github api issues
issues_url <- "https://api.github.com/repos/lter/lterwg-som-shiny/issues"


