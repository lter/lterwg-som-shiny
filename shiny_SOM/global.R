library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)
library(tidyverse)

library(httr) # to connect with github api for comments
library(jsonlite) # to write issues into json
library(readr) # to read the token into R

library(shinyjs)


# load tarball rds
tarball <- readRDS("somCompositeData_2020-06-11.rds")

# load control only function
# source('ext_ftns/control_filter.R', chdir=T)

# load SOM var info csv
var.info <- read.csv("SOM_data_key.csv", as.is=T)


# link to github api issues
issues_url <- "https://api.github.com/repos/lter/lterwg-som-shiny/issues"

# get the private issue token from local file
issue_token <- readr::read_file('machine_git_token.txt')
