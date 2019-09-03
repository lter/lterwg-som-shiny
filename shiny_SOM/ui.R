# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu

#Load libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)

### DO WE NEED THIS "setwd" LINE ON THE SERVER COPY?
#set working drive to folder where this script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load tarball rds
tarball <- readRDS("somCompositeData_2019-08-27.rds")

### BRING IN BETTER VAR NAMES, create lookup table csv from keykey to convert colummn names to full names

#Create UI option vectors
exp.types <-
  unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
networks <- unique(tarball$network)

#Create plot variable options vector
som.numerics <-
  colnames(as.data.frame(select_if(tarball, is.numeric)))
som.strings <-
  colnames(as.data.frame(select_if(tarball, is.character)))

### UI ###
ui <- fluidPage(
  fluidRow(column(
    12,
    offset = 0,
    titlePanel(title = "LTER SOM Database", windowTitle = "LTER SOM Database")
  )),
  hr(),
  h2("Filters"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput(
               'network',
               'Network:',
               choices = c("ALL", networks),
               selected = "ALL",
               multiple = T
             ),
             #HOW TO start with empty box instead of using "ALL"
             selectInput(
               'exptype',
               'Experiment type:',
               choices = c("ALL", exp.types),
               selected = "ALL"
             )
           )),
    column(3,
           wellPanel(
             radioButtons(
               "ctl",
               "Include treatments:",
               c("Include all data" = "ALL",
                 "Control data only" = "CTLONLY"),
               selected = "ALL"
             ),
             radioButtons(
               "timeseries",
               "Include timeseries:",
               c(
                 "Include all data" = "ALL",
                 "Only time series" = "only_ts",
                 "Remove time series" = "no_ts"
               ),
               selected = "ALL"
             )
           )),
    column(3,
           wellPanel(
             sliderInput(
               "top_d",
               "Min soil depth:",
               min = 0,
               max = 100,
               value = 0
             ),
             sliderInput(
               "bot_d",
               "Max soil depth:",
               min = 0,
               max = 300,
               value = 300
             )
           ))
  ),
  hr(),
  pageWithSidebar(
    h2("Output"),
    sidebarPanel(
      conditionalPanel(
        condition = "input.conditionedPanels==1",
        h3("Plot variables"),
        selectInput(
          'plot.x',
          'Plot X-Axis:',
          choices = som.numerics,
          selected = "lyr_soc"
        ),
        selectInput(
          'plot.y',
          'Plot Y-Axis:',
          choices = c(som.numerics, som.strings),
          selected = "google_dir"
        ),
        hr(),
        h3("Plot options"),
        selectInput(
          'plot.type',
          'Type:',
          choices = c("point", "line", "histogram", "boxplot"),
          selected = "point"
        ),
        selectInput(
          'plot.color',
          'Color:',
          choices = c(som.numerics, som.strings),
          selected = "network"
        ),
        selectInput(
          'plot.symbol:',
          'Symbolize:',
          choices = c(som.numerics, som.strings),
          selected = "google_dir"
        )
      ),
      conditionalPanel(
        condition = "input.conditionedPanels==2",
        h3("Map options"),
        selectInput(
          'map_base_lyr',
          'Base layer:',
          choices = c("Topographic", "Relief", "White"),
          selected = "Topographic"
        ),
        selectInput(
          'map_color',
          'Color:',
          choices = c(som.numerics, som.strings),
          selected = "lyr_soc"
        )
      ),
      conditionalPanel(
        condition = "input.conditionedPanels==3",
        h3("Data table"),
        hr(),
        downloadButton("downloadData", "Download data")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", value = 1, plotOutput("dataPlot")),
        tabPanel("Map", value = 2, leafletOutput("som_map")),
        tabPanel("Table", value = 3, DTOutput('tbl')),
        id = "conditionedPanels"
      )
    )
  )
)
