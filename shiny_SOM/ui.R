# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu

#Load libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

### DO WE NEED THIS "setwd" LINE ON THE SERVER COPY?
#set working drive to folder where this script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load tarball rds
tarball <- readRDS("somCompositeData_2019-08-27.rds")

###NEED TO ADD THe SCRIPT TO REPO
#load control only function
#source() --> make sure it only loads the function

### BRING IN BETTER VAR NAMES, create lookup table csv from keykey to convert colummn names to full names

#Create UI option vectors
exp.types <- unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
networks <- unique(tarball$network)

#Create plot variable options vector
som.numerics <- colnames(as.data.frame(select_if(tarball, is.numeric)))
som.strings <- colnames(as.data.frame(select_if(tarball, is.character)))

### UI ###
ui <- fluidPage(
  
      #NEED TITLE
      #CAN WE space the drop down filters not just top to bottom but left to right?      
  
        #Filter options for...
        selectInput('network', 'Network:', choices = c("ALL",networks),
                            selected = "ALL", multiple = T),  #HOW TO start with empty box instead of using "ALL"
        selectInput('ctl', 'Control only:', choices = c("ALL","Control Data Only"),
                    selected = "ALL"),
        selectInput('timeseries', 'Time series:', choices = c("ALL","Time Series Data Only"),
                    selected = "ALL"),
        selectInput('exptype', 'Experiment type:', choices = c("ALL", exp.types), 
                    selected = "ALL"),
        
        #ADD filter by soil depth... maybe as a slider?
        #ADD filter by lat-long...
        
      #NEED UI SEPARATOR HERE
        
        #Download button
        downloadButton("downloadData", "Download this data!"),
        
      #Separator
        
        #Variable options for plot
        selectInput('plot.x', 'Plot X-Axis:', choices = som.numerics, #Make the choices pull from tarball unique
                    selected = "lyr_soc"),
      
        selectInput('plot.y', 'Plot Y-Axis:', choices = c(som.numerics,som.strings), #Make the choices pull from tarball unique
                    selected = "google_dir"),
      
      # ADD slider limits for plot variables...
      # ADD color options for plot...
      # ADD symbol options for plot...

        #Plot object
        plotOutput("dataPlot"), 
        
      #Separator  
        
        #DataTable object    
        DTOutput('tbl')
      )


#Run the app
#shinyApp(ui=ui,server=server)




### Questions for improvement of app:
# DataTable filler for blank values?
# Prevent dataTable rows from expanding?
# Plotly > ggplot?
