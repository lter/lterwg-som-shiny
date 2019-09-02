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
colnames(tarball)
###NEED TO ADD THE CONTROL ONLY FTN SCRIPT TO REPO
#load control only function
#source() --> make sure it only loads the function

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


### SERVER ###
server <- function(input, output) {
  #reactive function to create data table
  data.tbl <- reactive({
    df <- NULL
    
    #Network filter
    if (input$network != "ALL") {
      df <- tarball %>% filter(network == input$network)
    } else {
      df <- tarball
    }
    
    #Control only filter
    if (input$ctl != "ALL") {
      df <- df[1, 1] #Make this use ctl_only ftn
    }
    
    #Time series filter
    if (input$timeseries != "ALL") {
      df <- df %>% filter(time_series == "YES")
    }
    
    #Experiment filter
    if (input$exptype != "ALL") {
      df <-
        df %>% filter(tx_L1_level == input$exptype) ### NEEDS TO FILTER ACROSS THE OTHER LEVELS
    }
    
    ## Return the filtered dataframe
    return(df)
    
  })
  
  #Create plot df, select columns and remove NA before plotting
  plot.df <- reactive({
    #df <- 
    
    #Select data columns and remove NA
    df <-
      data.tbl() %>% select(
        "google_dir",
        "location_name",
        "site_code",
        "lat",
        "long",
        input$plot.x,
        input$plot.y
      ) %>% na.omit()
    
    #Return two column df
    return(df)
    
  })
  
  #Create ggplot using filtered data from data.tbl() above
  output$dataPlot <- renderPlot({
    ggplot(plot.df(), aes_string(x = input$plot.x, y = input$plot.y)) + geom_point() +
      theme(axis.text.x = element_text(angle = 90))
    #...plot color and symbols
    #...change plot size
    #...dynamic title
    #...legend
    
  })
  
  #Create map df
  
  
  #Create map object
  
  
  #Create user filtered DataTable pulling dataframe from data.tbl() above
  output$tbl = renderDT(
    data.tbl(),
    options = list(lengthChange = TRUE,
                   pageLength = 10),
    class = 'white-space: nowrap'
  )
  
  #Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Filtered_SOM_data.csv")
    },
    content = function(file) {
      write.csv(data.tbl(), file, row.names = FALSE)
    }
  )
}


#Run the app
#shinyApp(ui=ui,server=server)




### Questions for improvement of app:
# DataTable filler for blank values?
# Prevent dataTable rows from expanding?
# Plotly > ggplot?
