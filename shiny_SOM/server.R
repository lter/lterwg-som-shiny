# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu

#Load libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)


### DO WE NEED THIS "setwd" LINE ON THE SERVER COPY?
#set working drive to folder where this script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load tarball rds
tarball <- readRDS("somCompositeData_2019-08-27.rds")
#colnames(tarball)

#load control only function
source('ext_ftns/control_filter.R', chdir=T)

#Create UI option vectors
exp.types <- unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
networks <- unique(tarball$network)

#Create plot variable options vector
som.numerics <- colnames(as.data.frame(select_if(tarball, is.numeric)))
som.strings <- colnames(as.data.frame(select_if(tarball, is.character)))

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
      dsets <- na.omit(as.character(unique(df$google_dir)))
      df <- do.call(rbind, lapply(dsets, ctl_filter, df))
    }
    
    #Time series filter
    if (input$timeseries == "only_ts") {
      df <- df %>% filter(time_series == "YES")
    }
    if (input$timeseries == "no_ts") {
      df <- df %>% filter(time_series == "NO")
    }
    
    #Experiment filter
    if (input$exptype != "ALL") {
      df <- df %>% filter(tx_L1_level == input$exptype) ### NEEDS TO FILTER ACROSS THE OTHER LEVELS
    }
    
    #Top depth filter
    if (input$top_d != "0") {
      df <- df %>% mutate(layer_top = as.numeric(layer_top)) %>% 
        filter(layer_top >= as.numeric(input$top_d)) 
    }
    
    #Bottom depth filter
    if (input$bot_d != "300") {
      df <- df %>% mutate(layer_bot = as.numeric(layer_bot)) %>% 
            filter(layer_bot <= as.numeric(input$bot_d)) 
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
      ) %>% na.omit(input$plot.x) %>% na.omit(input$plot.y)
    
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
  
  
### BEGIN MAP OBJECTS ###
  #Create map dataframe with lat longs and other useful information
  map_pts <- reactive({
    
      #Select data columns and remove NA
      df <- data.tbl() %>% select(
          "google_dir",
          "location_name",
          "site_code",
          "lat",
          "long",
        input$map_color) %>% 
        mutate(lat = as.numeric(lat)) %>% 
        mutate(long = as.numeric(long)) 

      df <- df[!duplicated(df[,c('lat','long')]),]
      
    return(df)
   })
  
  
  #Set column to use for map color
  map_colorby <- reactive({
    df <- map_pts()
    return(unname(unlist(df[,input$map_color])))
  })
  
  #Color palette for map
  map_pal <- reactive({
    map_data <- map_pts()
    
    if(input$map_color %in% som.numerics) {
      pal <- colorNumeric(palette = viridis(100), domain = map_data[,input$map_color])
    } else {
      pal <- colorFactor(palette = viridis(12), domain = map_data[,input$map_color])
    }
    
    return(pal)
  })
  
  #Use input to change base layer
  map_base <- reactive({
    string <- NULL
    if(input$map_base_lyr == "Topographic"){string <- "Esri.WorldStreetMap"}
    if(input$map_base_lyr == "White"){string <- "Stamen.TonerLite"}
    if(input$map_base_lyr == "Relief"){string <- "Esri.WorldPhysical"}
    return(string)
  })
  
  #Create map object
  output$som_map <- renderLeaflet({
    leaflet(map_pts(), options = leafletOptions(minZoom = 1, maxZoom = 11)) %>%
      addProviderTiles(map_base(), options = providerTileOptions(noWrap = TRUE)) %>%
      # setMaxBounds(lng1 = -180, 
      #              lat1 = -90, 
      #              lng2 = 180, 
      #              lat2 = 90) %>%
      addCircleMarkers(lng = ~long, 
                       lat = ~lat, 
                       radius = 5, 
                       color = ~map_pal()(map_colorby()),
                       stroke=FALSE,
                       fillOpacity = 0.8,
                       popup = ~paste("Google directory:", google_dir, "<br>",
                                      "Location name:", location_name, "<br>",
                                      "Latitude:", lat, "<br>",
                                      "Longitude:", long, "<br>")
                       ) %>%
      addLegend("bottomright", pal = map_pal(), values = map_colorby(), labels = "labels", title = "Legend") %>%
      addScaleBar(position = "topright", options = scaleBarOptions(maxWidth = 100, metric = TRUE))
  })
  
### BEGIN DataTable Objects ###
  
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


