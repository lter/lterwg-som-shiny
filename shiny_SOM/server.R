# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu




#Create UI option vectors
exp.types <- unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
networks <- unique(tarball$network)

#Create plot variable options vector
som.numerics <- colnames(as.data.frame(select_if(tarball, is.numeric)))
som.strings <- colnames(as.data.frame(select_if(tarball, is.character)))

### SERVER ###
server <- function(input, output, session) {
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
        input$plot.y,
        {if(input$plot.color != "None")input$plot.color},
        {if(input$plot.symbol != "None")input$plot.symbol}
      ) %>% na.omit(input$plot.x, input$plot.y)
    
    #Return two column df
    return(df)
    
  })
  
  #Create ggplot using filtered data from data.tbl() above
  output$dataPlot <- renderPlot({
    ggplot() +
  #Point plot
    {if(input$plot.type == "point")
      geom_point(data=plot.df(), aes_string(x = input$plot.x, y = input$plot.y))} +
    {if(input$plot.type == "point")
      geom_point(shape = 19, size = 3, alpha = 0.8, stroke = 0.5)} +
    {if(input$plot.symbol != "None" & input$plot.type == "point")
      geom_point(data=plot.df(),aes_string(x = input$plot.x, y = input$plot.y, shape=input$plot.symbol))} +
    {if(input$plot.color != "None" & input$plot.type == "point")
      geom_point(data=plot.df(),aes_string(x = input$plot.x, y = input$plot.y, color=input$plot.color))} +
  #Box plot
    {if(input$plot.type == "boxplot")
      geom_boxplot(data=plot.df(), aes_string(x = input$plot.x, y = input$plot.y))} +   
    {if(input$plot.color != "None" & input$plot.type == "boxplot")
      geom_boxplot(data=plot.df(), aes_string(x = input$plot.x, y = input$plot.y, color=input$plot.color))} +
    {if(input$plot.type == "boxplot")
      stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)} +
  #Histogram
    {if(input$plot.type == "histogram")
      geom_histogram(data=plot.df(), aes_string(x = input$plot.x), stat="count")} + 
    {if(input$plot.color != "None" & input$plot.type == "histogram")
      geom_histogram(data=plot.df(), aes_string(x = input$plot.x, fill=input$plot.color), stat="count")} +
  #Styling
    theme(axis.text.x = element_text(angle = 90))
    #...export plot
    #...dynamic titles
    #...better legend
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
  

  #Site analyte summary table
  output$site_sumry_tbl = renderDT(
    data.tbl(),
    options = list(lengthChange = TRUE,
                   pageLength = 200),
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
  
  
  
  ### BEGIN Comment Box Objects ###

  
  # check if any of the issue fields are blank
  any_null_issues <- reactive({
    c(input$issueTitle, input$issueBody, input$email, input$name) %>%
    lapply(function(x) x == "") %>%
    Reduce(f = any, .)
  })
  
  
  # Create a github issue when someone clicks the issue button
  observeEvent(input$issueSubmit, {
    
    # if any of the fields are blank when they click submit, then we give them a message
    if(any_null_issues()){
      
      shinyjs::show("allIssues")
      shinyjs::disable("issueSubmit")
      
    } else {
      
      # if all the fields are filled out, then submit the issue
      
      # put the text together into json format that the api likes
      json_text <- toJSON(list(
        title = input$issueTitle,
        body = paste(paste0("Name:", input$name), 
                     paste0("Email:", input$email), 
                     input$issueBody
        )
      ),
      auto_unbox = TRUE
      )
      
      # # send the issue to github
      # issue <- httr::POST(issues_url, body = json_text, config = )
      # 
      # if(status_code(issue) == 201){
      #   shinyjs::show("issueSuccess")
      # }
    }
  })
  
  
  # If all the fields are filled out, then hide the message
  observe({
    
    if(!any_null_issues()){
      
      shinyjs::hide("allIssues")
      shinyjs::enable("issueSubmit")
      
    }
  })
  
  
  
  # Clear all inputs on press
  observeEvent(input$clearIssue, {
    c("issueTitle", "email", "name") %>%
      lapply(function(x) updateTextInput(session = session, inputId = x, value = ""))
    
    updateTextAreaInput(session, inputId = "issueBody", value = "")
  })
  
  
  
}


