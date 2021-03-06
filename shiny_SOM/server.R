# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu


#Create UI option vectors
# exp.types <- unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
# networks <- unique(tarball$network)

# create vectors of tarball numeric and string column names for plotting
# options; this code is duplicated in both the ui and server functions as the
# map function needs these values from server whereas the rest of the map needs
# these from ui
som.numerics <- select_if(tarball, is.numeric) %>% colnames() %>% sort()
som.strings <- select_if(tarball, is.character) %>% colnames() %>% sort()

### SERVER ###
server <- function(input, output, session) {
  #reactive function to create data table
  data.tbl <- reactive({
    
    df <- NULL
    
    # filter by network
    if (!"ALL" %in% input$network) {
      df <- tarball %>% filter(network %in% input$network)
    } else {
      df <- tarball
    }
    
    #Control only filter
    if (input$ctl != "ALL") {
      df <- df %>% filter(control_sample == TRUE)
      # dsets <- na.omit(as.character(unique(df$google_dir)))
      # df <- do.call(rbind, lapply(dsets, ctl_filter, df))
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
      df <- df %>% filter(experiments == 'YES') %>% 
        filter(tx_L1_level == input$exptype | 
               tx_L2_level == input$exptype | 
               tx_L3_level == input$exptype | 
               tx_L2_level == input$exptype | 
               tx_L3_level == input$exptype | 
               tx_L4_level == input$exptype)
    }
    
    #Top depth filter
    if (input$top_d[1] != "-3" || input$top_d[2] != "300") {
      df <- df %>% mutate(layer_top = as.numeric(layer_top)) %>% 
        filter(layer_top >= as.numeric(input$top_d[1])) %>% 
        filter(layer_top <= as.numeric(input$top_d[2]))  
    }
    
    #Bottom depth filter
    if (input$bot_d[1] != "-3" || input$bot_d[2] != "300") {
      df <- df %>% mutate(layer_bot = as.numeric(layer_bot)) %>% 
        filter(layer_bot >= as.numeric(input$bot_d[1])) %>% 
        filter(layer_bot <= as.numeric(input$bot_d[2]))  
    }
    
    # remove empty columns from filtered dataframe
    df <- df %>% 
      discard(~all(is.na(.x))) %>%
      map_df(~.x)
    
    ## Return the filtered dataframe
    return(df)
    
  })
  
  
  # update vars available to plot from filtered data frame
  observe({
    
    input$network 
    input$timeseries 
    input$exptype 
    input$top_d
    input$bot_d 
    
    updateSelectInput(session,
                      inputId = "plot.x",
                      choices = data.tbl() %>% colnames() %>% sort(),
                      selected = "google_dir")
    
    updateSelectInput(session,
                      inputId = "plot.y",
                      choices = data.tbl() %>% colnames() %>% sort(),
                      selected = "google_dir")
    
    updateSelectInput(session,
                      inputId = "plot.color",
                      choices = c("None", data.tbl() %>% colnames() %>% sort()),
                      selected = "None")
    
  })
  
  
  # create plot df and select columns
  plot.df <- reactive({
    
    # select data columns and remove NA
    # the call to remove NA was removing desired data, and was omitted
    df <- data.tbl() %>%
      select(
        "google_dir",
        "location_name",
        "site_code",
        "lat",
        "long",
        input$plot.x,
        input$plot.y,
        {if(input$plot.color != "None")input$plot.color},
        {if(input$plot.symbol != "None")input$plot.symbol}
      )
    
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
  
  
  # map ---------------------------------------------------------------------
  
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
  
  ### DataTable Objects 
  
  ## Create user filtered DataTable pulling dataframe from data.tbl() above
  output$tbl = renderDT(
    data.tbl(),
    options = list(lengthChange = TRUE,
                   pageLength = 10),
    class = 'white-space: nowrap'
  )
  
  ## Tarball summary tables
  #Sites with var1, var1, var,3
  var_n.bysite <- reactive({
    
    if(paste0(input$var2_n,input$var3_n,input$var4_n) == "") {
      df <- tarball %>% group_by(site_code, network, location_name) %>%
        summarise(var1_n = sum(!is.na(!!sym(input$var1_n)))) %>%
        setNames(c("Site Code", "Network", "Location Name", input$var1_n))
    } else if (input$var2_n != "" & paste0(input$var3_n,input$var4_n) == "") {
      df <- tarball %>% group_by(site_code, network, location_name) %>%
        summarise(var1_n = sum(!is.na(!!sym(input$var1_n))),
                  var2_n = sum(!is.na(!!sym(input$var2_n)))) %>%
        setNames(c("Site Code", "Network", "Location Name", input$var1_n, input$var2_n))
    } else if (input$var2_n != "" & input$var2_n != "" & input$var4_n == "") {
      df <- tarball %>% group_by(site_code, network, location_name) %>%
        summarise(var1_n = sum(!is.na(!!sym(input$var1_n))),
                  var2_n = sum(!is.na(!!sym(input$var2_n))),
                  var3_n = sum(!is.na(!!sym(input$var3_n)))) %>%
        setNames(c("Site Code", "Network", "Location Name", input$var1_n, input$var2_n, input$var3_n))
    } else {
      df <- tarball %>% group_by(site_code, network, location_name) %>%
        summarise(var1_n = sum(!is.na(!!sym(input$var1_n))),
                  var2_n = sum(!is.na(!!sym(input$var2_n))),
                  var3_n = sum(!is.na(!!sym(input$var3_n))),
                  var4_n = sum(!is.na(!!sym(input$var4_n)))) %>%
        setNames(c("Site Code", "Network", "Location Name", input$var1_n, input$var2_n, input$var3_n, input$var3_n))
    }
    
    #Remove row when all var_n columns are zero
    if(input$var_ex.ALL == TRUE) {
      df <- df[apply(df[c(4:ncol(df))],1,function(z) any(z!=0)),]
    }
    #remove row if any one of the var_n columns is zero
    if(input$var_ex.ANY == TRUE) {
      df <- df[apply(df[c(4:ncol(df))],1,function(z) !any(z==0)),] 
    }
    
    return(df)
  })
  
  output$var_n_tbl = renderDT(
    var_n.bysite(),
    options = list(lengthChange = TRUE,
                   pageLength = 200),
    rownames= FALSE,
    class = 'white-space: nowrap'
  )
  
  #Var n by site
  site.var_n <- reactive({
    df <- tarball %>% group_by(location_name) %>% select_if(is.numeric) %>% summarise_all(funs(sum(!is.na(.)))) %>%
      filter(location_name == input$site.varn)
    
    df.t <- as.data.frame(unlist(df[1,]), stringsAsFactors = F)
    colnames(df.t) <- c("Count")
    
    df.t <- df.t %>% rownames_to_column('var') %>% filter(Count != 0) %>% select(var, Count)
    
    # Match up table var codes with full names and level 
    
    df.t <- df.t %>% inner_join(var.info, by=c("var" = "Column.Name")) %>% 
      select(Variable.Name, var, Level, Class, Count) %>% 
      setNames(c("Variable", "Column Name", "Level", "Class", "Count"))
    
    #Remove location data
    if(input$sitevar_ex.loc == TRUE) {
      df.t <- df.t %>% filter(Level != "location")
    }
    
    #Remove profile data
    if(input$sitevar_ex.prof == TRUE) {
      df.t <- df.t %>% filter(Level == "location")
    }
    
    #Exclude character class data
    if(input$sitevar_ex.class == TRUE) {
      df.t <- df.t %>% filter(Class != "character")
    }
    
    return(df.t)
  })
  
  output$site_varn_tbl = renderDT(
    site.var_n(),
    options = list(lengthChange = TRUE,
                   pageLength = 200),
    rownames= TRUE,
    class = 'white-space: nowrap'
  ) 
  
  #soilHarmonization file popups  !!!Note: this functionality does not work locally, paths are set for use on Cosima server  
  #Notes - find PDF file using site name
  site.dir <- reactive({
    df <- tarball %>% filter(location_name == input$site.varn)
    dir <- unique(df$google_dir)
    return(dir)
  })
  
  #Open Notes PDF on click
  onclick("site_notes", 
          for(dir.i in 1:length(site.dir()))
          {
            runjs(paste0("window.open('/lter-som/HMGZD_notes/",site.dir()[dir.i],"_HMGZD_NOTES.pdf')"))
          }
  )
  
  #Open QC html on click
  onclick("site_qc", 
          for(dir.i in 1:length(site.dir()))
          {
            runjs(paste0("window.open('/lter-som/HMGZD_qc/",site.dir()[dir.i],"_HMGZD_QC.html')"))
          }
  )
  
  
  ## Var info summary tables
  # Location var info tbl
  var_loc.tbl <- var.info %>% filter(Level == "location")
  output$var_info.loc = renderDT(
    var_loc.tbl,
    options = list(lengthChange = TRUE,
                   pageLength = 100),
    rownames= FALSE,
    class = 'white-space: nowrap'
  )
  
  # Profile var info tbl
  var_prof.tbl <- var.info %>% filter(Level != "location")
  output$var_info.prof = renderDT(
    var_prof.tbl,
    options = list(lengthChange = TRUE,
                   pageLength = 200),
    rownames= FALSE,
    class = 'white-space: nowrap'
  )
  
  
  # data download -----------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Filtered_SOM_data.csv")
    },
    content = function(file) {
      write.csv(data.tbl(), file, row.names = FALSE)
    }
  )
  
  
  # comments (as GitHub issue) ----------------------------------------------
  
  # check if any of the issue fields are blank
  any_null_issues <- reactive({
    c(input$issueTitle, input$issueBody, input$email, input$name) %>%
      lapply(function(x) x == "") %>%
      Reduce(f = any, .)
  })
  
  
  # Create a github issue when someone clicks the issue button
  observeEvent(input$issueSubmit, {
    
    # If any of the fields are blank when they click submit, then we give them a message and disable the button
    if(any_null_issues()){
      
      shinyjs::show("allIssues")
      shinyjs::disable("issueSubmit")
      
    } else {
      
      # if all the fields are filled out, then submit the issue
      
      # put the text together into json format that the api likes
      json_text <- toJSON(
        list(
          title = unbox(input$issueTitle),
          body = unbox(
            paste(paste0("Name: ", input$name, "\n"), 
                  paste0("Email: ", input$email, "\n"), 
                  input$issueBody
            )
          ),
          labels = "shiny comment"
        )
      )
      
      # send the issue to github. (user doesn't show up anywhere, but the parameter can't be NULL)
      issue <- httr::POST(issues_url, 
                          body = json_text, 
                          config = authenticate(user = 'user', password = issue_token))
      
      # show some confirmation text if the issue went through
      if(status_code(issue) == 201){
        shinyjs::show("issueSuccess")
      } 
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
  
  
  # close the server function -----------------------------------------------
  
} # close server

