# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu


### BRING IN BETTER VAR NAMES, create lookup table csv from keykey to convert colummn names to full names

#Create UI option vectors
exp.types <- c(unique(na.omit(tarball$tx_L1_level)), 
               unique(na.omit(tarball$tx_L2_level)),
               unique(na.omit(tarball$tx_L3_level)),
               unique(na.omit(tarball$tx_L4_level)),
               unique(na.omit(tarball$tx_L5_level)),
               unique(na.omit(tarball$tx_L6_level))
               )
exp.types <- union(exp.types,exp.types) #removes duplicates from the vector

networks <- unique(tarball$network)

#List of site names
site.names <- sort(unique(tarball$location_name))

# create vectors of tarball numeric and string column names for plotting
# options; this code is duplicated in both the ui and server functions as the
# map function needs these values from server whereas the rest of the map needs
# these from ui
som.numerics <- select_if(tarball, is.numeric) %>% colnames() %>% sort()
som.strings <- select_if(tarball, is.character) %>% colnames() %>% sort()

### UI ###
ui <- fluidPage(
  # javascript code for google analytics. if app gets too big, we can move this to it's own script
  # and use includeScript inside tags$head
  tags$head(
    #shiny::includeHTML("google-analytics.html")
    # javascript code for google analytics. if app gets too big, we can move this to it's own script
    # and use includeScript inside tags$head
    HTML(
      "<!-- Google Analytics -->
        <script>
          (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
          m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
          })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
        
          ga('create', 'UA-148614327-1', 'auto');
          ga('send', 'pageview');
       
        
        </script>"
    )
  ),
  theme = "bootstrap.css",
  tags$style(
    HTML(
      "
      .navbar-default .navbar-brand {color: white;font-size: 22px;font-family: sans-serif;}
      .navbar-default .navbar-brand:hover {color: #3a3a3a;}
      .navbar { background-color: #3a3a3a;}
      .navbar-default .navbar-nav > li > a {color:white;font-size: 16px;}
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #222222;font-size: 18px;outline: 0;}
      .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#222222;font-size: 17px;}
      "
    )
  ),
  useShinyjs(),
  navbarPage(
    a("LTER SOM", href = "https://lter.github.io/som-website/",  style = "color:white"),
    windowTitle = "LTER SOM Database",
    tabPanel(
      "Query",
      fluidRow(column(
        12,
        offset = 0,
        titlePanel(title = "LTER SOM Database")
      )),
      hr(),
      
      
      # data filters ------------------------------------------------------------
      
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
        column(5,
               wellPanel(
                 sliderInput(
                   "top_d",
                   "Top depth of soil layer (cm):",
                   min = -3,
                   max = 300,
                   value = c(-3,300)
                 ),
                 sliderInput(
                   "bot_d",
                   "Bottom depth of soil layer (cm):",
                   min = -3,
                   max = 300,
                   value = c(-3,300)
                 ),
                 hr(),
                 h5("Sliders expanded to their full range will include all available data.", 
                    style="color:#85929E"),
                 h5("To filter data by an exact soil layer (e.g. 0-10 cm), move sliders to overlap 
                    on a single value (e.g. Top = 0, Bottom = 10)", 
                    style="color:#85929E")
               ))
      ),
      hr(),
      pageWithSidebar(
        h2("Output"),
        sidebarPanel(
          
          
          # plot data controls ------------------------------------------------------
          
          conditionalPanel(
            condition = "input.conditionedPanels==1",
            h3("Plot variables"),
            selectInput(
              inputId = 'plot.x',
              label = 'Plot X-Axis:',
              choices = c(som.numerics, som.strings),
              selected = "google_dir"
            ),
            selectInput(
              inputId = 'plot.y',
              'Plot Y-Axis:',
              choices = c(som.numerics, som.strings),
              selected = "lyr_soc"
            ),
            hr(),
            h3("Plot options"),
            selectInput(
              inputId = 'plot.type',
              'Type:',
              choices = c("point", "boxplot", "histogram"),
              selected = "point"
            ),
            selectInput(
              inputId = 'plot.color',
              'Color:',
              choices = c("None", som.numerics, som.strings),
              selected = "None"
            ),
            selectInput(
              'plot.symbol',
              'Symbolize:',
              choices = c("None", "network", "site_code"),
              selected = "None"
            )
          ),
          
          
          # map data controls -------------------------------------------------------
          
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
          
          
          # download data button ----------------------------------------------------
          
          conditionalPanel(
            condition = "input.conditionedPanels==3",
            h3("Data table"),
            hr(),
            downloadButton("downloadData", "Download data", onclick = "ga('send', 'event', 'click', 'download data')")
          )
        ),
        
        
        # query main panel --------------------------------------------------------
        
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", value = 1, plotOutput("dataPlot")),
            tabPanel("Map", value = 2, leafletOutput("som_map")),
            tabPanel("Table", value = 3, DTOutput('tbl')),
            id = "conditionedPanels"
          )
        )
      )
    ),
    
    
    # data summary ------------------------------------------------------------
    
    tabPanel("Data Summary",
             tabsetPanel(
               tabPanel(
                 "By Analytes",
                 hr(),
                 fluidRow(
                   column(
                     width = 3,
                     selectInput('var1_n',
                                 'Analyte:',
                                 choices = som.numerics,
                                 selected = "lyr_soc")
                   ),
                   column(
                     width = 3,
                     selectInput(
                       'var2_n',
                       'Analyte:',
                       choices = c("", som.numerics),
                       selected = ""
                     )
                   ),
                   column(
                     width = 3,
                     selectInput(
                       'var3_n',
                       'Analyte:',
                       choices = c("", som.numerics),
                       selected = ""
                     )
                   ),
                   column(
                     width = 3,
                     selectInput(
                       'var4_n',
                       'Analyte:',
                       choices = c("", som.numerics),
                       selected = ""
                     )
                   ),
                   checkboxInput("var_ex.ALL", "Exclude if data missing for ALL variables", TRUE),
                   checkboxInput("var_ex.ANY", "Exclude if data missing for ANY variable", FALSE),
                   hr(),
                   DTOutput('var_n_tbl')
                 )
               ),
               tabPanel(
                 "By Site",
                 hr(),
                 fluidRow(
                   column(
                     width = 3,
                     selectInput('site.varn',
                                 'Site:',
                                 choices = site.names,
                                 selected = ""),
                     checkboxInput("sitevar_ex.loc", "Exclude location level data", FALSE),
                     checkboxInput("sitevar_ex.prof","Exclude profile (layer, etc.) level data",FALSE),
                     checkboxInput("sitevar_ex.class", "Exclude character class data", FALSE)
                   ),
                   #column(
                   # width = 0.2),
                 ),
                 hr(),
                 fluidRow(column(DTOutput('site_varn_tbl'), width = 8))
               )
             )),
    tabPanel("Data Key",
             tabsetPanel(
               tabPanel("Location",
                        DTOutput('var_info.loc')),
               tabPanel("Profile",
                        DTOutput('var_info.prof'))
             )),
    
    
    # comments via GitHub -----------------------------------------------------
    
    tabPanel(
      "Comments",
      h1("Give us feedback on the app!"),
      textInput("issueTitle", label = "Title"),
      textAreaInput("issueBody", label = "Body", height = '300px'),
      textInput("name", label = "Name:"),
      textInput("email", label = "Email:"),
      actionButton("issueSubmit", label = "Submit"),
      actionButton("clearIssue", label = "Clear All Fields"),
      shinyjs::hidden(
        p(id = 'allIssues', 'Please fill out all values!',
          style = 'color: gray')
      ),
      shinyjs::hidden(
        p(
          id = "issueSuccess",
          "Issue successfully submitted!",
          style = "color: green"
        )
      )
    )
    
  ),
### app versioning ###
# Last updated on commit by Derek Pierson: April 21, 2020
  column(
    width = 12,
    style = "text-align: right; color:grey; font-size: 9px; padding-top: 10px;",
    p("Shiny SoDaH Version: 1.06")
  )
)
