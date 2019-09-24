# Shiny app for SOM data filtering, plots, and data download
# Created Aug 30, 2019
# Derek Pierson, piersond@oregonstate.edu


### BRING IN BETTER VAR NAMES, create lookup table csv from keykey to convert colummn names to full names

#Create UI option vectors
exp.types <-
  unique(tarball$tx_L1_level) #How to remove unwanted otpions? e.g. NA, L1
networks <- unique(tarball$network)

#List of site names
site.names <- sort(unique(tarball$location_name))


#Create plot variable options vector
som.numerics <-
  sort(colnames(as.data.frame(select_if(tarball, is.numeric))))
som.strings <-
  sort(colnames(as.data.frame(select_if(
    tarball, is.character
  ))))

### UI ###
ui <- fluidPage(
  # javascript code for google analytics. if app gets too big, we can move this to it's own script
  # and use includeScript inside tags$head
  tags$head(
    shiny::includeHTML("google-analytics.html")
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
    a("LTER SOM", href = "https://lter.github.io/som-website/", style =
        "color:white"),
    #"LTER SOM Database",
    tabPanel(
      "Query",
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
              choices = c(som.numerics, som.strings),
              selected = "google_dir"
            ),
            selectInput(
              'plot.y',
              'Plot Y-Axis:',
              choices = c(som.numerics, som.strings),
              selected = "lyr_soc"
            ),
            hr(),
            h3("Plot options"),
            selectInput(
              'plot.type',
              'Type:',
              choices = c("point", "boxplot", "histogram"),
              selected = "point"
            ),
            selectInput(
              'plot.color',
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
    ),
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
                 selectInput('site.varn',
                             'Site:',
                             choices = site.names,
                             selected = ""),
                 checkboxInput("sitevar_ex.loc", "Exclude location level data", FALSE),
                 checkboxInput(
                   "sitevar_ex.prof",
                   "Exclude profile (layer, etc.) level data",
                   FALSE
                 ),
                 checkboxInput("sitevar_ex.class", "Exclude character class data", FALSE),
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
    
  )
    )
