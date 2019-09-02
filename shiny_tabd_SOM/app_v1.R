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

ui <- fluidPage(      #Page title row
  fluidRow(
    column(12,
           offset = 0,
           titlePanel(title = "LTER SOM Database", windowTitle = "LTER SOM Database"))
  ),

#Separator
hr(),
h2("Filters"),

#Options row
fluidRow(
  column(3,
         wellPanel(
           #Filter options for...
           selectInput('network', 'Network:', choices = c("ALL",networks), selected = "ALL", multiple = T),  #HOW TO start with empty box instead of using "ALL"
           selectInput('exptype', 'Experiment type:', choices = c("ALL", exp.types), selected = "ALL")
         )
  ),
  column(3,
         wellPanel(
           #Filter options for...
           selectInput('ctl', 'Control only:', choices = c("ALL","Control Data Only"), selected = "ALL"),
           selectInput('timeseries', 'Time series:', choices = c("ALL","Time Series Data Only"), selected = "ALL")
         )
  )
),

#ADD filter by soil depth... maybe as a slider?
#ADD filter by lat-long...

#Separator
hr(),

pageWithSidebar(
  
  h2("Output"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     h3("Plot options"),
                     selectInput('plot.x', 'Plot X-Axis:', choices = som.numerics, #Make the choices pull from tarball unique
                                 selected = "lyr_soc"),
                     
                     selectInput('plot.y', 'Plot Y-Axis:', choices = c(som.numerics,som.strings), #Make the choices pull from tarball unique
                                 selected = "google_dir")
                     
                     # ADD slider limits for plot variables...
                     # ADD color options for plot...
                     # ADD symbol options for plot...
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     h3("Map options"),
                     selectInput('ctl', 'Control only:', choices = c("ALL","Control Data Only"), selected = "ALL"),
                     selectInput('timeseries', 'Time series:', choices = c("ALL","Time Series Data Only"), selected = "ALL")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     h3("Data table options"),
                     selectInput('ctl', 'Control only:', choices = c("ALL","Control Data Only"), selected = "ALL"),
                     selectInput('timeseries', 'Time series:', choices = c("ALL","Time Series Data Only"), selected = "ALL")
    )  
  ),
  mainPanel(
    tabsetPanel(tabPanel("Plot", value=1, plotOutput("dataPlot")), 
                tabPanel("Map", value=2),
                tabPanel("Table", value=3, DTOutput('tbl')),
              id = "conditionedPanels"
              )
    ) 
  )
)

server <- function(input,output){
  
  #reactive function to create data table
  data.tbl <- reactive({
    
    df <- NULL
    
    #Network filter
    if(input$network != "ALL") {
      df <- tarball %>% filter(network == input$network)
    } else {
      df <- tarball
    }
    
    #Control only filter
    if(input$ctl != "ALL") {
      df <- df[1,1] #Make this use ctl_only ftn
    }
    
    #Time series filter
    if(input$timeseries != "ALL") {
      df <- df %>% filter(time_series == "YES")
    }
    
    #Experiment filter
    if(input$exptype != "ALL") {
      df <- df %>% filter(tx_L1_level == input$exptype) ### NEEDS TO FILTER ACROSS THE OTHER LEVELS
    }
    
    ## Return the filtered dataframe
    return(df)
    
  })
  
  #Create ggplot using filtered data from data.tbl() above
  output$dataPlot <- renderPlot({
    ggplot(data.tbl(), aes_string(x=input$plot.x, y=input$plot.y)) + geom_point() + 
      theme(axis.text.x = element_text(angle = 90))
    #...plot color and symbols
    #...change plot size
    #...dynamic title
    #...legend
    
  })
  
  #Create user filtered DataTable pulling dataframe from data.tbl() above  
  output$tbl = renderDT(data.tbl(), 
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
shinyApp(ui=ui,server=server)