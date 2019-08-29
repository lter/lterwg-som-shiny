library(dplyr)
library(ggplot2)

# Get the tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv")

# Vector of numeric analyte names
num.analytes <- colnames(as.data.frame(select_if(tarball, is.numeric)))
str.analytes <- colnames(as.data.frame(select_if(tarball, is.factor)))

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("SOM PLOT"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput('y', 'Analyte:', choices = num.analytes,
                  selected = "lyr_soc"),
      selectInput('x', 'By:', choices = str.analytes, 
                  selected = "google_dir")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("myPlot")  
    )
    
  )
)
