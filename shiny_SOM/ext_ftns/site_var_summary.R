# Description: Filter Tarball-V2 data for 'control' data only 
# Date: May 8, 2019
# Author: Derek Pierson

# Libraries
library(dplyr)

tarball <- readRDS("C:/Users/Derek/Google Drive/LTER-SOM/homoged_and_bound_output/somCompositeData_2019-09-14.rds")

unique(tarball$location_name)

var_sumry <- function(df, col) {
  
  
  
}






site_var_summary <- function(site, tar) {
  
  #DEBUG
  site <- "Harvard Forest"
  
  
  #Extract site data from tarball
  df <- tarball %>% filter(location_name == site)
  
  #use ftn to produce vector with n, mean for each column, then call with rbind
  



  #return the dataframe
  return(var.unique)
}



