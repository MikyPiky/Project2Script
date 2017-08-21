## Descriptiom ##

' This Script produces summary/descriptive statistics for the climate periods, i.e. 1971 - 2000, 2021 -2050, and 2071 - 2099. "

'

## Input ##

'
-  MeteoMonth_df_tidy <- from KlimaMeteo_netcdfTo_sf$Tidy (reshaped climate data from widy to tidy)s

'

## Output ##

'
- Data.frames of mean and median of subperiods 
  MeteoMonth_df_tidy_MeanMedian_subperiod -> "./data/data_proj/output/MeanMedian_comId/MeteoMonth_df_tidy_MeanMedian_", namelist_periods[[l]], namelist_models[[i]],".csv"
- And summary statistics of these data -> "./data/data_proj/output/MeanMedian_comId/MeteoMonth_df_tidy_MeanMedian_", namelist_periods[[l]], namelist_models[[i]],"_Summary.csv"


'


#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
library(zoo)
library(foreign)
# library(maps)
library(colorspace)
library(lattice)
library(stringr)
library(DataCombine)
library(reshape2)
library(sf)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(grid)
library(stargazer)
library(stringr)


##################################################################################################################################################################################################
##################################################################################################################################################################################################


######################################################################################
#### Check means of the administrative districts to validate the SMI climate data ####
######################################################################################

#### Preparation of loop ####
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")
i=1

#### Start of loop through RCms ####
for (i in 1:5){
  MeteoMonth_df_tidy <- read.csv(paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_models[[i]],".csv", sep=""))
  MeteoMonth_df_tidy$X <- NULL
  
  x <- summary(MeteoMonth_df_tidy)
  capture.output(x, file = paste("./data/data_proj/input/MeteoMonth_df_tidy_", namelist_models[[i]],"_Summary.txt", sep=""))
  
  #### Preparation for loop through climate periods ####
  namelist_periods <- c("1971_2000_", "2021_2050_", "2070_2099_")
  list_periods <- list(c(1971, 2021, 2070), c(2000,2050,2099))
  list_periods[[2]][[1]]
  
  #### Start of loop trough climate periodsc ####
  for (l in 1:3){
    #### Retriev com specific Mean of the SMI of subperiods ####
    MeteoMonth_df_tidy_MeanMedian_subperiod <-  MeteoMonth_df_tidy %>%
      group_by(comId) %>%
      filter(comId != ("09180") & comId != ("09780") & comId != ("09776") & comId != ("09763")) %>% # filter for coms which have no agricultural area
      filter(year >= list_periods[[1]][[l]] & year <=  list_periods[[2]][[l]]) %>%
      summarise(SMI_Jun_mean = round(mean(SMI_Jun),2), SMI_Jul_mean = round(mean(SMI_Jul),2), SMI_Aug_mean = round(mean(SMI_Aug),2) ,
                SMI_Jun_median = round(median(SMI_Jun),2), SMI_Jul_median = round(median(SMI_Jul),2), SMI_Aug_median = round(median(SMI_Aug),2))
    
    
    write.csv(MeteoMonth_df_tidy_MeanMedian_subperiod, paste("./data/data_proj/output/MeanMedian_comId/MeteoMonth_df_tidy_MeanMedian_", namelist_periods[[l]], namelist_models[[i]],".csv", sep=""))
    
    # View(MeteoMonth_df_tidy_MeanMedian_subperiod )
    
    #### Export Summary/ Descriptive Statistics of the SMI Data ####
    capture.output( summary(MeteoMonth_df_tidy_MeanMedian_subperiod ), file = paste("./data/data_proj/output/MeanMedian_comId/MeteoMonth_df_tidy_MeanMedian_", namelist_periods[[l]], namelist_models[[i]],"_Summary.txt", sep="") )
    
  } # End of loop trough climate periods
  
  
  #### Retriev com specific Mean of the SMI for the entire period ####
  MeteoMonth_df_tidy_MeanMedian <-  MeteoMonth_df_tidy %>%
    group_by(comId) %>%
    # filter(year >= 1971 & year <= 2000) %>%
    filter(comId != ("09180") & comId != ("09780") & comId != ("09776") & comId != ("09763")) %>% # filter for coms which have no agricultural area
    summarise(SMI_Jun_mean = round(mean(SMI_Jun),2), SMI_Jul_mean = round(mean(SMI_Jul),2), SMI_Aug_mean = round(mean(SMI_Aug),2) ,
              SMI_Jun_median = round(median(SMI_Jun),2), SMI_Jul_median = round(median(SMI_Jul),2), SMI_Aug_median = round(median(SMI_Aug),2))
  
  write.csv(MeteoMonth_df_tidy_MeanMedian,paste("./data/data_proj/output/MeanMedian_comId/MeteoMonth_df_tidy_MeanMedian_", namelist_models[[i]],".csv", sep=""))
  
  # View(MeteoMonth_df_tidy_MeanMedian )
  summary(MeteoMonth_df_tidy_MeanMedian )
} # End of loop trough RCMs

