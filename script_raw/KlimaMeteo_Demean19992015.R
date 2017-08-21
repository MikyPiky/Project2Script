## Descriptiom ##

' This script serves to substitute the mean of the climate meteorological data, i.e. precipitation and temperature, with the mean from the training period 1999 - 2015
to avoid any bias induced by the diverging mean in the climate models from the mean in training period. "

'

## Input ##

'
-  Climate_*.csv <- from KlimaMeteo_netcdfTo_sf$Tidy (reshaped climate data from widy to tidy)
-  YieldMeteo.csv <- tidy data from the first project to with the period 1999 - 2015 representing the training period 

'

## Output ##

' MeteoMeonth_df_tidy_demean19992015_*.csv'

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
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")

############################
#### Load training data ####
YieldMeteo_train <- read_csv("./data/data_processed/YieldMeteo.csv")
YieldMeteo_train$X1 <- NULL
str(YieldMeteo_train)

##################################
#### Change names of the data ####
names(YieldMeteo_train) <-  (str_replace(names(YieldMeteo_train), "Tav", "T" ))
names(YieldMeteo_train) <-  (str_replace(names(YieldMeteo_train),  "Pre", "P" ))
names(YieldMeteo_train)

#####################################################################
#### Select meteorological variables which occur in climate data ####
Train <- YieldMeteo_train %>% select("comId","P_Jan", "P_Feb", "P_Mar", "P_Apr", "P_May", "P_Jun", "P_Jul", "P_Aug", "P_Sep", "P_Oct", "P_Nov", "P_Dec",
                                                        "T_Jan", "T_Feb", "T_Mar", "T_Apr", "T_May", "T_Jun", "T_Jul", "T_Aug", "T_Sep", "T_Oct", "T_Nov", "T_Dec")
# , 
                                                        # "T_Aug_lag", "T_Sep_lag", "T_Oct_lag", "T_Nov_lag", "T_Dec_lag", "P_Aug_lag", "P_Sep_lag", "P_Oct_lag", "P_Nov_lag", "P_Dec_lag" )

names(Train)

#####################################
#### Generate mean of these data ####
Train_mean <-  Train %>% group_by(comId) %>% summarise_all(funs(mean))
View(Train_mean)

#############################################################
#### Replicate data.frame 149 to match with climate data ####
Train_mean <- as.data.frame(sapply(Train_mean, rep.int, times=149))

#### Generate year column ####
x <- as.data.frame(sapply((1951:2099), rep, times=410))
y <-gather(x)
Train_mean$year <- y[,2]

#### Order Columns ####
Train_mean <- Train_mean[, c(1,26,2:25)]

View(Train_mean)
str(Train_mean)

#####################################
#### Loop through climate models ####
for (i in 1:length(namelist_models)){
  
  ####################
  #### Load data ####
  ##################
  Climate <- read_csv(paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_models[[i]],".csv", sep=""))
  Climate$X1.12360 <- NULL
  str(Climate)  
  # View(Climate)
  
  Climate_filter  <- Climate %>% filter(comId != c(2000,11000))
  View(Climate_filter)
  
  str(Climate_filter)
  
  ##########################################
  ##### Select meteorological variables #####
  Climate_select <- Climate %>% select("comId","year","P_Jan", "P_Feb", "P_Mar", "P_Apr", "P_May", "P_Jun", "P_Jul", "P_Aug", "P_Sep", "P_Oct", "P_Nov", "P_Dec",
                                                         "T_Jan", "T_Feb", "T_Mar", "T_Apr", "T_May", "T_Jun", "T_Jul", "T_Aug", "T_Sep", "T_Oct", "T_Nov", "T_Dec")
  
                                                         # "T_Aug_lag", "T_Sep_lag", "T_Oct_lag", "T_Nov_lag", "T_Dec_lag", "P_Aug_lag", "P_Sep_lag", "P_Oct_lag", "P_Nov_lag", "P_Dec_lag" )
  
  str(Climate_select)
  View(Climate_select)
  
  #### Merge Climate_select and Train_mean ####
  ClimateTrain_merge <- merge (Climate_select, Train_mean, by=c("comId", "year"), suffix=c(".climate",".train_mean"))
  str(ClimateTrain_merge)
  
  View(ClimateTrain_merge)
  ClimateTrain_merge$comId <- as.factor(ClimateTrain_merge$comId)
  levels(ClimateTrain_merge$comId)
  
  ####################################################
  #### Generate group specific mean of these data ####
  Climate_mean <-  Climate_select %>% group_by(comId) %>% mutate_all(funs(mean))
  Climate_mean
  Climate_mean$comId <- as.factor(Climate_select$comId)
  Climate_mean$year <- Climate_select$year
  str(Climate_mean)
  
  View(Climate_mean)
  
  ' Da die Rheinfolge immmer nocht nicht stimmt muss ich von hier an nochmals weiterarbeiten'
  
  ####################################################
  #### Merge ClimateTrain_merge with Climate_mean ####
  ClimateTrain_merge_mean <- merge (ClimateTrain_merge, Climate_mean, by=c("comId", "year"), suffix = c("", ".mean"))
  str(ClimateTrain_merge_mean)
  names(ClimateTrain_merge_mean)[51:74] <- paste(names(ClimateTrain_merge_mean)[51:74], ".climate_mean", sep="")
  View(ClimateTrain_merge_mean )
  
  #######################################################################
  #### Comprise new data.frames from ClimateTrain_merge_mean subsets ####
  Climate <- ClimateTrain_merge_mean[,3:26]
  str(Climate)
  Train_mean <- ClimateTrain_merge_mean[,27:50]
  str(Train_mean)
  Climate_mean <- ClimateTrain_merge_mean[, 51:74]
  str(Climate_mean)
  
  ##############################################
  #### Subsitute Climate mean by Train Mean ####
  Climate_newMean <- Climate - Climate_mean + Train_mean
  
  Climate_newMean$comId <-   ClimateTrain_merge_mean$comId
  Climate_newMean$year <-   ClimateTrain_merge_mean$year
  Climate_newMean <-  Climate_newMean[, c(25,26,1:24)]
  names(Climate_newMean) <- gsub(".climate", "", names(Climate_newMean))
  str(Climate_newMean)
  View(Climate_newMean)
  
  ##############################
  #### Validate new results ####
  '1001 1951 P_Jan: Climate 120.76562 ; Climate Mean = 111.7454 ; Train_Mean= 86.30207'
  120.86562 - 111.7454 + 86.30207 

  ############################################
  #### Append SMI Data from Climate Model ####
  names(Climate)
  Climate_SMI <- Climate %>% select(comId:SMI_Dec)
  
  Climate2 <- merge(Climate_SMI, Climate_newMean, by=c("comId", "year"))
  str(Climate2)
  
  ########################################
  #### Export newly create data.frame ####
   write_csv(Climate, paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_models[[i]],"_normalizedRef.csv", sep=""))
    
  
  
}



