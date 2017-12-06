## Descriptiom ##

' 
Demean meteorological data for the entire period, i.e. 1951 - 2015, and the reference period 1970 - 2000. 
I figured that the latter makes no sense since we want to imply a mean of 0.5 for the SMI data, which is the case for the 
period 1951 - 2015. Thus, use this data. 
'

## Input ##

'
- "MeteoMonth_df_tidy_*.csv <- from KlimaMeteo_netcdf_to_sf&Tidy (reshaped Meteo_train data from wide to tidy)

'

## Output ##

' Meteo_train_demean.csv -> ./data/data_proj/ '

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
############################
#### Load training data ####
Meteo_train <- read_csv("./data/data_processed/MeteoMonth_df_tidy.csv")
Meteo_train$X1 <- NULL
Meteo_train$comId <- as.integer(Meteo_train$comId)
Meteo_train
View(Meteo_train)

##################################
#### Change names of the data ####
names(Meteo_train) <-  (str_replace(names(Meteo_train), "Tav", "T" ))
names(Meteo_train) <-  (str_replace(names(Meteo_train),  "Pre", "P" ))
names(Meteo_train)

# #####################################################################
# #### Select meteorological variables which occur in Meteo_train data ####
# Meteo_train_select <- Meteo_train %>% select(comId, year, P_May:P_Oct, T_May:T_Oct, SMI_May:SMI_Oct)  
# Meteo_train_select


###########################################################################################
#### Demean all meteorological variables for the entire period 1951 - 2015 ####
#########################################################################################
Meteo_train_demean <- Meteo_train %>%
  group_by(comId) %>%
  mutate_at(vars(P_Jan:PET_Dec_lag), funs(demeaned = . - mean(.)))

'For the case that the data do not have full observations (17), no mean is created and the data are transformed into NAs'


#### Check mean for SMI ####
Meteo_train_SMImean  <-  Meteo_train %>%
  group_by(comId) %>%
  summarise_at ( vars(SMI_Jan:SMI_Dec), funs(mean = mean(.) ))
               
Meteo_train_SMImean
' This supports the use of the data demeaned for the entire period since by doing so we also have a SMI mean of 0.5'

# ############################################
# #### Demean for the period 1971 - 2000 ####
# ##########################################
# 
# #####################################################################
# #### Generate group specific mean for the subperiod 1971 to 2000 ####
# Meteo_train_1971_2000 <- Meteo_train_select %>% 
#   filter(year >= 1971 & year <= 2000) %>% 
#   group_by(comId) %>% 
#   summarise_if(is.double, funs(mean=mean))
# Meteo_train_1971_2000
# # View(Meteo_train_1971_2000)
# 
# #############################################################
# #### Replicate data.frame 149 to match with Meteo_train data ####
# Meteo_train_1971_2000_replicate <- coredata(Meteo_train_1971_2000)[rep(seq(nrow(Meteo_train_1971_2000)), 65),]
# Meteo_train_1971_2000_replicate
# str(Meteo_train_1971_2000_replicate)
# 
# ##############################
# #### Generate year column ####
# x <- as.data.frame(sapply((1951:2015), rep, times=412))
# y <-gather(x)
# summary(y)
# Meteo_train_1971_2000_replicate$year <- y[,2]
# Meteo_train_1971_2000_replicate
# 
# #######################
# #### Order Columns ####
# Meteo_train_1971_2000_replicate <- Meteo_train_1971_2000_replicate[, c(1,20,2:19)]
# Meteo_train_1971_2000_replicate
# 
# ####################################################################
# #### Merge Meteo_train_filter and Meteo_train_1971_2000_replicate #####
# Meteo_train_1971_2000_join <- inner_join(Meteo_train_select, Meteo_train_1971_2000_replicate, by=c("comId", "year"))
# Meteo_train_1971_2000_join
# 
# View(Meteo_train_1971_2000_join)
# 
# #####################################################################################################
# #### Decompose large data.frame containing absolute values and means for the period 1971 - 2000 #####
# Meteo_train_1971_2000_join_means  <- Meteo_train_1971_2000_join %>% select(P_May_mean : T_Oct_mean)
# Meteo_train_1971_2000_join_meteo  <- Meteo_train_1971_2000_join %>% select(P_May : T_Oct)
# Meteo_train_1971_2000_unselect    <- Meteo_train_1971_2000_join %>% select( -(SMI_May_mean:SMI_Oct_mean),-(P_May : T_Oct), -(P_May_mean : T_Oct_mean) )
# 
# ################################################################################
# #### Substract mean of period 1071 - 2000 from all meteorological variables ####
# Meteo_train_1971_2000_join_meteo_demeaned <- as.tibble(Meteo_train_1971_2000_join_meteo - Meteo_train_1971_2000_join_means)
# 
# #######################################
# #### Change names of demeaned data ####
# # names(Meteo_train_1971_2000_join_meteo_demeaned) <- paste(names(Meteo_train_1971_2000_join_meteo_demeaned), "_19712000_demeaned", sep="")
# 
# Meteo_train_1971_2000_join_meteo_demeaned
# 
# ##########################################################################################
# #### Cbind demeanded data with unselected data (SMI, spatial and temporal information ####
# Meteo_train_1971_2000_join_meteo_demeaned2 <- bind_cols(Meteo_train_1971_2000_unselect, Meteo_train_1971_2000_join_meteo_demeaned )
# Meteo_train_1971_2000_join_meteo_demeaned2 
# 
# ##########################################################################################################
# #### Join data with spatial information derived from training data to add state specific information ####
# ########################################################################################################
# 
# #### Load training data ####
# Meteo_train <- read_csv("./data/data_processed/Meteo.csv")
# Meteo_train$X1 <- NULL
# Meteo_train$comId <- as.integer(Meteo_train$comId)
# Meteo_train
# 
# Meteo_train_select <- Meteo_train %>% group_by(year) %>% filter(year == 1999)  %>% select(comId:comState)
# Meteo_train_select$year <- NULL
# 
# #### Replicate data.frame 149  times to match with Meteo_train data ####
# Train_mean_replicate <- coredata(Meteo_train_select)[rep(seq(nrow(Meteo_train_select)),149),]
# str(Train_mean_replicate)
# 
# #### Generate year column ####
# x <- as.data.frame(sapply((1951:2099), rep, times=410))
# y <-gather(x)
# summary(y)
# Train_mean_replicate$year <- y[,2]
# str(Train_mean_replicate)
# 
# 
# Meteo_train_1971_2000_join_meteo_demeaned3 <- inner_join(Train_mean_replicate,Meteo_train_1971_2000_join_meteo_demeaned2, by=c("comId","year"))
# Meteo_train_1971_2000_join_meteo_demeaned3


########################################
#### Export newly create data.frame ####
write_csv(Meteo_train_demean, "./data/data_processed/Meteo_train_demean.csv")







