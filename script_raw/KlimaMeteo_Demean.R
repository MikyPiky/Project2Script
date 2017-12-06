## Descriptiom ##

'
This script serves to substitute the mean of the climate meteorological data, i.e. precipitation and temperature, with the mean from the training period 1999 - 2015
to avoid any bias induced by the diverging mean in the climate models from the mean in training period. 
This step is not necessary anymore: Instead we decided to only rely on demeaned climate data. For this, 
- we employ the mean of the period 1971 - 2000 to demean the data. 
- Filter for comIds for which no data exist because of no agricultural land (noAgri <- c(9780, 9776, 9763, 9180))
- Also: Change all zero values of SMI to slightly different from zero values (necessary for making categories -> avoid NAs)



"

'

## Input ##

'
- "MeteoMonth_df_tidy_*.csv <- from KlimaMeteo_netcdf_to_sf&Tidy (reshaped climate data from wide to tidy)
-  YieldMeteo.csv <- not preprocessed tidy data from the first project to with the period 1999 - 2015 representing the training period 

'

## Output ##

' MeteoMeonth_df_tidy_19712000_demeaned_*.csv -> ./data/data_proj/ '

#### Packages ####
source("./script/script_raw/Packages.R")
##################################################################################################################################################################################################
##################################################################################################################################################################################################


# ############################
# #### Load training data ####
# YieldMeteo_train <- read_csv("./data/data_processed/YieldMeteo.csv")
# YieldMeteo_train$X1 <- NULL
# YieldMeteo_train$comId <- as.integer(YieldMeteo_train$comId)
# YieldMeteo_train


# ##################################
# #### Change names of the data ####
# names(YieldMeteo_train) <-  (str_replace(names(YieldMeteo_train), "Tav", "T" ))
# names(YieldMeteo_train) <-  (str_replace(names(YieldMeteo_train),  "Pre", "P" ))
# names(YieldMeteo_train)
# 
# #####################################################################
# #### Select meteorological variables which occur in climate data ####
# Train <- YieldMeteo_train %>% select("comId","P_Jan", "P_Feb", "P_Mar", "P_Apr", "P_May", "P_Jun", "P_Jul", "P_Aug", "P_Sep", "P_Oct", "P_Nov", "P_Dec",
#                                                         "T_Jan", "T_Feb", "T_Mar", "T_Apr", "T_May", "T_Jun", "T_Jul", "T_Aug", "T_Sep", "T_Oct", "T_Nov", "T_Dec")
# # , 
#                                                         # "T_Aug_lag", "T_Sep_lag", "T_Oct_lag", "T_Nov_lag", "T_Dec_lag", "P_Aug_lag", "P_Sep_lag", "P_Oct_lag", "P_Nov_lag", "P_Dec_lag" )
# 
# names(Train)
# 
# #####################################
# #### Generate mean of these data ####
# Train_mean <-  Train %>% group_by(comId) %>% summarise_all(funs(mean)) %>% data.frame()
# # View(Train_mean)
# str(Train_mean)
# 
# ####################################################################
# #### Replicate data.frame 149  times to match with climate data ####
# Train_mean_replicate <- coredata(Train_mean)[rep(seq(nrow(Train_mean)),149),]
# str(Train_mean_replicate)
# 
# 
# #### Generate year column ####
# x <- as.data.frame(sapply((1951:2099), rep, times=410))
# y <-gather(x)
# summary(y)
# Train_mean_replicate$year <- y[,2]
# str(Train_mean_replicate)
# 
# #### Order Columns ####
# Train_mean <- Train_mean_replicate[, c(1,26,2:25)]
# str(Train_mean)
# 
# Train_mean$comId <- as.integer(Train_mean$comId)
# 
# # View(Train_mean)
# # str(Train_mean)

#####################################
#### Loop through climate models ####
for (i in 1:length(namelist_RCMs)){
  
  ####################
  #### Load data ####
  ##################
  Climate <- read_csv(paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_RCMs[[i]],".csv", sep=""))
  Climate$comId <- as.integer(Climate$comId)
  # View(Climate)
  Climate
  
  #############################################################
  ## Filter for comId not represented for in training data ####
  Climate_filter  <- Climate %>% filter(comId != c(2000,11000))
  Climate_filter
  # View(Climate_filter)
  
  

  ##########################################
  ##### Select meteorological variables ####
  Climate_select <- Climate_filter %>% select(comId ,year,  SMI_May:SMI_Oct, P_May:P_Oct, T_May:T_Oct, PET_May:PET_Oct )
  Climate_select
  # str(Climate_select)
  # View(Climate_select)
  
  # #############################################
  # #### Merge Climate_select and Train_mean ####
  # ClimateTrain_merge <- inner_join(Climate_select, Train_mean, by=c("comId", "year"), suffix=c(".climate",".train_mean"))
  # ClimateTrain_merge
  # 

  # ####################################################
  # #### Generate group specific mean of these data ####
  # Climate_mean <-  Climate_select %>% group_by(comId) %>% mutate_all(funs(mean))
  # Climate_mean
  # Climate_mean$comId <- as.factor(Climate_select$comId)
  # Climate_mean$year <- Climate_select$year
  # str(Climate_mean)
  # 
  
  #####################################################################
  #### Generate group specific mean for the subperiod 1971 to 2000 ####
  Climate_mean_1971_2000 <- Climate_select %>% filter(year >= 1971 & year <= 2000) %>% group_by(comId) %>% summarise_if(is.double, funs(mean=mean))
  Climate_mean_1971_2000
  # View(Climate_mean_1971_2000)
 
  #############################################################
  #### Replicate data.frame 149 to match with climate data ####
  Climate_mean_1971_2000_replicate <- coredata(Climate_mean_1971_2000)[rep(seq(nrow(Climate_mean_1971_2000)),149),]
  Climate_mean_1971_2000_replicate
  str(Climate_mean_1971_2000_replicate)
  
  ##############################
  #### Generate year column ####
  x <- as.data.frame(sapply((1951:2099), rep, times=410))
  y <-gather(x)
  summary(y)
  Climate_mean_1971_2000_replicate$year <- y[,2]
  Climate_mean_1971_2000_replicate
  
  #######################
  #### Order Columns ####
  Climate_mean_1971_2000_replicate <- Climate_mean_1971_2000_replicate[, c(1,20,2:19)]
  Climate_mean_1971_2000_replicate
  
  ####################################################################
  #### Merge Climate_filter and Climate_mean_1971_2000_replicate #####
  Climate_mean_1971_2000_join <- inner_join(Climate_select, Climate_mean_1971_2000_replicate, by=c("comId", "year"))
  Climate_mean_1971_2000_join
  str(Climate_mean_1971_2000_join)
  # View(Climate_mean_1971_2000_join)
  
  #####################################################################################################
  #### Decompose large data.frame containing absolute values and means for the period 1971 - 2000 #####
  Climate_mean_1971_2000_join_means <- Climate_mean_1971_2000_join %>% select(P_May_mean : T_Oct_mean)
  Climate_mean_1971_2000_join_meteo <- Climate_mean_1971_2000_join %>% select(P_May : T_Oct)
  Climate_mean_1971_2000_unselect <- Climate_mean_1971_2000_join %>% select( -(SMI_May_mean:SMI_Oct_mean),-(P_May : T_Oct), -(P_May_mean : T_Oct_mean) )
  
  ################################################################################
  #### Substract mean of period 1071 - 2000 from all meteorological variables ####
  Climate_mean_1971_2000_join_meteo_demeaned <- as.tibble(Climate_mean_1971_2000_join_meteo - Climate_mean_1971_2000_join_means)

  #######################################
  #### Change names of demeaned data ####
  names(Climate_mean_1971_2000_join_meteo_demeaned) <- paste(names(Climate_mean_1971_2000_join_meteo_demeaned), "_demeaned", sep="")
  
  Climate_mean_1971_2000_join_meteo_demeaned
  
  
  
  ##########################################################################################
  #### Cbind demeanded data with unselected data (SMI, spatial and temporal information ####
  Climate_mean_1971_2000_join_meteo_demeaned2 <- bind_cols(Climate_mean_1971_2000_unselect, Climate_mean_1971_2000_join_meteo_demeaned )
  Climate_mean_1971_2000_join_meteo_demeaned2 

  ##########################################################################################################
  #### Join data with spatial information derived from training data to add state specific information ####
  ########################################################################################################
  
  #### Load training data ####
  YieldMeteo_train <- read_csv("./data/data_processed/YieldMeteo.csv")
  YieldMeteo_train$X1 <- NULL
  YieldMeteo_train$comId <- as.integer(YieldMeteo_train$comId)
  YieldMeteo_train
  
  YieldMeteo_train_select <- YieldMeteo_train %>% group_by(year) %>% filter(year == 1999)  %>% select(comId:state)
  YieldMeteo_train_select$year <- NULL
  
  #### Replicate data.frame 149  times to match with climate data ####
  Train_mean_replicate <- coredata(YieldMeteo_train_select)[rep(seq(nrow(YieldMeteo_train_select)),149),]
  str(Train_mean_replicate)
  
  #### Generate year column ####
  x <- as.data.frame(sapply((1951:2099), rep, times=410))
  y <-gather(x)
  summary(y)
  Train_mean_replicate$year <- y[,2]
  str(Train_mean_replicate)
  
  
  Climate_mean_1971_2000_join_meteo_demeaned3 <- inner_join(Train_mean_replicate,Climate_mean_1971_2000_join_meteo_demeaned2, by=c("comId","year"))
  Climate_mean_1971_2000_join_meteo_demeaned3
  
  ####################################################################################
  #### Filter for comIds for which no data exist because of no agricultural land ####
  ##################################################################################
  noAgri <- c(9780, 9776, 9763, 9180)
  Climate_mean_1971_2000_join_meteo_demeaned4  <- Climate_mean_1971_2000_join_meteo_demeaned3  %>% filter(!comId %in% noAgri)
  # filter(comId != c(9780)) %>% filter(comId != c(9776)) %>% filter(comId != c(9763)) %>% filter(comId != c(9180))
  View(Climate_mean_1971_2000_join_meteo_demeaned4  %>% select(comId,year, SMI_May:SMI_Oct))
  
  #################################################################
  #### Change all zero to slightly different from zero values ####
  ###############################################################
  Climate_mean_1971_2000_join_meteo_demeaned4 <- as.data.frame(Climate_mean_1971_2000_join_meteo_demeaned4)
  # which(Climate_mean_1971_2000_join_meteo_demeaned4 == 0.000000e+00, arr.ind = T)
  # which(Climate_mean_1971_2000_join_meteo_demeaned4 == 0, arr.ind = T)
  # 
  
  Climate_mean_1971_2000_join_meteo_demeaned4[Climate_mean_1971_2000_join_meteo_demeaned4 == 0.000000e+00 ] <- 1.000000e-60
  Climate_mean_1971_2000_join_meteo_demeaned4[Climate_mean_1971_2000_join_meteo_demeaned4 == 0 ] <- 1.000000e-60
  # View(Climate_mean_1971_2000_join_meteo_demeaned4  %>% select(comId,year, SMI_May:SMI_Oct))
  ' Dieses Problem wird eingehender in KlimaMeteo_SMI_zeroAndOne untersucht. '
  
  
  
  
  ########################################
  #### Export newly create data.frame ####
  write_csv(Climate_mean_1971_2000_join_meteo_demeaned4, paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_RCMs[[i]],"_19712000_demean.csv", sep=""))
    
  
  
}



