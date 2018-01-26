#######################################################################
## Base Model for predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################
#### Description of Script ####
' 

Script to make climate projections for the period 1951 - 2099 for each RCM () and 
prediction model (SMI June & August vs SMI July only - July Prec and Temp in both)

Ex post (predicting absolut yield values and than demean them) and ex ante demeaning (directly predicting demeaned SMI values) deliver the same results.

'
#### Output ####
## Files
'
  - Climate_predicted <- ./data/data_proj/output/", namelist_RCMs[[r]],"/Climate_predicted.csv
  - Climate_predicted_allRCMs <- ./data/data_proj/output/Climate_predicted_allRCMs.csv



'
## Plots
''

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
' - Maize_meteo.csv -> /Proj2/data/data_proj/output/ (BaseData_PreProcessing.R)
    Climate -> /data/data_proj/MeteoMonth_df_tidy_*.csv/ (indexed for various RCMs) <- KlimaMeteo_netcdfTo_sf$Tidy.R
'



###################
## Load Packages ##
library(MASS)
library(plm)
library(boot)
library(gtools)
library(lme4)
library(lmtest)
library(car)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(stringr)
library(classInt)
library(RColorBrewer)
library(stargazer)
library(ggthemes)
library(caret)   
library(plyr)
library(sf)
library(tidyverse)
library(ggplot2)  
library(grDevices)
library(car)
library(dplyr)
library(leaps)
library(relaimpo)
library(MuMIn)
library(forcats)

##############################################################################################################################################################################
# rm(list=ls())
getwd()

#################################################
#### Read in data.frame and estimate models ####
###############################################
source("./script/script_raw/BaseModel.R")



# ###############################################
# #### Generate frame of comIds to merge on ####
# #############################################
# ' This data.frame only includes those comIds which have complete data for silage maize. Needed for projection data.
# --> not sure, maybe its enough to merge with vgr2500_krs'
# 
# ComIdMerge<- as_tibble(avgYield_comId$comId)
# names(ComIdMerge) <- "comId"
# ComIdMerge 

#########################################################################################################################################################
#############################################################
#### Loop for different models used to make Predictions ####
###########################################################
#########################################################################################################################################################
' lists of models to loop through are defined in BaseModel.R'

###############################################################
#### Loop to make predictions for all five climate models ####
#############################################################
' Until here I was only working with the training data, i.e. the observation from the year 1999 - 2015. 
    From here on I am using the projections derived from the climate models to make predictions'

## Create Namelist used in the models ##

# #### Create container to store tidy.data.frames of all models ####
# 
# # predictData_anomaly_expost_tidy_all <- data.frame()
# # predictData_anomaly_tidy_all <- data.frame()
# predictData_tidy_all <- data.frame()
# # 
# # predictData_anomaly_expost_tidy_july_all <- data.frame()
# # 
# # predictData_anomaly_tidy_july_all <- data.frame()
# predictData_tidy_july_all <- data.frame()

################################################
#### Start loop through climate predictions ####
for (r in seq_along(namelist_RCMs) ) {
  
  #### Load Projections of one RCM ####  
  # Climate <-  read_csv( paste("./data/data_proj/MeteoMonth_df_tidy_DMI_19712000_demean.csv"))
  
  Climate <-  read_csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_RCMs[[r]],"_19712000_demean.csv", sep=""))
  Climate 
  Climate$comId <- as.factor(Climate$comId)
  Climate$year <- as.factor(Climate$year)
  
  
  # View(Climate)
  # 
  # View(Climate%>% select(comId, year, SMI_May:SMI_Oct))
  # View(Climate %>% filter(comId==10046) %>% select(comId, year, SMI_May:SMI_Oct))
  # View(Climate %>% filter(year==2099) %>% select(comId, year, SMI_May:SMI_Oct))
  # 
  ##################################################################################
  #############################################
  ##### Prepare data.frame for prediction ####
  
  #####################################
  #### Stepwise with six anomalies ####
  # May
  Climate$SMI_May6 <- relevel(cut(Climate$SMI_May, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  # June
  Climate$SMI_Jun6 <- relevel(cut(Climate$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  # JuLY
  Climate$SMI_Jul6 <- relevel(cut(Climate$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  # Aug
  Climate$SMI_Aug6 <- relevel(cut(Climate$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  # Sep
  Climate$SMI_Sep6 <- relevel(cut(Climate$SMI_Sep, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  # Oct
  Climate$SMI_Oct6 <- relevel(cut(Climate$SMI_Oct, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")), ref= "nrml") 
  
  #########################################
  #### Generate Model.Matrix of ComIds ####
  modelmatrix <- model.matrix(~ Climate$comId)
  modelmatrix_Df <- as.data.frame((modelmatrix))
  dim(modelmatrix_Df)
  rm(modelmatrix)
  str(modelmatrix_Df)
  
  # ###############################################
  # #### Generate Model.Matrix including NULLS ####
  # dim(modelmatrix_Df)
  # modelmatrix_Df_NULL <- modelmatrix_Df
  # modelmatrix_Df_NULL[modelmatrix_Df_NULL == 1] <- 0
  # 
  # # View(modelmatrix_Df_NULL)
  # dim(modelmatrix_Df_NULL)
  
  ###################################################################
  #### Use Cbind to generate Dataframe that includes modelmatrix ####
  Climate_modelmatrix <- bind_cols(Climate, modelmatrix_Df)
  Climate_modelmatrix
  # 
  # ########################################################################
  # #### Use Cbind to generate Dataframe that includes NULL modelmatrix ####
  # Climate_modelmatrix_NULL <- bind_cols(Climate, modelmatrix_Df_NULL)
  # Climate_modelmatrix_NULL
  
  ########################
  #### Clean up names ####
  x <- make.names(names(Climate_modelmatrix))
  x
  colnames <- gsub("Climate.", "", x)
  colnames
  colnames(Climate_modelmatrix) <- colnames
  # colnames(Climate_modelmatrix_NULL) <- colnames
  
  Climate_modelmatrix
  
  ########################################################################
  #### Make prediction for each year derived from the climate models ####
  ######################################################################
  
  
  ##############################################
  #### Start of loop trough modelList_climate ####
  predictlm_container <- as_tibble(list(x = seq_len(dim( Climate_modelmatrix)[1]   ) ) )
  for (s in seq_along(modelList_climate) ){
    
    
    #### with ModelMatrix to capture comIds ####
    predictlm <- as_tibble(predict(modelList_climate[[s]] , newdata = Climate_modelmatrix ) )
    names(predictlm) <- paste("sMA_",nameList_climate[[s]] , sep="")
    Climate_modelmatrix_predict <- bind_cols(Climate_modelmatrix[,c(1:5)], predictlm)
    Climate_modelmatrix_predict 
    
    predictlm_container <- bind_cols(predictlm_container, predictlm)
    
  }
  
  # nas_sMA_lm.fit_SMI_6_Jun_Aug_modelmatrix <-as.vector(which(is.na(predictlm_container$sMA_lm.fit_SMI_6_Jun_Aug_modelmatrix)))
  # nas_sMA_lm.fit_SMI_6_Jul_modelmatrix <-as.vector(which(is.na(predictlm_container$sMA_lm.fit_SMI_6_Jul_modelmatrix)))

  # predictlm_container$sMA_lm.fit_SMI_6_Jun_Aug_modelmatrix
  # 
  
  # View(Climate_modelmatrix [ nas_sMA_lm.fit_SMI_6_Jun_Aug_modelmatrix ,])
  # View(Climate_modelmatrix [ nas_sMA_lm.fit_SMI_6_Jul_modelmatrix ,]) 

  
  predictlm_container$x <- NULL
  Climate_modelmatrix_predict 
  
  ########################################################
  #### Cbind newly created data.frames with Climate ####
  
  ## test for order
  all(Climate_modelmatrix$comId[1:1000]  ==   Climate$comId[1:1000]  )
  
  ## combine
  Climate_predicted <- bind_cols(Climate, predictlm_container)
  
  #### Append RCM ####
  model <- as_tibble(rep(namelist_RCMs[[r]], dim(Climate_predicted)[1]))
  names(model) <- "RCM"
  
  Climate_predicted <- bind_cols(  Climate_predicted, model)
  
  #######################################################
  #### Write large data.frame for each climate model ####
  dir.create( paste("./data/data_proj/output/", namelist_RCMs[[r]], sep="") , showWarnings = F) # does not overwrite automatically
  
  write_csv(Climate_predicted , paste("./data/data_proj/output/", namelist_RCMs[[r]],"/Climate_predicted.csv", sep="") )
  


} ## End of loop which uses different models to make predictions

##################################################################
#### Create one large data.frame including all climate model ####
################################################################
Climate_predicted_list <- list( "DMI" = list(), "ICTP"= list(), "KNMI"= list(), "MPI"= list(),  "SMHI"= list(), "All_RCMs" = list())

for(r in seq_along(namelist_RCMs)){
Climate_predicted_list[[r]]<- read_csv(paste("./data/data_proj/output/", namelist_RCMs[[r]],"/Climate_predicted.csv", sep="") )
}


## Store all RCMS in one data.frame ##
Climate_predicted_allRCMs <- bind_rows(bind_rows(bind_rows(bind_rows(Climate_predicted_list[[1]], Climate_predicted_list[[2]]), Climate_predicted_list[[3]]), Climate_predicted_list[[4]]), Climate_predicted_list[[5]])
Climate_predicted_allRCMs_average <- Climate_predicted_allRCMs

## Make Indicator for all models ##
Climate_predicted_allRCMs_average$RCM <-  rep("All_RCMs", dim(Climate_predicted_allRCMs_average ) [1])

Climate_predicted_total <- bind_rows(Climate_predicted_allRCMs, Climate_predicted_allRCMs_average )

## Export data ##
write_csv(Climate_predicted_allRCMs, paste("./data/data_proj/output/Climate_predicted_allRCMs.csv"))
write_csv(Climate_predicted_total, paste("./data/data_proj/output/Climate_predicted_allRCMs_total.csv"))

