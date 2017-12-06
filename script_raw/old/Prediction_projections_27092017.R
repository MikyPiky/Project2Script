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
predictData_tidy_complete         -> "./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv" (ex Ante - Standard)
predictData_tidy_complete_expost  -> "./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_expostAnomaly.csv" (ex Post)
'
## Plots
''

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
' - Maize_meteo.csv -> /Proj2/data/data_proj/output/ (BaseData_PreProcessing.R)
    NewValues -> /data/data_proj/MeteoMonth_df_tidy_*.csv/ (indexed for various RCMs) <- KlimaMeteo_netcdfTo_sf$Tidy.R
'



###################
## Load Packages ##
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
##############################################################################################################################################################################


###################################################
##### Read Maize_meteo.csv  ####
#################################################
Maize_meteo <- read.csv( file="./data/data_processed/Maize_meteo.csv")

str(Maize_meteo)
Maize_meteo$comId <- as.factor(Maize_meteo$comId)
levels(Maize_meteo$comId)
Maize_meteo$X <- NULL

#################################################
#### Load shape of administrative districts ####
###############################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

##########################################
#### Load com specific average yield ####
########################################

avgYield_comId <- read.csv( file="./data/data_processed/avgYield_comId.csv")
avgYield_comId$X <- NULL
avgYield_comId$comId <- as.factor(avgYield_comId$comId)
str(avgYield_comId)


#########################################################################################
#### Preparations to make predictions for the data derived from five climate models ####
#######################################################################################
' Since I am not using the same data on which the model is trained I need to implement some changes. 
  In particular I cannot work with factor(comId) but need to derive the model matrix to be able to use predict().'


##############################################
#### Select variables used for estimation ####
Maize_meteo_short <- 
  Maize_meteo %>% 
  select (year,comId, siloMaize, SMI_Jun6,SMI_Jul6, SMI_Aug6, T_Jul, P_Jul, siloMaizeAnomaly)


#########################################
#### Append model.matrix explicitly ####
#######################################
'Here I include the model.matrix for the comID exclusively since the  predict command had issues to deal with those. 
Also, only the data used in the models are considered further.'

#### Create model.matrix ####
modelmatrix <- model.matrix(~ Maize_meteo$comId)
dim(modelmatrix)
str(modelmatrix)

#### Convert model.matrix to data.frame ####
modelmatrix_Df <-as.data.frame((modelmatrix))
str(modelmatrix_Df)
modelmatrix_Df$`Maize_meteo$comId1002` # There is a one for each year in the data when comId == 1002 is true

#### Delete modelmatrix ####
rm(modelmatrix)

#### Cbind modelmatrix with short data.frame ####
Maize_meteo_modelmatrix <- cbind(Maize_meteo_short, modelmatrix_Df)
Maize_meteo_modelmatrix$`(Intercept)` <- NULL

#### Clean up names  ####
x <- make.names(names(Maize_meteo_modelmatrix))
x
colnames <- gsub("Maize_meteo.", "", x)
colnames(Maize_meteo_modelmatrix) <- colnames
str(Maize_meteo_modelmatrix)

########################################
#### Fit model used for prediction ####
######################################

#### Delete columns year and comId ####
names(Maize_meteo_modelmatrix)
Maize_meteo_modelmatrix$year <- NULL
Maize_meteo_modelmatrix$comId <- NULL

#####################################################
#### lm.fit with explicit model.matrix of comIds ####

#### June to August ####
## Yield
drops <- c("SMI_Jul6", "siloMaizeAnomaly")
lm.fit_SMI_6_Jun_Aug_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jun_Aug_modelmatrix) # Adjusted R-squared:   0.6857 

## Yield - Anomaly 
drops <- c("SMI_Jul6", "siloMaize")
lm.fit_SMI_6_Jun_Aug_modelmatrix_anomaly <- 
  lm(siloMaizeAnomaly ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jun_Aug_modelmatrix_anomaly) # Adjusted R-squared:    0.3362 

#### July ####
## Yield
drops <- c("SMI_Jun6", "SMI_Aug6", "siloMaizeAnomaly")
lm.fit_SMI_6_Jul_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jul_modelmatrix) # Adjusted R-squared:   0.661 

## Yield - Anomaly
drops <- c("SMI_Jun6", "SMI_Aug6", "siloMaize")
lm.fit_SMI_6_Jul_modelmatrix_anomaly <- 
  lm(siloMaizeAnomaly ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jul_modelmatrix_anomaly) # Adjusted R-squared:    0.2841 


# ########################################################    
# #### lm.fit with NO explicit model.matrix of comIds ####
# 
# ## June to August ##
# ## Yield
# # drops <- c("SMI_Jul6", "siloMaizeAnomaly")
# str(Maize_meteo_short)
# lm.fit_SMI_6_Jun_Aug <- 
#   lm(siloMaize ~ I(T_Jul) + I(T_Jul^2) + I(T_Jul^3) + I(P_Jul)  + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId  ,
#      data = Maize_meteo_short )
# summary(lm.fit_SMI_6_Jun_Aug) # Adjusted R-squared:   0.6857 
# 
# ## Yield - Anomaly
# lm.fit_SMI_6_Jun_Aug_anomaly <- 
#   lm(siloMaizeAnomaly ~ I(T_Jul) + I(T_Jul^2) + I(T_Jul^3) + I(P_Jul)  + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId  ,
#      data = Maize_meteo_short)
# summary(lm.fit_SMI_6_Jun_Aug_anomaly) # Adjusted R-squared:    0.3362 
# 
# ## July ##
# ## Yield
# lm.fit_SMI_6_Jul <- 
#   lm(siloMaize ~ I(T_Jul) + I(T_Jul^2) + I(T_Jul^3) + I(P_Jul)  + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId  ,
#      data = Maize_meteo_short)
# summary(lm.fit_SMI_6_Jul) # Adjusted R-squared:   0.661 
# 
# ## Yield - Anomaly
# lm.fit_SMI_6_Jul_anomaly <- 
#   lm(siloMaizeAnomaly ~ I(T_Jul) + I(T_Jul^2) + I(T_Jul^3) + I(P_Jul)  + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId  ,
#      data = Maize_meteo_short)
# summary(lm.fit_SMI_6_Jul_anomaly) # Adjusted R-squared:    0.2841 


###############################################
#### Generate frame of comIds to merge on ####
#############################################
' This data.frame only includes those comIds which have complete data for silage maize. Needed for projection data.'

str(Maize_meteo)
ComIdMerge<- as.data.frame(avgYield_comId$comId)
names(ComIdMerge) <- "comId"
str(ComIdMerge )



########################################################################################################
#############################################################
#### Loop for different models used to make Predictions ####
###########################################################
' it is necessary to create folder which have the names of the models in data/data_pro/output'
modelListMatrix <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix, lm.fit_SMI_6_Jul_modelmatrix )
modelListMatrixAnomaly <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix_anomaly, lm.fit_SMI_6_Jul_modelmatrix_anomaly )
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")

modelList <- list(lm.fit_SMI_6_Jun_Aug, lm.fit_SMI_6_Jul)
modelListAnomaly <- list(lm.fit_SMI_6_Jun_Aug_anomaly, lm.fit_SMI_6_Jul_anomaly )
modelListNames <- list("lm.fit_SMI_6_Jun_Aug", "lm.fit_SMI_6_Jul")

for (s in 1:length(modelListMatrix) ){
   
  dir.create(paste("./data/data_proj/output/",modelListMatrixNames[[s]], sep="" ), showWarnings = F) # does not overwrite automatically
  dir.create(paste("./data/data_proj/output/",modelListNames[[s]], sep="" ), showWarnings = F) # does not overwrite automatically
  dir.create(paste("./figures/figures_exploratory/Proj/",modelListMatrixNames[[s]], sep="" ), showWarnings = F) # does not overwrite automatically
  dir.create(paste("./figures/figures_exploratory/Proj/",modelListNames[[s]], sep="" ), showWarnings = F) # does not overwrite automatically
  

  ###############################################################
  #### Loop to make predictions for all five climate models ####
  #############################################################
  ' Until here I was only working with the training data, i.e. the observation from the year 1999 - 2015. 
    From here on I am using the projections derived from the climate models to make predictions'
  
  ## Create Namelist used in the models ##
  namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")
  
  #### Create container to store tidy.data.frames of all models ####

  predictData_anomaly_expost_tidy_all <- data.frame()
  predictData_anomaly_tidy_all <- data.frame()
  predictData_tidy_all <- data.frame()
  
  predictData_anomaly_expost_tidy_july_all <- data.frame()

  predictData_anomaly_tidy_july_all <- data.frame()
  predictData_tidy_july_all <- data.frame()
  
  ####################
  #### Start loop ####

  for (r in 1:5){
    #### Load Projections of one RCM ####  
    NewValues <-  read.csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_models[[r]],".csv", sep=""))
    names(NewValues)  
    NewValues$X <- NULL
    unique(NewValues$year) # 1951 - 2099
    dim(NewValues)
    str(NewValues)

    NewValues$comId <- as.factor(NewValues$comId)
    ' Achtung: NewValues und comId merge haben einen unterschiedliche Reihenfolge'
    
    str(NewValues)
    
    ##################################################################################
    #############################################
    ##### Prepare data.frame for prediction ####
    
    #####################################
    #### Stepwise with six anomalies ####
    # June
    NewValues$SMI_Jun6 <- relevel(cut(NewValues$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    # JuLY
    NewValues$SMI_Jul6 <- relevel(cut(NewValues$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    # August
    NewValues$SMI_Aug6 <- relevel(cut(NewValues$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    #################################
    #### Choose variables needed ####
    NewValues_short <- 
      NewValues %>% 
      select (year, comId, SMI_Jun6, SMI_Jul6, SMI_Aug6, T_Jul, P_Jul)
    
    ##############################################################################################################
    #### Merge with Yield Data Frame to get same comIds, i.e. those which have a least nine observations (334) ####
    NewValues_short$comId <- as.factor(NewValues_short$comId)
    
    NewValues_merge <- merge(NewValues_short, ComIdMerge ,  by="comId") #
    str(NewValues_merge) # 49766/149 = 334 : passt also, da wir nur noch 334 (262) comIds übrig haben mit cutoff 9 (17)
    head(NewValues_merge, 50)
    NewValues_merge$comId
    ' Achtung, hier ändert sich die Reihenfolge in den comIds. Das muss später berücksichtigt werden. Bei NewValues, NewValue_short und avgYield_comId
    ist diese anders. Dort wird die länge der Zahl berücksichtigt. Hier wird nur die erste Ziffer bei der Reihenfolge berücksichtigt, also nach 1004 kommt 10042.'
    
    ######################################################################
    #### Extract NewValuesyear comIds in the proper order for merging ####
    NewValuesyear_comId_order <- 
      NewValues_merge %>% 
      filter(year==2000) %>% 
      select(comId)
    
    str(NewValuesyear_comId_order )
    
    ######################################################
    #### Generare Container to store predicted values ####
    predictData <- predictData_anomaly <- predictData_anomaly_expost <- NewValuesyear_comId_order
    str(predictData)

    ###############################################
    #### Generate Factors for comIds and years ####
    NewValues_merge[,c("comId","year")] <- lapply(NewValues_merge[,c("comId","year")], factor )
    
    #########################################
    #### Generate Model.Matrix of ComIds ####
    modelmatrix <- model.matrix(~ NewValues_merge$comId)
    modelmatrix_Df <- as.data.frame((modelmatrix))
    dim(modelmatrix_Df)
    dim(NewValues_merge)
    rm(modelmatrix)
    
    ###############################################
    #### Generate Model.Matrix including NULLS ####
    dim(modelmatrix_Df)
    modelmatrix_Df_NULL <- modelmatrix_Df
    modelmatrix_Df_NULL[modelmatrix_Df_NULL == 1] <- 0
    
    View(modelmatrix_Df_NULL)
    dim(modelmatrix_Df_NULL)
    
    ###################################################################
    #### Use Cbind to generate Dataframe that includes modelmatrix ####
    NewValues_modelmatrix <- cbind(NewValues_merge, modelmatrix_Df)
    str(NewValues_modelmatrix)
    
    ########################################################################
    #### Use Cbind to generate Dataframe that includes NULL modelmatrix ####
    NewValues_modelmatrix_NULL <- cbind(NewValues_merge, modelmatrix_Df_NULL)
    str(NewValues_modelmatrix_NULL)

    ########################
    #### Clean up names ####
    x <- make.names(names(NewValues_modelmatrix))
    x
    colnames <- gsub("NewValues_merge.", "", x)
    colnames
    colnames(NewValues_modelmatrix) <- colnames
    colnames(NewValues_modelmatrix_NULL) <- colnames
    
    str(NewValues_modelmatrix)
    
    ###########################################################################################
    #### Generate data which are demeaned by the average of the climate period 1971 - 2000 ####
    str(NewValues_modelmatrix_NULL)
    NewValues_modelmatrix_NULL$year <- as.numeric(NewValues_modelmatrix_NULL$year)
    
    NewValues_modelmatrix_NULL_demeaned <-  NewValues_modelmatrix_NULL %>% filter(year > 1971 & year < 2001) %>% summarise(SMI_Jun6_mean_ref = mean(SMI_Jun6))
    dim(NewValues_modelmatrix_NULL_demeaned )
    
    ########################################################################
    #### Make prediction for each year derived from the climate models ####
    ######################################################################
    "Ab hier loop über die Jahre 1951 - 2099 der Projection des jeweiligen Klimamodels"
    
    #### Define list of years to loop through ####
    listyear <- seq (1951, 2099)
    length(listyear)
    listyear[149]
    
    #### Start loop over each year derived from the climate projections ####
    for (l in 1:length(listyear)){

      print(listyear[[l]])
      
      #### Filter for year l ####
      NewValuesyear <- NewValues_modelmatrix %>% filter(NewValues_modelmatrix$year == listyear[[l]] )
      rownames(NewValuesyear) <- NULL
      str(NewValuesyear)
  
      ###################################
      #### Make absolute predictions ####
      summary(modelListMatrix[[s]])
      predictlm <- as.data.frame(predict.lm(modelListMatrix[[s]] , newdata = NewValuesyear))
      summary(predictlm)
      str(predictlm)
      
      ##################################################
      #### Get avgYield_comId in order of predictlm ####
       
      ## Merge it with avgYield_comId but keep the order ##
      avgYield_comId_ordered <- merge(NewValuesyear_comId_order, avgYield_comId, by="comId" , all.x=T)
      
      ######################################################################
      #### Ex Post Anomalies: Clear absolute predictions for mean yield ####
      predictlm_anomaly_expost  <- predictlm - avgYield_comId_ordered$avgYield_comId
      summary(predictlm_anomaly_expost )
      'When getting avgYield_comId und predictlm in the right order, the expost and exante estimates are the same.'
      
      
      ###############################################################################
      #### Ex Ante Anomalies: Use model with yield anomalies to make predictions ####
      predictlm_anomaly <- as.data.frame(predict.lm(modelListMatrixAnomaly[[s]] , newdata = NewValuesyear))
      summary(predictlm_anomaly)
      'Das passt hier nicht, die predictions sind zu groß'
      
      ################################################
      #### Change name of predictlm_* data.frames ####
      names(predictlm) <- names(predictlm_anomaly ) <- names(predictlm_anomaly_expost) <- paste(listyear[l])

      #######################################
      #### Manual prediction for com1001 ####
      ' The calculation of the manual prediction can be found in Baseprediction_long. However, it made sense.'
      
      #################################################################
      #### Combine to one large data.frame including all the years ####
      predictData                <- cbind(predictData, predictlm)
      predictData_anomaly_expost <- cbind(predictData_anomaly_expost, predictlm_anomaly_expost)
      predictData_anomaly        <- cbind(predictData_anomaly, predictlm_anomaly)
      
      'When getting avgYield_comId und predictlm in the right order both the exante and the expost approach deliver the same results'

      
    } ## End of loop through all the years of one climate model ##
    
    # #########################################################
    # #### Save one wide data.frame for each climate model ####
    # str(predictData)
    # str(predictData_anomaly )
    # str(predictData_anomaly_expost )
    # 
    # names(predictData)
    # names(predictData_anomaly)
    # names(predictData_anomaly_expost)

    ##########################################
    #### Convert wide data,frame to tidy ####
    ########################################
    
    #### Data.frame with absolut predictions ####
    predictData_tidy <- predictData %>% gather(year, Y_absolut, 2:150, factor_key = T)
    str(predictData_tidy)
    levels(predictData_tidy$year)
    

    #### Data.frane with anomaly predictions - ex ante ####
    predictData_anomaly_tidy <- predictData_anomaly  %>% gather(year, Y_anomaly, 2:150, factor_key = T)
    str(predictData_anomaly_tidy)
    
    #### Data.frame with anomaly predictions - ex post ####
    predictData_anomaly_expost_tidy <- predictData_anomaly_expost  %>% gather(year, Y_anomaly_ep, 2:150, factor_key = T)
    str(predictData_anomaly_expost_tidy)

    ####################################################
    #### Create data.frame defining the model names ####
    model <- as.data.frame(rep(namelist_models[[r]], dim(predictData_tidy)[1]))
    names(model) <- "model"
    
    ##############################################
    #### Append Model Name to tidy data.frame ####
    predictData_tidy                <- cbind(model, predictData_tidy)
    predictData_anomaly_tidy        <- cbind(model, predictData_anomaly_tidy)
    predictData_anomaly_expost_tidy <- cbind(model, predictData_anomaly_expost_tidy)

    
    ##############################################################
    #### Combine to one large data.frame including all models ####
    predictData_tidy_all                <- rbind(predictData_tidy_all, predictData_tidy)
    predictData_anomaly_tidy_all        <- rbind(predictData_anomaly_tidy_all, predictData_anomaly_tidy)
    predictData_anomaly_expost_tidy_all <- rbind(predictData_anomaly_expost_tidy_all, predictData_anomaly_expost_tidy)

  } ## End of loop over different climate models


#####################################################################################
#### Combine absolute predictions and anomaly predictions to one tidy data.frame ####
str(predictData_tidy_all)
str(predictData_anomaly_tidy_all)
str(predictData_anomaly_expost_tidy_all)

####
names(predictData_tidy_all)[4] <- "Y"

predictData_tidy_complete <- merge(predictData_tidy_all, predictData_anomaly_tidy_all)
predictData_tidy_complete_expost <- merge(predictData_tidy_all, predictData_anomaly_expost_tidy_all)

#################################
#### Write large data.frames ####
write.csv(predictData_tidy_complete,        paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep="") )
write.csv(predictData_tidy_complete_expost, paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_expostAnomaly.csv", sep="") )


} ## End of loop which uses different models to make predictions

rm(list=ls())


