#### Description of Script ####
' 
- Predict on data which were used to train the model (one prediction for each year) to allow comparision 
- Fit combined Model and best model from Paper 1
- Loop through those models to make prediction for each year
'
#### Output ####
## Files
' 

'
## Plots
'

'

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
'  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)

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
#############################################################################################################################################################################
#############################################################################################################################################################################
rm(list=ls())
getwd()

#### Read in Maize_meteo Data ####
Maize_meteo <- read_csv( file="./data/data_processed/Maize_meteo.csv")
Maize_meteo
# Maize_meteo$X1 <- NULL
# str(Maize_meteo)
# names(Maize_meteo)

#### Make factors necessary for statistical applications ####
Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")] <- 
  lapply(Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")], factor )

levels(Maize_meteo$SMI_Aug6)

#### Relevel SMI data ####
Maize_meteo$SMI_May6 <- relevel(Maize_meteo$SMI_May6, ref= "nrml") 
Maize_meteo$SMI_Jun6 <- relevel(Maize_meteo$SMI_Jun6, ref= "nrml") 
Maize_meteo$SMI_Jul6 <- relevel(Maize_meteo$SMI_Jul6, ref= "nrml") 
Maize_meteo$SMI_Aug6 <- relevel(Maize_meteo$SMI_Aug6, ref= "nrml") 
Maize_meteo$SMI_Sep6 <- relevel(Maize_meteo$SMI_Sep6, ref= "nrml") 
Maize_meteo$SMI_Oct6 <- relevel(Maize_meteo$SMI_Oct6, ref= "nrml") 

Maize_meteo$SMI_May6 <- fct_relevel(Maize_meteo$SMI_May6, "drght_svr", after =1)
Maize_meteo$SMI_Jun6 <- fct_relevel(Maize_meteo$SMI_Jun6, "drght_svr", after =1)
Maize_meteo$SMI_Jul6 <- fct_relevel(Maize_meteo$SMI_Jul6, "drght_svr", after =1)
Maize_meteo$SMI_Aug6 <- fct_relevel(Maize_meteo$SMI_Aug6, "drght_svr", after =1)
Maize_meteo$SMI_Sep6 <- fct_relevel(Maize_meteo$SMI_Sep6, "drght_svr", after =1)
Maize_meteo$SMI_Oct6 <- fct_relevel(Maize_meteo$SMI_Oct6, "drght_svr", after =1)
# 
levels(Maize_meteo$SMI_May6)
levels(Maize_meteo$SMI_Jun6)
levels(Maize_meteo$SMI_Jul6)
levels(Maize_meteo$SMI_Aug6)
levels(Maize_meteo$SMI_Sep6)
levels(Maize_meteo$SMI_Oct6)



#################################################################################################################################################################
###########################################################
#### Predictions on observational data - annual junks ####
#########################################################
#################################################################################################################################################################


###############################################
#### Estimate models used for predictions ####
#############################################

## Yield Anomaly, demeaned meteo, SMI Jun and Aug, comId ##
lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                           + T_Jul 
                                         + I(T_Jul^2) 
                                         + I(T_Jul^3) 
                                         + P_Jul 
                                         + I(P_Jul^2) 
                                         + I(P_Jul^3) 
                                         + SMI_Jun6 
                                         + SMI_Aug6 
                                         + comId
                                         ,
                                         data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean )
par(mfrow=c(2,2))
plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

## Yield Anomaly, demeaned meteo, SMI Jun and Aug, NO comId ##
lm.fit_SMI_6_Jun_Aug_anomaly <- lm(siloMaizeAnomaly ~ 
                                           + T_Jul 
                                         + I(T_Jul^2) 
                                         + I(T_Jul^3) 
                                         + P_Jul 
                                         + I(P_Jul^2) 
                                         + I(P_Jul^3) 
                                         + SMI_Jun6 
                                         + SMI_Aug6 
                                         ,
                                         data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_anomaly )
plot(lm.fit_SMI_6_Jun_Aug_anomaly)

## Yield Anomaly, demeaned meteo, SMI JuL  ##
lm.fit_SMI_6_July_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                             + T_Jul_demeaned
                                           + I(T_Jul_demeaned^2)
                                           + I(T_Jul_demeaned^3)
                                           + P_Jul_demeaned
                                           + I(P_Jul_demeaned^2)
                                           + I(P_Jul_demeaned^3)
                                           + SMI_Jul6
                                           
                                           ,
                                           data = Maize_meteo)
summary(lm.fit_SMI_6_July_anomaly_demean)
plot(lm.fit_SMI_6_July_anomaly_demean, main="Std. anomaly demean - noComId")

## Best model derived via dredge model subset selection based on BIC ##
bestModelDredgeBIC_anomaly_demean <- lm(formula = siloMaizeAnomaly ~ 
                           poly(P_Jul_demeaned, degree = 3, raw = T) + 
                           poly(P_Jun_demeaned, degree = 3, raw = T) +
                           poly(P_May_demeaned, degree = 3, raw = T) +
                           poly(P_Oct_demeaned, degree = 3, raw = T) +
                           poly(T_Aug_demeaned, degree = 3, raw = T) +
                           poly(T_Jul_demeaned, degree = 3, raw = T) + 
                           poly(T_Jun_demeaned, degree = 3, raw = T) +
                           poly(T_May_demeaned, degree = 3, raw = T) +
                           poly(T_Oct_demeaned, degree = 3, raw = T) +
                           poly(T_Sep_demeaned, degree = 3, raw = T) +
                           SMI_May6 + SMI_Jun6 + 1
                         , data = Maize_meteo)
summary(bestModelDredgeBIC_anomaly_demean)
plot(bestModelDredgeBIC_anomaly_demean, main="bestModelDredgeBIC")




#######################################################################
#### Loop through those models to make predictions for each year #####
#####################################################################

######################
#### Prepare loop ####
modelList <- list(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean, lm.fit_SMI_6_July_anomaly_demean, lm.fit_SMI_6_July_anomaly_demean, bestModelDredgeBIC_anomaly_demean)

modelListNames <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jun_Aug", "lm.fit_SMI_6_Jul", "bestModelDredgeBIC")


str(modelList,1 )
str(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

################################################
#### Load shape of administrative districts ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"

##############################################
#### Start loop thorugh predictive models ####
for (i in seq_along(modelList)){
  dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)
  
  ######################################################################################################################################
  #### Loop thorugh 1999 to 2015 to make predictions for each year (stored in predictData_train and predictData_train_anomaly) ####
  
  ######################
  #### Prepare loop ####
  ## Set years to loop through ##
  listyear <- seq(1999, 2015)
  
  ## Create container to store predicted data of each annual chunk ##
  predictData_train_anomaly_allyears  <- data.frame()
  
  #### Choose color Setting for plotting ####
  ## Absolute
  # summary(predictData_train_sf)
  # myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  # sc_abs <- scale_fill_gradientn("Yield Deviation", colours = myPalette(100), limits=c(200,700))
  
  ## Anomaly
  summary(Maize_meteo$siloMaizeAnomaly)
  myPalette_anomaly <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_anomaly <- scale_fill_gradientn("Yield Deviation", colours = myPalette_anomaly(100), limits=c(-150, 150))
  
  
  ## Start loop trough 17 years in training data ##
  for(m in 1:17){
    ## Create data.frame for the year m ##
    Maize_meteo_year <- Maize_meteo %>% filter(year == listyear[[m]] )
    str(Maize_meteo_year)
    Maize_meteo_year$comId
    
    # ################################################
    # #### Predict YIELD model on that data.frame ####
    # predict_year <- as.data.frame(predict(modelList[[i]], newdata = Maize_meteo_year ))
    # names(predict_year) <- "Yield_abs"
    # 
    # ## Cbind time and Spatial Information ##
    # predict_year <- cbind(Maize_meteo_year[,c(1:2)], predict_year)
    # str(predict_year)
    
    # ########################################################
    # #### Predict YIELD ANOMALY model on that data.frame ####
    # predict_year_anomaly_post  <- as.data.frame(as.numeric(predict_year$Yield_abs) - as.numeric(Maize_meteo_year$avgYield_comId))
    # names(predict_year_anomaly_post) <- "Yield_anomaly_expost"
    # str(predict_year_anomaly_post)
    # 
    # ## Cbind Spatial Information ##
    # predict_year_anomaly_post <- cbind(Maize_meteo_year[,c(1:2)], predict_year_anomaly_post)
    # str(predict_year_anomaly_post)
    
    ##############################################################
    #### Predict YIELD ANOMALY (PRE) model on that data.frame ####
    predict_year_anomaly_pre  <- as.tibble(predict(modelList[[i]], newdata = Maize_meteo_year))
    names(predict_year_anomaly_pre) <- c("Yield_anomaly")
    
    
    ## Cbind Spatial Information ##
    predict_year_anomaly_pre <- bind_cols(Maize_meteo_year[,c(1:5)], predict_year_anomaly_pre)
    predict_year_anomaly_pre 
    
    # ############################################
    # #### Compare expost and exant anomalies ####
    # head( predict_year_anomaly_post)
    # head( predict_year_anomaly_pre)
    # 'Both have exactly the same results -> only work with ex-ante yield anomalies !!!!!!!!!!!!!!!!!!!!!!!!!!'
    # 
    
    ####################################################
    #### Define PRE setting as standard for anomaly ####
    predict_year_anomaly  <- predict_year_anomaly_pre
    str(predict_year_anomaly)
    names( predict_year_anomaly)[3] <-"Yield_anomaly"
    
    # ################################
    # #### Change names to year m ####
    # names(predict_year) <-  c("comId", "year", "com", listyear[[m]])
    # names(predict_year_anomaly ) <- c("comId", "year", "com",  paste(listyear[[m]],"anomaly",sep="_"))
    # 
    #########################################################################################################
    #### Append to create a large data.frame including all comIds (334) and both yield and yield anomaly ####
    ## Create containers with comIds from external source including all 334 comId used ##
    predictData_train <- as.data.frame(avgYield_comId$comId)
    colnames(predictData_train) <- "comId"
    str(predictData_train)
    
    #### Merge predicted data with those containers representing 334 comIds ####
    predictData_train <- merge(predictData_train, predict_year, by="comId", all.x=T)
    str(predictData_train)
    
    #### Merge this data.frame with anomaly data ####
    # str(predictData_train)
    # str( predict_year_anomaly)
    predictData_train_anomaly <- merge( predictData_train , predict_year_anomaly , by=c("comId","year"), all.x=T)
    str(predictData_train_anomaly)
    
    ##############################
    #### Plot predicted data ####
    ############################
    
    ####################################
    #### Create spatial data.frame #####
    predictData_train_sf <- NULL
    predictData_train_sf <- merge(vg2500_krs, predictData_train_anomaly, by="comId")
    str(predictData_train_sf)
    names(predictData_train_sf) <- gsub("X","", names(predictData_train_sf))
    # names(predictData_train_sf) <- gsub(".y","", names(predictData_train_sf))
    
    # predictData_train_sf$year.x <- NULL
    
    str(predictData_train_sf )
    
    ## Anomaly
    predictData_train_sf_anomaly_plot  <- 
      ggplot(   predictData_train_sf) + 
      geom_sf(data=vg2500_krs, fill="gray", color="white")  + 
      geom_sf(aes(fill =  predictData_train_sf$Yield_anomaly ))  +  
      guides(fill = guide_legend(title = "Predicted Yield Anomaly")) + 
      sc_anomaly +
      ggtitle(paste(listyear[m])) + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5)) 
    
    ## Absolut
    predictData_train_sf_absolut_plot  <- 
      ggplot(   predictData_train_sf) + 
      geom_sf(data=vg2500_krs,fill="gray", color="white") + 
      geom_sf(aes(fill =  predictData_train_sf$Yield_abs ))  +  
      guides(fill = guide_legend(title = "Predicted Yield")) + 
      sc_abs + 
      ggtitle(paste( listyear[m] ))  + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5)) 
    
    
    #### Save the plots ####
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf", sep=""),predictData_train_sf_anomaly_plot , device="pdf", width=8, height= 8) 
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_", listyear[m],".pdf", sep=""),     predictData_train_sf_absolut_plot, device="pdf", width=8, height= 8) 
    
    ########################################################
    #### Create on large data frame including all years ####
    names(predictData_train_anomaly) <- c( "comId", "year", "yield_predicted", "yield_anomaly_predicted")
    predictData_train_anomaly_allyears <- rbind(predictData_train_anomaly_allyears ,  predictData_train_anomaly  )
    dim(predictData_train_anomaly_allyears)
    dim(predictData_train_anomaly)
  } ## End of loop through 17 years in training data
  
  
  ###########################################################################################################################
  #### Export the data.frame including the maize yield and maize yield deviations (anomalies) of the period 1999 - 2015 ####
  #########################################################################################################################
  str(predictData_train_anomaly_allyears)
  write.csv(predictData_train_anomaly_allyears, paste("./data/data_processed/Train/", modelListNames[[i]], "/Yield_predict_allYears.pdf", sep="" ))
  
} # Close loop through to predictive models
rm(list=ls())

###############################################################################################
#### Make Plots of sums the anomalies within each comID and the time series of each comID ####
#############################################################################################

################################################
#### Prepare loop thorugh predictive models ####
modelListNames <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jul_comId")

################################################
#### Load shape of administrative districts ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"

#### Read in avgYield_comId to extract 334 coms ####
avgYield_comId <- read.csv( file="./data/data_processed/avgYield_comId.csv")
avgYield_comId$X <- NULL
str(avgYield_comId)


#### Make comId list and comIdName list ####
comList <- merge(avgYield_comId, vg2500_krs, by="comId")
str(comList)

comId_list <- comList$comId
str(comId_list)
comIdName_list <- comList$GEN
str(comIdName_list)

##############################################
#### Start loop thorugh predictive models ####
for (i in 1:2){
  dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)
  
  
  predictData_train_anomaly_allyears <- read.csv(paste("./data/data_processed/Train/", modelListNames[[i]], "/Yield_predict_allYears.pdf", sep="" ))
  predictData_train_anomaly_allyears$X <- NULL
  str(predictData_train_anomaly_allyears)
  
  ############################################################################
  #### Add spatial information - make sf data.frame to allow map plotting ####
  predictData_train_anomaly_allyears_sf <- merge(vg2500_krs, predictData_train_anomaly_allyears, by = "comId") 
  str(predictData_train_anomaly_allyears_sf)
  
  ##########################################################
  #### Calculate sums of the predictions for each comId ####
  predictData_train_sums <- 
    predictData_train_anomaly_allyears_sf %>% 
    group_by(comId)  %>% 
    summarise(comIdsums = sum(yield_anomaly_predicted))
  
  summary(predictData_train_sums )
  str(predictData_train_sums )
  
  ##########################################################################
  #### Plot sums of Predictions over years of each comId in train Data ####
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn(colours = colorRampPalette((brewer.pal(11, "BrBG")))(100))
  
  predictData_train_sums_plot <- 
    ggplot(predictData_train_sums) + 
    geom_sf(aes(fill = comIdsums)) + 
    ggtitle("Sums of annual predictions")  + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    sc
  
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf", sep=""),  predictData_train_sums_plot , device="pdf", width=8, height= 8) 
  
  
  ###########################################################################
  #### Loop to plot time series of yield and yield anomaly of each year ####
  #########################################################################
  for (r in 1:length(comId_list)){
    
    ## Filter for each comID
    predictData_train_anomaly_allyears_year   <-
      predictData_train_anomaly_allyears  %>%
      filter(comId == comId_list[[r]])
    
    #### Plot yield ####
    timeseries <- ggplot(predictData_train_anomaly_allyears_year , aes(year, yield_predicted  )) +
      # ylim(250, 650) +
      geom_point(size=0.5, color="grey")    +
      # # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
      geom_hline(aes(yintercept = mean(yield_predicted ))) +
      geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5)    +
      geom_smooth(color="green", se = FALSE,fill="red", size=1.5)      +
      # # geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
      ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
      theme_minimal()   +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Silage Maize Yield")
    
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yield_", comId_list[r], ".pdf", sep=""),
           plot = timeseries , width=14, height=8)
    
    #### Plot yield anomalies ####
    timeseries_anomaly <- ggplot(predictData_train_anomaly_allyears_year , aes(year, yield_anomaly_predicted  )) +
      ylim(-200, 200) +
      geom_point(size=0.5, color="grey") + 
      # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
      geom_hline(aes(yintercept = mean(yield_anomaly_predicted))) +
      geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
      geom_smooth(color="green", se = FALSE, fill="red", size=1.5) +
      # geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
      ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
      theme_minimal() +  
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Silage Maize Yield Anomaly")
    
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_", comId_list[r], ".pdf", sep=""),
           plot = timeseries_anomaly , width=14, height=8)
  } ## End of Loop to produce time series  
  
  
} ## END OF LOOP WHICH LOOPS THROUGH THE DIFFERENT PREDICTIVE MODELS 


rm(list=ls())
