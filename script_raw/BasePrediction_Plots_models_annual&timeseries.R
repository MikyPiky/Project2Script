############################################################################
#### Plots of Crop Yield (SM) predicted in BasePrediction_projection.R  ####
############################################################################

' Achtung, hier gibt es Probleme bei den comId spezifischen Time Series, bisher 12064,12071. Dort hängt sich R auf.'

#### Description ####
'
Here I look at annula plots of yield and yield anomaly for each comId derived from different prediction models
- Preparation for loop 
- Loop through prediction models
  - Read in tidy data of prediction employed in BasePrediction.R and spatial information
  - Loop through differrent prediction models
    - Loop through climate models
      - Loop trough each year 1951 - 2099
        Makes plots of Yield and Yield anomaly (referential to period 1999 - 2015)

Here I look at time series plots of yield and yield anomaly for each comId derived from different prediction models
- Preparation for loop 
- Loop through prediction models
  - Read in tidy data of prediction employed in BasePrediction.R and spatial information
  - Loop through differrent prediction models
    - Loop through climate models
      - Loop trough each each comId
        Makes plots of time series of Yield and Yield anomaly (referential to period 1999 - 2015)

'



#### Input ####
'
Spatial Information: Shapefile of comdIDs ("vg2500_krs")_
  - vg2500_krs -> data_proj_Input/CLC 
BasePrediction_projections.R: tidy.data.frames of yield and yield anomaly predictions based on different estimation models:
  - "./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv"


'

#### Output ####
'
Annual Maps:    "./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/Annual/plot_annual_yield_", 
                    year_list[r], "_", namelist_models[[t]],".pdf"
Time Series: "./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/TimeSeries/plot_annual_yield_", 
years_list[r], "_", namelist_models[[t]],".pdf"
*modelListMatrixNames[[s]] -> predictive model used
*namelist_models[[t]] -> climate model


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
library(tidyr)
library(gridExtra)
library(cowplot)
library(grid)
library(tidyverse)
library(sf)
library(ggplot2) 
####################################################################################################################################################################



##############################################################################################################################################################################
##############################################################################################################################################################################
##############################################################
#### Plot Maps of predicted data (yearly considerations) ####
############################################################

###############################
#### Preparation for loop ####
#############################

#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

## Change RS to five digits ##
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS


#### Create lists for the loop ####
## Create List of models to loop trrough##
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")

## List of Names used to store the figures ##
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")


## List of Names used in figures ##
modelListYieldNames <-list("Model: combined", "Model: July")

##################################################
#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){
  
  dir.create(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] ,sep=""),showWarnings = FALSE)
  
  
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  ' one large data.frame also including a marker for the model'
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep="") )
  str(PredictData_df_tidy) # 195190/149/5 = 262
  levels(PredictData_df_tidy$model)
  PredictData_df_tidy$X <- NULL
  
  #### Change Y_anomaly_*  to Y_anomaly ####
  names(PredictData_df_tidy) <- c("model" ,"comId","year", "Y","Y_anomaly")
  
  
  ############################################################################################################################################################
  #### Loop through all 5 climate models to create Means and SDs of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099)  ####
  ##########################################################################################################################################################
  for (t in 1:length(namelist_models)){
    ##################################################################################################################
    #### Loop to plot yield and yield anomaly of each year ####
    ################################################################################################################
    #### Make list of years ####
    length(unique(PredictData_df_tidy$comId))
    year_list <- seq(1951,2099)
    
    for (r in 1:length(year_list)){
      PredictData_df_tidy_year   <-
        PredictData_df_tidy    %>%
        filter(year == year_list[[r]])
      

       # #### Merge with Spatial Information ####
      PredictData_df_tidy_year_sf <- merge(vg2500_krs,   PredictData_df_tidy_year  , by.x = "RS", by.y = "comId")
      str(PredictData_df_tidy_year_sf)
       
      ####################
      #### Plot yield ####
      summary(PredictData_df_tidy_year_sf$Y)
      myPalette <- colorRampPalette((brewer.pal(9, "YlGn")))
      sc <- scale_fill_gradientn("Yield", colours = myPalette(100), limits=c(250, 650))
      
      plot_yield_comId <-
        ggplot(PredictData_df_tidy_year_sf) +
        geom_sf(data = vg2500_krs, fill = "gray") +
        geom_sf(aes(fill = Y)) + 
        facet_wrap(~ model) +
        ggtitle(paste(year_list[[r]])) +
        sc +
        theme_bw() +        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/Annual/plot_annual_yield_", year_list[r], ".pdf", sep=""), plot = plot_yield_comId , width=10, height=8)
      
      ##############################
      #### Plot yield anomalies ####
      summary(PredictData_df_tidy_year_sf$Y_anomaly )
      myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
      sc <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(-200, 200))
      
      plot_yield_comId_anomaly <-
        ggplot(PredictData_df_tidy_year_sf) +
        geom_sf(data = vg2500_krs, fill = "gray") +
        geom_sf(aes(fill = Y_anomaly )) +
        facet_wrap(~ model) +
        ggtitle(paste(year_list[[r]])) +
        sc +
        theme_bw() +        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/Annual/plot_annual_yieldAnomaly_", year_list[r], ".pdf", sep=""), plot = plot_yield_comId_anomaly ,width=10, height=8)
      
    } ## End of loop through years
    
  } ## End of loop through all five climate models -> index is t
  
  
} ## End of loop through models -> index is s

#





##############################################################################################################################################################################
############################################
#### Plot comID specific time - series ####
##########################################
##############################################################################################################################################################################


###############################
#### Preparation for loop ####
#############################

#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

## Change RS to five digits ##
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS


# #### Create lists for the loop ####
# ## Create List of models to loop trrough##
# namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")

## List of Names used to store the figures ##
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")


## List of Names used in figures ##
modelListYieldNames <-list("Model: combined", "Model: July")

##################################################
#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){

  dir.create(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] ,sep=""),showWarnings = FALSE)

  ######################################################################
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  ' one large data.frame also including a marker for the model'
  PredictData_df_tidy1 <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep="") )
  PredictData_df_tidy1$X <- NULL
  str(PredictData_df_tidy1) # 248830/149/5 = 334
  
  ##################################################################################################################
  #### Generate data.frame which substitues various model by "All Models"  to allow "All Models" facet in plots ####
  PredictData_df_tidy2 <- PredictData_df_tidy1
  PredictData_df_tidy2$model <- as.factor(rep("All Models", dim(PredictData_df_tidy2)[1]))
  str(PredictData_df_tidy2)
  
  ##################################
  #### Combine both data.frames ####
  PredictData_df_tidy <- rbind(PredictData_df_tidy2,PredictData_df_tidy1)
  str(PredictData_df_tidy)
  levels(PredictData_df_tidy$model)
  
  ######################################
  #### Generate means for plotting ####
  ####################################
  #### Generate mean conditional on the model and the state ####
  PredictData_df_tidy_mean <- 
    PredictData_df_tidy %>% 
    group_by(model,comId) %>%  
    summarise(com_mean_Y = mean(Y), com_mean_Y_anomaly = mean(Y_anomaly))
  
  str(PredictData_df_tidy_mean)
  summary(PredictData_df_tidy_mean)
  
  
  # ##########################################
  # #### Change Y_anomaly_*  to Y_anomaly ####
  # names(PredictData_df_tidy) <- c("model" ,"comId","year", "Y","Y_anomaly")
  # 
  
  ##########################################
  #### Load data to generate comIdNames ####
  vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
  str(vg2500_krs, 2)
  
  ## Change RS to five digits ##
  vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
  vg2500_krs$RS
  
  names(vg2500_krs)[2] <- "comId"

  ##########################################################################
  #### Make list of comIds and names of comIds derived from vg2500_krs ####
  ########################################################################
  
  #### Merge vgr2500_krs with PredictData_df_tidy ####
  PredictData_df_tidy_sf <- merge(PredictData_df_tidy, vg2500_krs, by="comId")
  str(PredictData_df_tidy_sf)
  str(PredictData_df_tidy)
  
  #### Filter for one year and RCM ####
  PredictData_df_tidy_sf_year <- PredictData_df_tidy_sf %>% filter(year==1990 & model == "DMI")
  length( PredictData_df_tidy_sf_year$comId)
  
  #### Create lists ####
  comId_list <- PredictData_df_tidy_sf_year$comId
  comIdName_list <- PredictData_df_tidy_sf_year$GEN
  
  #### rm simple feature because of size ####
  rm(PredictData_df_tidy_sf)
  
    ####################################################################################
    #### Loop to plot yield and yield time series for each administrative district ####
    ##################################################################################
    
    #### Start of loop through all comIds ####
    for (r in 1:length(comId_list)){
      
      ## Filter for each comID ##
      ## General data
      PredictData_df_tidy_year   <-
        PredictData_df_tidy  %>%
        filter(comId == comId_list[[r]])
      
      ## Mean Data
      PredictData_df_tidy_mean_year   <-
        PredictData_df_tidy_mean  %>%
        filter(comId == comId_list[[r]])
      
      #### Plot yield ####
      timeseries <- ggplot(PredictData_df_tidy_year , aes(year, Y)) +
         ylim(250, 650) +
         geom_point(size=0.5, color="grey") + 
         # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
         facet_wrap(~ model) +
         geom_hline(aes(yintercept = com_mean_Y), data = PredictData_df_tidy_mean_year) +
         geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
         geom_smooth(color="green", se = FALSE,fill="red", size=1.5) +
         geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
         ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
         theme_minimal() +  
         theme(plot.title = element_text(hjust = 0.5)) +
         ylab("Silaga Maize Yield")

      ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/TimeSeries/administrative_districts/timeSeries_yield_RCMs_", comId_list[r], ".pdf", sep=""), plot = timeseries , width=14, height=8)

      #### Plot yield anomalies ####
      timeseries_anomaly <- ggplot(PredictData_df_tidy_year , aes(year, Y_anomaly ))+
        ylim(-200, 200) +
        geom_point(size=0.5, color="grey") + 
        # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
        facet_wrap(~ model) +
        geom_hline(aes(yintercept = com_mean_Y_anomaly), data = PredictData_df_tidy_mean_year) +
        geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
        geom_smooth(color="green", se = FALSE, fill="red", size=1.5) +
        geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
        ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Silage Maize Yield Anomaly")
      
      ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_RCMs_", comId_list[r], ".pdf", sep=""), plot = timeseries_anomaly , width=14, height=8)
      
    }
  
} ## End of loop through models -> index is s

##############################################################################################################################################################################
##############################################################################################################################################################################
####################################################
#### Plot federal state specific time - series ####
##################################################

###############################
#### Preparation for loop ####
#############################

#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

## Change RS to five digits ##
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS


#### Create lists for the loop ####
## Create List of models to loop trrough##
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")

## List of Names used to store the figures ##
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")


## List of Names used in figures ##
modelListYieldNames <-list("Model: combined", "Model: July")

##################################################
#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){
  
  dir.create(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] ,sep=""),showWarnings = FALSE)
  
  ######################################################################
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  ' one large data.frame also including a marker for the model'
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep="") )
  levels(PredictData_df_tidy$model)
  PredictData_df_tidy$X <- NULL
  str(PredictData_df_tidy)   
  
  #### Change Y_anomaly_*  to Y_anomaly ####
  names(PredictData_df_tidy) <- c("model" ,"comId","year", "Y","Y_anomaly")
  str(PredictData_df_tidy$comId)
  
  
  ##################################################################
  #### Make data.frame including stateId and state abbreviaton ####
  ################################################################
  
  ## Make stateId ##
  PredictData_df_tidy$stateId <- as.factor(str_sub(PredictData_df_tidy$comId,1, -4))
  str(PredictData_df_tidy$stateId)

  ## Make state name Variables ##
  stateIdlist <-  c (1  , 3 , 5,  6,  7,  8 , 9, 10, 12, 13, 14, 15 ,16)
  stateList <- c("SH", "LS", "NRW", "HE", "RP", "BW", "BY", "SL","BB", "MV", "SN", "ST", "TH")
  
  PredictData_df_tidy_state <- list()
  
  ## Loop to produuced state and state ##
  for (l in 1:length(stateList)){
  PredictData_df_tidy_stateId <-
    PredictData_df_tidy %>% 
    filter(stateId == stateIdlist[l]) %>%
     mutate(state = as.factor(stateList[l]) )
  PredictData_df_tidy_state <- rbind(PredictData_df_tidy_state, PredictData_df_tidy_stateId)
  }
  
  str(PredictData_df_tidy_state)
  # View(PredictData_df_tidy_state)
  
  ######################################################################
  #### Generate All Models factor to allow for "All Models" - facet ####
  PredictData_df_tidy_state2 <- 
    PredictData_df_tidy_state 
  
  str(PredictData_df_tidy_state2 )
  summary(PredictData_df_tidy_state2)
  PredictData_df_tidy_state$model <- as.factor(rep("All Models", 248830))

  PredictData_df_tidy_state_all <-rbind(PredictData_df_tidy_state, PredictData_df_tidy_state2)
  str(PredictData_df_tidy_state_all)
 
  
  ######################################
  #### Generate means for plotting ####
  ####################################
  
  #### Generate mean conditional on the model and the state ####
  PredictData_df_tidy_state_mean <- 
    PredictData_df_tidy_state_all %>% 
    group_by(model,stateId) %>%  
    summarise(state_mean_Y = mean(Y), state_mean_Y_anomaly = mean(Y_anomaly))
  
  str(PredictData_df_tidy_state_mean )
  summary(PredictData_df_tidy_state_mean )
  
  ############################################################
  #### Loop to plot yield and yield anomaly of each year ####
  ##########################################################
    
  ## Names used for plotting ##
  stateList_names <- c("Schleswig-Holstein", "Lower Saxony", "North Rhine-Westphalia", "Hesse", 
                       "Rhineland-Palatinate", "Baden-Württemberg", "Bavaria", "Saarland","Brandenburg", 
                       "Mecklenburg-Vorpommern", "Saxony", "Saxony-Anhalt", "Thuringia")
    
  for (r in 1:length(stateIdlist)){
      
    ## Filter general data for each stateIdlist ##
    PredictData_df_tidy_year   <-
        PredictData_df_tidy_state_all  %>%
        filter(stateId == stateIdlist[r])  
    str(PredictData_df_tidy_year)
    
    ## Filter mean data for each stateIdlist ##
    ## Filter for each stateIdlist ##
    PredictData_df_tidy_state_mean_year   <-
      PredictData_df_tidy_state_mean %>%
      filter(stateId == stateIdlist[r])  
    
    str(PredictData_df_tidy_state_mean_year)
      
    ####################
    #### Plot yield ####
    timeseries <- ggplot(PredictData_df_tidy_year, aes(year, Y)) +
        ylim(250, 650) +
        facet_wrap(~ model) +
        geom_point(size=0.5, color="grey") + 
        geom_hline(aes(yintercept = state_mean_Y), data= PredictData_df_tidy_state_mean_year) +
        geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
        geom_smooth(color="green", size=1.5) +
        geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 100, size=1.5) +
        ggtitle(paste(stateList_names[[r]])) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Silage Maize Yield")

    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/TimeSeries/federal_states/timeSeries_yield_RCMs_", stateList[r],  ".pdf", sep=""), plot = timeseries , width=14, height=10)

    ##############################
    #### Plot yield anomalies ####
    timeseries_anomaly <- ggplot(PredictData_df_tidy_year , aes(year, Y_anomaly ))+
        ylim(-200, 200) +
        geom_point(size=0.5, color="grey") + 
        # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
        facet_wrap(~ model) +
      geom_hline(aes(yintercept = state_mean_Y_anomaly), data= PredictData_df_tidy_state_mean_year) +
      geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
        geom_smooth(color="green",fill="red", size=1.5) +
        geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
        ggtitle(paste(stateList_names[[r]])) +
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Silage Maize Yield Anomaly")
      
    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/TimeSeries/federal_states/timeSeries_yieldAnomaly_RCMs_",  stateList[r], ".pdf", sep=""), plot = timeseries_anomaly , width=14, height=10)
      
  }

} ## End of loop through models -> index is s
