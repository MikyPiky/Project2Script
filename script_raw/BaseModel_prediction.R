#### Description of Script ####
' 
  - Use models estimated in BaseModel.R to predict siloMaize yield Anomalies 
  - Loop through those models to make prediction for each year (maps) and comId (time series)
'
#### Output ####
## Files
' 
  - Maize_meteo including the predicted values from the models in BaseModel.R "./data/data_processed/Maize_meteo_predicted.csv" 
'
## Plots
' - maps of predicted silage maize anomalies for each year in the training period ->     
  "./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf"

  - com specific sum of predicred values -> /figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf"
  - time series for each com -> ./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_", 
                 comId_list$comId[r]

'



#### Dependencies and Input ####
' - BaseModel.R
  - vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")


'



###################
## Load Packages ##
source("./script/script_raw/Packages.R")


##############################################################################################################################################################################
#############################################################################################################################################################################
#############################################################################################################################################################################
# rm(list=ls())
getwd()

#################################################
#### Read in data.frame and estimate models ####
###############################################
source("./script/script_raw/BaseModel.R")


#################################################################################################################################################################
###########################################################
#### Predictions on observational data - annual junks ####
#########################################################
#################################################################################################################################################################




#######################################################################
#### Loop through those models to make predictions for each year #####
#####################################################################

################################################
#### Load shape of administrative districts ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")

#### Change RS to five digits #####
vg2500_krs$RS <- as.factor(as.integer(str_sub(vg2500_krs$RS, 1,5)))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"

######################
#### Prepare loop ####
## Set years to loop through ##
listyear <- seq(1999, 2015)

result <- data.frame(matrix(nrow = 4625, ncol = 10))


######################################################################
#### Loop thorugh 1999 to 2015 to make predictions for each year ####
####################################################################

for (i in seq_along(modelList)){

  #### Create directories output saved in ####
  # dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)
  # dir.create(paste("./data/data_processed/Train/", modelListNames[[i]], sep=""), showWarnings = F)
  'Now all files are saved in one output, compareable to climate predictions. '

  ### Create container to store predicted data of each annual chunk ####
  # predictData_train_anomaly_allyears  <- data.frame()



  ##############################################################
  #### Predict YIELD ANOMALY model using model in modelList ####
  predict_year_anomaly  <- as.tibble(predict(modelList[[i]], newdata = Maize_meteo))
  names(predict_year_anomaly) <- paste("siloMaizeAnomaly_predicted")

  #### Combine with Maize_meteo to allow spatial plots later ####
  Maize_meteo_predicted <- bind_cols( Maize_meteo[ c(1:5,8)],  predict_year_anomaly)

  ########################################################
  #### Export the data.frame of the predicted values ####
  #########################################################################################################################
  #### Include Model into name of predicted siloMais ####
  names(predict_year_anomaly) <- paste("sMA", modelListNames[i], sep="_")

  #### Combine with Maize_meteo ####
  result[,i] <-  predict_year_anomaly
  names(result )[i] <- paste("sMA", modelListNames[i], sep="_")


#   ##########################################################################
#   #### Start loop trough 17 years in training data to make plotted maps ####
#   
#   ## Define colors ##
#   Maize_meteo$siloMaizeAnomaly
#   summary(Maize_meteo$siloMaizeAnomaly)
#   myPalette_anomaly <- colorRampPalette((brewer.pal(11, "BrBG")))
#   sc_anomaly <- scale_fill_gradientn("Yield Deviation", colours = myPalette_anomaly(400), limits=c(- 200, 200))
#   
#   for(m in 1:17){
#     
#     ## Create data.frame for the year m ##
#     Maize_meteo_year <- Maize_meteo_predicted %>% select(year:state, siloMaizeAnomaly_predicted ) %>% filter(year == listyear[[m]] )
#     Maize_meteo_year
#     
#     ##############################
#     #### Plot predicted data ####
#     ############################
# 
#     ####################################
#     #### Create spatial data.frame #####
#     predictData_train_sf <- NULL
#     predictData_train_sf <- inner_join(vg2500_krs, Maize_meteo_year , by="comId")
#     predictData_train_sf
# 
# 
#     ## Anomaly
#     predictData_train_sf_anomaly_plot  <-
#       ggplot(   predictData_train_sf) +
#       geom_sf(data=vg2500_krs, fill="gray", color="white")  +
#       geom_sf(aes(fill =  predictData_train_sf$siloMaizeAnomaly_predicted ))  +
#       guides(fill = guide_legend(title = "Predicted Yield Anomaly")) +
#       sc_anomaly +
#       ggtitle(paste(listyear[m], modelListNames[i], sep = " - ")) +
#       theme_bw() +
#       theme(plot.title = element_text(hjust = 0.5))
# 
#     #### Save the plots ####
#     ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf", sep=""),predictData_train_sf_anomaly_plot , device="pdf", width=8, height= 8)
#     # ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_", listyear[m],".pdf", sep=""),     predictData_train_sf_absolut_plot, device="pdf", width=8, height= 8)
# 
# } ## End of loop through 17 years in training data

  #############################################################
  #### Greate Maize_meteo_predicted including all modells #### 
  ###########################################################
  Maize_meteo_predicted <- bind_cols( Maize_meteo[ c(1:5,8)],  result)
  


} # Close loop through to predictive models

#### Write Maize_meteo including predicted siloMaize Anomalies derived from the models in BaseModel.R ###
write_csv(Maize_meteo_predicted, "./data/data_processed/Maize_meteo_predicted.csv")




# ##############################################################################################################################################################################################
# ###############################################################################################
# #### Make Plots of sums the anomalies within each comID and the time series of each comID ####
# #############################################################################################
# ##############################################################################################################################################################################################
# rm(list=ls())
# #################################################
# #### Read in data.frame and estimate models ####
# ###############################################
# source("./script/script_raw/BaseModel.R")
# 
# 
# #################################################
# #### Load shape of administrative districts ####
# ###############################################
# vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
# vg2500_krs
# 
# #### Change RS to five digits #####
# vg2500_krs$RS <- as.factor(as.integer(str_sub(vg2500_krs$RS, 1,5)))
# vg2500_krs$RS
# names(vg2500_krs)[2] <- "comId"
# 
# #### Read in avgYield_comId to extract 334 coms ####
# avgYield_comId <- read_csv( file="./data/data_processed/avgYield_comId.csv")
# avgYield_comId
# avgYield_comId$comId <-as.factor(avgYield_comId$comId)
# 
# ###########################################################
# #### Make comId list including comIds and comIds Names ####
# comList <- inner_join(avgYield_comId, vg2500_krs, by="comId")
# comList 
# 
# comId_list <- comList %>% select (comId, GEN) %>% as.list()
# comId_list
# 
# #############################################
# #### Read in data of predicted anomalies ####
# predictData_train_anomaly_allyears <- read_csv(paste("./data/data_processed/Maize_meteo_predicted.csv", sep="" ))
# predictData_train_anomaly_allyears <- predictData_train_anomaly_allyears %>% mutate_at("comId", as.factor)
# 
# predictData_train_anomaly_allyears
# 
# ############################################################################
# #### Add spatial information - make sf data.frame to allow map plotting ####
# predictData_train_anomaly_allyears_sf <- inner_join(vg2500_krs, predictData_train_anomaly_allyears, by = "comId") 
# predictData_train_anomaly_allyears_sf
# 
# ##########################################################
# #### Calculate sums of the predictions for each comId ####
# predictData_train_sums <- 
#   predictData_train_anomaly_allyears_sf %>% 
#   group_by(comId)  %>% 
#   select( starts_with("sMA_"))  %>%
#   summarise(. = sum(.))
# 
# predictData_train_sums
# 
# 
# 
# ##############################################
# #### Start loop thorugh predictive models ####
# for (i in  seq_along(modelList)){
#   
#   ############################################
#   #### Create directories output saved in ####
#   dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)
#   dir.create(paste("./data/data_processed/Train/", modelListNames[[i]], sep=""), showWarnings = F)  
#   dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], "/TimeSeries/", sep=""), showWarnings = F)
#   dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], "/TimeSeries/administrative_districts/", sep=""), showWarnings = F)
#   
#   
#  
#   ##########################################################################
#   #### Plot sums of Predictions over years of each comId in train Data ####
#   myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
#   sc <- scale_fill_gradientn(colours = colorRampPalette((brewer.pal(11, "BrBG")))(100), limits=c(- 300, 300))
# 
#   predictData_train_sums_alone <- predictData_train_sums %>% select(names(predictData_train_sums) [1+i])
#   names( predictData_train_sums_alone )[2] <- "sMA"
#   
#   predictData_train_sums_plot <- 
#     ggplot(predictData_train_sums_alone) + 
#     geom_sf(data=vg2500_krs, fill="gray", color="white")  +
#     # guides(fill = guide_legend(label = T)) +
#     geom_sf(aes(fill =    sMA)) +
#     ggtitle(paste("Sums of annual predictions", modelListNames[i], sep=" - ") )  + theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
#     sc
#   
#   ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf", sep=""),  predictData_train_sums_plot , device="pdf", width=8, height= 8) 
# 
#   ' Hier gibt es den Unterschied das Modelle mit Fixed Effekten sich zu Null summieren, während das bei Modelle ohne nicht der Fall ist. Für mich bedeutet das, dass in den
#     Fixed Effekten Informationen sind über die Periode 1999 - 2015, welche in den reinen Anomalie Modellen fehlen. Dort sind die Daten demeaned für die Period ab 1951 sowohl
#     für die Meteorologie als auch den SMI. '
#   
#   ###########################################################################
#   #### Loop to plot time series of yield and yield anomaly of each year ####
#   #########################################################################
#   for (r in seq_along(comId_list$comId)){
#     
#     
#     ###############################
#     #### Filter for each comID ####
#     predictData_train_anomaly_allyears_year   <-
#       predictData_train_anomaly_allyears  %>%
#       filter(comId == comId_list$comId[r]) %>%
#       select(year, starts_with("sMA_")) 
#      
#     predictData_train_anomaly_allyears_year_alone <- predictData_train_anomaly_allyears_year  %>% 
#       select(year, paste(names(predictData_train_anomaly_allyears_year)[1+i]) )
#     
#     names(    predictData_train_anomaly_allyears_year_alone) <- c("year", "Yield_anomaly")
#     
#     ##########################################
#     #### Plot yield anomalies time series ####
#     timeseries_anomaly <- ggplot(predictData_train_anomaly_allyears_year_alone , aes(year,Yield_anomaly)) +
#       ylim(-200, 200) +
#       geom_point(size=0.5, color="grey") + 
#       # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
#       geom_hline(aes(yintercept = mean(Yield_anomaly))) +
#       geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
#       geom_smooth(color="green", se = FALSE, fill="red", size=1.5) +
#       # geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
#       ggtitle(paste(comId_list$comId [[r]], comId_list$GEN[[r]], modelListNames[[i]], sep = " - ")) +
#       theme_minimal() +  
#       theme(plot.title = element_text(hjust = 0.5)) +
#       ylab("Silage Maize Yield Anomaly")
#     
#     ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_", 
#                  comId_list$comId[r], ".pdf", sep=""),
#            plot = timeseries_anomaly , width=14, height=8)
#   } ## End of Loop to produce time series  
#   
#   
# } ## END OF LOOP WHICH LOOPS THROUGH THE DIFFERENT PREDICTIVE MODELS 
# 
# 
# rm(list=ls())
