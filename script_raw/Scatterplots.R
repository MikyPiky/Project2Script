#### Description ####
'
This Script serves to make varios scatterplots 
1) Scatterplot of the time period 1999 - 2015 for each prediction model and RCM (10 plots)

'


#### Input ####
'
- Observed Data: Maize_meteo (tify data.frame after preparation scheme, i.e. anomalies etc) -> /Proj2/data/data_processed/
- Simulated data 
  
'
###################
## Load Packages ##
source("./script/script_raw/Packages.R")
source("./script/script_raw/BaseModel.R")


###############################
#### Load observation data ####
#### Read in tidy Dataframe for Maize ####
Maize_meteo <- read_csv("./data/data_processed/Maize_meteo.csv")
Maize_meteo


#### Ony select relevant variables from observational data ####
siloMaize_obs <- Maize_meteo %>% select(comId, stateId, year, siloMaize, siloMaizeAnomaly)
siloMaize_obs
rm(Maize_meteo)

# names(siloMaize_obs)[[4]] <- "siloMaize_obs"
# names(siloMaize_obs)[[5]] <- "siloMaizeAnomaly_obs"
# str(siloMaize_obs)

################################################################################
#### Load all predictions on training data with different prediction models ####
siloMaize_sim_train <-  read_csv( "./data/data_processed/Maize_meteo_predicted.csv")

######################################################################
#### Load simulated data derived from different prediction models ####
siloMaize_sim_climate_all <-  read_csv("./data/data_proj/output/Climate_predicted_allRCMs.csv")
siloMaize_sim_climate_all

#### Ony select relevant variables from observational data ####
siloMaize_sim_climate_all <- siloMaize_sim_climate_all %>% select(comId:year, contains("sMA"), RCM)

####################################################################################
#### Histogramm of observed and fitted maize yield for time period 1999 - 2015 #### 
##################################################################################

###############################################
#### Summary siloMaize_obs and siloMaize_sim_train ####
summary(siloMaize_sim_train)
siloMaize_sim_train


##################################################
#### Make a tidy.data. frame form Maize train ####
siloMaize_sim_train_sma <- siloMaize_sim_train %>% select(siloMaizeAnomaly:sMA_mgcv_SMI_6_Jun_Aug)
Maize_train_tidier <- siloMaize_sim_train_sma %>% gather(type, yield, siloMaizeAnomaly:sMA_mgcv_SMI_6_Jun_Aug)
# View(Maize_train_tidier)
unique(Maize_train_tidier$type)

####################################################################
#### Frequency Polygon  - Maize Anomaly - ALL PREDICTIVE MODELS ####


freqpoly <- ggplot(Maize_train_tidier, aes(yield, colour=type  )) +
  xlim(-250 , 250) +
  ylim(0, 750) +
  ggtitle("Maize Anomaly") + 
  geom_freqpoly(bins=100)

density <- ggplot(Maize_train_tidier, aes(yield, colour=type  )) +
  xlim(-250 , 250) +
  ylim(0, 0.03) +
  ggtitle("Maize Anomaly") + 
  geom_density()

ggsave(paste("./figures/figures_exploratory/Train/Histogram/FreqPoly_1999-2015_Anomalies_allModels.pdf", sep=""), 
       plot = freqpoly, width=12, height=8)
ggsave(paste("./figures/figures_exploratory/Train/Histogram/Density_1999-2015_Anomalies_allModels.pdf", sep=""), 
       plot = density, width=12, height=8)

########################################################################
#### Filter for  "sMA_lm.fit_SMI_6_Jun_Aug"  and "siloMaizeAnomaly" ####
target <- c("sMA_lm.fit_SMI_6_Jun_Aug", "siloMaizeAnomaly")
Maize_train_tidier_sMA_lm.fit_SMI_6_Jun_Aug <-  Maize_train_tidier  %>% filter(type %in% target)
Maize_train_tidier_sMA_lm.fit_SMI_6_Jun_Aug 

freqpoly_sMA_lm.fit_SMI_6_Jun_Aug <- ggplot(Maize_train_tidier_sMA_lm.fit_SMI_6_Jun_Aug, aes(yield, colour=type  )) +
  xlim(-250 , 250) +
  ylim(0, 750) +
  ggtitle("Maize Anomaly") + 
  geom_freqpoly(bins=100)

density_sMA_lm.fit_SMI_6_Jun_Aug <- ggplot(Maize_train_tidier_sMA_lm.fit_SMI_6_Jun_Aug, aes(yield, colour=type  )) +
  xlim(-250 , 250) +
  ylim(0, 0.03) +
  ggtitle("Maize Anomaly") + 
  geom_density()

ggsave(paste("./figures/figures_exploratory/Train/Histogram/FreqPoly_1999-2015_Anomalies_allModels_sMA_lm.fit_SMI_6_Jun_Aug.pdf", sep=""), 
       plot = freqpoly_sMA_lm.fit_SMI_6_Jun_Aug, width=12, height=8)
ggsave(paste("./figures/figures_exploratory/Train/Histogram/Density_1999-2015_Anomalies_allModels_sMA_lm.fit_SMI_6_Jun_Aug.pdf", sep=""), 
       plot = density_sMA_lm.fit_SMI_6_Jun_Aug, width=12, height=8)


#########################################
#### Loop through prediction models ####
#######################################
modelListNames
for (r in seq_along(modelListNames))
  {
  #### Create one directory for all scatterplot seperating by predictive models
  dir.create(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]], sep=""),showWarnings = F)
  
  #####################################################################################################
  #### Scatterplot of time period 1999 - 2015 of simulated data using observed inputs of the time ####
  ###################################################################################################
  scatterplot_train <-
    ggplot(siloMaize_sim_train, aes_string(  names(siloMaize_sim_train[,6+r]),"siloMaizeAnomaly")) +
    scale_x_continuous(limits = c(-200,200)) +
    scale_y_continuous(limits = c(-200,200)) +
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes_string("siloMaizeAnomaly", names(siloMaize_sim_train[,6+r])),
                method=lm,
                se=T,
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015 - Fitted vs. Observed")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Train_1999-2015.pdf", sep=""), 
         plot =   scatterplot_train, width=11, height=8)
  
  ###################################
  #### Plot conditional on state ####
  scatterplot_train_state <-
    ggplot(siloMaize_sim_train, aes_string(  names(siloMaize_sim_train[,6+r]),"siloMaizeAnomaly", color="state")) +
    scale_x_continuous(limits = c(-200,200)) +
    scale_y_continuous(limits = c(-200,200)) +
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes_string("siloMaizeAnomaly", names(siloMaize_sim_train[,6+r])),
                method=lm,
                se=F,
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015 - Fitted vs. Observed")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Train_1999-2015_State.pdf", sep=""), 
         plot =   scatterplot_train_state, width=11, height=8)
  

 
  

  ####################################################################################################################
  #### Scatterplot of time period 1999 - 2015 of simulated data with RCMs against observed data - colors by RCMS ####
  ##################################################################################################################
  

  # siloMaize_sim_climate_all_19992015  <- siloMaize_sim_climate_all %>% filter(year>=1999) %>% filter(year<=2015)
  
  

  #### Merge siloMaize_sim and siloMaize_obs - natural / inner join: only years 1999 - 2015 are maintained #####
  Maize_1999_2015_RCMs <- inner_join(siloMaize_sim_climate_all, siloMaize_obs, by=c("comId", "year", "stateId"))

  #################################################
  #### Plot Anomaly Values conditional on RCMs ####
  scatterplot_1999_2015_RCMs <-
    ggplot(Maize_1999_2015_RCMs, aes_string(names(Maize_1999_2015_RCMs)[5+r], "siloMaizeAnomaly" , color = "RCM")) +
    scale_x_continuous(limits = c(-250, 250)) +
    scale_y_continuous(limits = c(-250, 250)) +
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes_string(names(Maize_1999_2015_RCMs)[5+r], "siloMaizeAnomaly" , color = "RCM") ,
                method=lm,  
                se=F,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Climate_1999-2015_RCMs.pdf", sep=""), 
         plot =   scatterplot_1999_2015_RCMs, width=11, height=8)
  
  
  #################################################
  #### Plot Anomaly Values conditional on State ####
  scatterplot_1999_2015_State <-
    ggplot(Maize_1999_2015_RCMs, aes_string(names(Maize_1999_2015_RCMs)[5+r], "siloMaizeAnomaly" , color = "state")) +
    scale_x_continuous(limits = c(-250, 250)) +
    scale_y_continuous(limits = c(-250, 250)) +
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes_string(names(Maize_1999_2015_RCMs)[5+r], "siloMaizeAnomaly" , color = "state"),
                method=lm,  
                se=F,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Climate_1999-2015_State.pdf", sep=""), 
         plot = scatterplot_1999_2015_State, width=11, height=8)
  
  #### Density Plots #####
  density <- ggplot(Maize_1999_2015_RCMs, aes_string(names(Maize_1999_2015_RCMs)[5+r], colour= "RCM"  )) +
    xlim(-250 , 250) +
    ylim(0, 0.03) +
    ggtitle("Period 1999 to 2015") + 
    geom_density() + 
    geom_density(mapping = aes_string("siloMaizeAnomaly"),show.legend = TRUE, inherit.aes = TRUE) 
    
  
  ggsave(paste("./figures/figures_exploratory/Train/Histogram/FreqPoly_1999-2015_Anomalies_allModels.pdf", sep=""), 
         plot = freqpoly, width=12, height=8)

  

  ###################################################################################################################################
  #### Scatterplot of subsets of time period 1971 - 2000 in simulated data against observed data for 1999 - 2015, colors by RCMS ####
  ###################################################################################################################################
  
  #### Storage for different periods ####
  # Maize_scatter_long_period <- list(period_1971 = list(), period_1977 = list(), period_1984 = list())
  
  #### List of periods ####
  period_list <- list(seq(1971, 1984), seq(1987,2000))
  period_list[[1]][[1]]
  period_list[[2]][[1]]
  
  ###################################################
  #### Loop through subset of period 1971 - 2000 ####
  for (n in 1:length(period_list[[1]]))
    {
    
    ###############################################
    #### Store data of each RCM for one period #### 
    Maize_scatter_long <- NULL 
    Maize_scatter_long <-  data.frame()
  
    Maize_scatter_long_anomaly <- NULL 
    Maize_scatter_long_anomaly <-  data.frame()
  
    ###########################  
    #### Loop through RCMS ####
    for (i in 1:length(namelist_RCMs))
      {
      siloMaize_sim_climate_all
      
      ##################################################################################  
      ##### Extract data of the a subset of the time period 1971 - 2000  of one RCM ####
      siloMaize_sim <- siloMaize_sim_climate_all %>% 
        filter(RCM == namelist_RCMs[[i]]) %>% # filte for RCM
        filter(year >= period_list[[1]][n] & year <= period_list[[2]][n]) %>% # set subset of time period  
       select(comId, state,  names(siloMaize_sim_climate_all)[5+r])
      # 
      # names(siloMaize_sim)[[2]] <- "siloMaizeAnomaly_sim"
      # names(siloMaize_sim)[[3]] <- "MaizeAnomaly_sim"
      # str(siloMaize_sim)
      
      ######################################################
      #### Merge with comIds derived from observations ####
      ####################################################
      'Hier (simulate data) gibt es 334 comIds, bei den Obs gib es unterschiedlich viele, da es für bestimmte Jahre keine Beobachtungen gibt. 
      Daher brauche auch zusätzlich die Jahre um mit den simulierten Daten mergen-
      '
      ##########################################################################
      #### Make fake year column for merging - time period of observed data ####
      year <- as.data.frame(rep(seq(1999,2015), 406))
      names(year) <- "year"
      str(year)
      # 
      ########################################################################
      #### Combine simulated data and year from observed data for merging ####
      siloMaize_sim_merge <- cbind(siloMaize_sim, year)
      str(siloMaize_sim_merge)
      # names(siloMaize_sim_merge) <- c("comId", "year")

      # #########################################################################################
      # #### Merge siloMaize_obs with siloMaize_sim_merge to get the same comIds as in observed data ####
      # str(siloMaize_obs)
      # str(siloMaize_sim)
      # str(siloMaize_sim_merge)
      
      siloMaize_obs_merge <- as_tibble(inner_join(  siloMaize_sim_merge , siloMaize_obs, by=c("comId", "year")))
      siloMaize_obs_merge
   
      
      # View(siloMaize_obs_merge)
      
      #### Drop rows with NAs ####
      Maize_merge <- na.omit(siloMaize_obs_merge)
      
      ##################################
      #### Sort yield anomaly data ####
      ################################
      
      ############################
      #### Sort siloMaize_sim ####
      siloMaize_sim_sorted  <-   Maize_merge[order(Maize_merge[[2]]), ] %>% select(comId, state, year, names(Maize_merge)[3])
      siloMaize_sim_sorted
      
      # siloMaize_sim_sorted$siloMaize_obs <-siloMaize_sim_sorted$siloMaizeAnomaly_obs <- siloMaize_sim_sorted$MaizeAnomaly_sim <-  NULL
      # str(  siloMaize_sim_sorted )
      
      ##############################
      #### Sort siloMaize_obs_merge ####
      siloMaize_obs_sorted  <-   Maize_merge[order(Maize_merge[[6]]), ] %>% select(comId, state,year, siloMaizeAnomaly)
      siloMaize_obs_sorted 
      
      #####################################################
      #### Cbind siloMaize_obs_sorted and siloMaize_sim_sorted ####
      Maize_scatter <- inner_join( siloMaize_obs_sorted, siloMaize_sim_sorted, by=c("comId", "year", "state"))
      Maize_scatter
      
      ######################
      #### Append model ####
      Maize_scatter$RCM <- factor(rep(namelist_RCMs[[i]], dim(  Maize_scatter )[1] ))
      
      Maize_scatter_long <- rbind(Maize_scatter_long, Maize_scatter)
      
      
       }
  Maize_scatter_long
  
  
  # Maize_scatter_long_period[[n]] <- Maize_scatter_long
  # str(Maize_scatter_long_period)
  
  
  #############################################
  #### Plot Scatterplot of Anomaly Values ####
  ###########################################
  ## conditional on RCMs ##
  scatterplot_RCMs <-
    ggplot( Maize_scatter_long, aes_string( names(Maize_scatter_long)[5],"siloMaizeAnomaly ", color = "RCM")) +
    scale_x_continuous(limits = c(-250, 250)) +
    scale_y_continuous(limits = c(-250, 250)) +
    geom_point(shape=1)  +  
    # geom_smooth(mapping = aes(x=siloMaize_sim, y=siloMaize_obs),
    #  method=lm,
    #  se=T,
    #  fullrange=TRUE,
    #  inherit.aes = T) +
    geom_abline() + 
    ggtitle( paste("Period", period_list[[1]][n], "to", period_list[[2]][n] )) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Climate_", period_list[[1]][n], "-", period_list[[2]][n], "_RCMs.pdf", sep=""), 
         plot = scatterplot_RCMs, width=11, height=8)

  ## conditional on States ##
  scatterplot_state <-
    ggplot( Maize_scatter_long, aes_string( names(Maize_scatter_long)[5],"siloMaizeAnomaly ", color = "state")) +
    scale_x_continuous(limits = c(-250, 250)) +
    scale_y_continuous(limits = c(-250, 250)) +
    geom_point(shape=1)  +  
    # geom_smooth(mapping = aes(x=siloMaize_sim, y=siloMaize_obs),
    #  method=lm,
    #  se=T,
    #  fullrange=TRUE,
    #  inherit.aes = T) +
    geom_abline() + 
    ggtitle( paste("Period", period_list[[1]][n], "to", period_list[[2]][n], "- Anomaly" )) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Scatterplots/", modelListNames [[r]],"/Climate_", period_list[[1]][n], "-", period_list[[2]][n], "_state.pdf", sep=""), 
         plot = scatterplot_state, width=11, height=8)
  
  }  
}
