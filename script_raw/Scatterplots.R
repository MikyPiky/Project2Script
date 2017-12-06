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


###############################
#### Load observation data ####
#### Read in tidy Dataframe for Maize ####
Maize_meteo <- read_csv("./data/data_processed/Maize_meteo.csv")
Maize_meteo


#### Ony select relevant variables from observational data ####
siloMaize_obs <- Maize_meteo %>% select(comId, stateId, year, siloMaize, siloMaizeAnomaly)
siloMaize_obs
rm(Maize_meteo)

names(siloMaize_obs)[[5]] <- "siloMaizeAnomaly_obs"
names(siloMaize_obs)[[4]] <- "siloMaize_obs"
str(siloMaize_obs)

#### Loop Preparation ####
# modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")
# modelListNames  <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jul_comId")
#
# namelist_RCMs <- c("DMI","ICTP", "KNMI","MPI","SMHI")




########################################
#### Loop through prediction models ####
for (r in seq_along(modelList_climate))
  {
  
  #####################################################################################################
  #### Scatterplot of timer period 1999 - 2015 of simulated data using observed inputs of the time ####
  #####################################################################################################
  siloMaize_sim_train <-  read_csv(paste("./data/data_processed/Train/", modelListNames [[r]],"/Yield_predict_allYears.csv", sep=""))
  siloMaize_sim_train$X1 <- NULL
  str(siloMaize_sim_train)
  
  names(siloMaize_sim_train)[[3]] <- "Maize_train"
  names(siloMaize_sim_train)[[4]] <- "MaizeAnomaly_train"
  
  ####################
  #### Merge Data ####
  Maize_train <- merge(siloMaize_obs, siloMaize_sim_train, by = c( "comId", "year" ))
  str(Maize_train)
  
  ##############################
  #### Plot Absolute Values ####
  scatterplot_train <-
    ggplot(Maize_train, aes(x = Maize_train, y = siloMaize_obs)) +
    scale_x_continuous(limits = c(100, 800)) + 
    scale_y_continuous(limits = c(100, 800)) + 
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes(x=Maize_train, y=siloMaize_obs),
                method=lm,  
                se=T,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015 - Fitted vs. Observed")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Scatterplot/1999-2015.pdf", sep=""), plot = scatterplot_train, width=11, height=8)
  
  #############################
  #### Plot Anomaly Values ####
  scatterplot_train_Anomalies <-
    ggplot(Maize_train, aes(x=MaizeAnomaly_train, y=siloMaizeAnomaly_obs)) +
    scale_x_continuous(limits = c(-300, 300)) +
    scale_y_continuous(limits = c(-300, 300)) +
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes(x=MaizeAnomaly_train, y=siloMaizeAnomaly_obs),
                method=lm,  
                se=T,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015 - Anomalies - Fitted vs. Observed")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Scatterplot/1999-2015_model_Anomalies.pdf", sep=""), 
         plot =   scatterplot_train_Anomalies, width=11, height=8)

  ####################################################################################
  #### Histogramm of observed and fitted maize yield for time period 1999 - 2015 #### 
  ##################################################################################
  
  
  ###############################################
  #### Summary siloMaize_obs and siloMaize_sim_train ####
  summary(Maize_train$siloMaizeAnomaly_obs)
  summary(Maize_train$MaizeAnomaly_train)
  
  ##################################################
  #### Make a tidy.data. frame form Maize train ####
  str(Maize_train)
  Maize_train_tidier <- Maize_train %>% gather(type, yield, siloMaize_obs:MaizeAnomaly_train)
  str(Maize_train_tidier)
  Maize_train_tidy <-  Maize_train_tidier %>% separate(type, into=c("yield_type", "data_type"))
  str(Maize_train_tidy)
  
  ############################################
  #### Frequency Polygon  - Maize Anomaly ####
  Maize_train_tidy_anomaly <- Maize_train_tidy %>% filter(yield_type == "MaizeAnomaly")
  str(Maize_train_tidy_anomaly)
  
  hist_Anomaly <- ggplot(Maize_train_tidy_anomaly, aes(yield, colour=data_type  )) +
    xlim(-400 , 400) +
    ylim(0, 1000) +
    ggtitle("Maize Anomaly") + 
    geom_freqpoly(bins=100)
  
  dir.create(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Histogram/", sep=""))

  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Histogram/Hist_1999-2015_Anomalies.pdf", sep=""), 
         plot = hist_Anomaly, width=10, height=8)
  
  
  ###################################
  #### Frequency Polygon - Maize ####
  Maize_train_tidy_anomaly <- Maize_train_tidy %>% filter(yield_type == "Maize")
  
  hist <- ggplot(Maize_train_tidy_anomaly, aes(yield, colour=data_type  )) +
    xlim(0 , 800) +
    ylim(0,1000) + 
    ggtitle("Maize Anomaly") + 
    geom_freqpoly(bins=100)
  
  dir.create(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Histogram/", sep=""))
  
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames [[r]],"/Histogram/Hist_1999-2015.pdf", sep=""), 
         plot = hist, width=10, height=8)
  
  
  ####################################################
  #### Kernel Densitiy Plot of observed anomalies ####
  ggplot(siloMaize_obs, aes(siloMaizeAnomaly_obs  )) +
    geom_density()
  
  ##########################################
  #### Histogramm of observed anoamlies ####
  ggplot(siloMaize_sim_train, aes(MaizeAnomaly_train )) +
    geom_histogram() 
  
  ####################################################
  #### Kernel Densitiy Plot of observed anomalies ####
  ggplot(siloMaize_sim_train, aes(MaizeAnomaly_train )) +
    geom_density()
  
  
  ####################################################################################################################
  #### Scatterplot of time period 1999 - 2015 of simulated data with RCMs against observed data - colors by RCMS ####
  ##################################################################################################################
  
  ######################################################################
  #### Load simulated data derived from different prediction models ####
  siloMaize_sim_all <-  read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[r]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep=""))
  siloMaize_sim_all$X <- NULL
  str(siloMaize_sim_all)
  
  
  ###################################
  #### Select relevant variables #### 
  # names(siloMaize_sim_all )
  siloMaize_sim <- siloMaize_sim_all
  # %>% 
  #   select(comId, year, Y,Y_anomaly, model)
  # str(siloMaize_sim)
  # str(siloMaize_sim_all)
  # 
  names(siloMaize_sim)[[4]] <- "siloMaize_sim"
  names(siloMaize_sim)[[5]] <- "MaizeAnomaly_sim"
  
  #### Merge siloMaize_sim and siloMaize_obs - natural / inner join: only years 1999 - 2015 are maintained #####
  Maize_1999_2015_RCMs <- merge(siloMaize_sim, siloMaize_obs, by=c("comId", "year"))
  str(Maize_1999_2015_RCMs)
  
  ##############################
  #### Plot Absolute Values ####
  scatterplot_1999_2015_RCMs <-
    ggplot(Maize_1999_2015_RCMs, aes(x=siloMaize_sim, y=siloMaize_obs, color = model)) +
    scale_x_continuous(limits = c(100, 800)) + 
    scale_y_continuous(limits = c(100, 800)) + 
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes(x=siloMaize_sim, y=siloMaize_obs),
                method=lm,  
                se=T,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/1999-2015_model.pdf", sep=""), 
         plot = scatterplot_1999_2015_RCMs, width=11, height=8)

  #############################
  #### Plot Anomaly Values ####
  scatterplot_1999_2015_RCMs_Anomalies <-
    ggplot(Maize_1999_2015_RCMs, aes(x=MaizeAnomaly_sim, y=siloMaizeAnomaly_obs, color = model)) +
    # scale_x_continuous(limits = c(100, 800)) + 
    # scale_y_continuous(limits = c(100, 800)) + 
    geom_point(shape=1)  +  
    geom_smooth(mapping = aes(x=MaizeAnomaly_sim, y=siloMaizeAnomaly_obs),
                method=lm,  
                se=T,   
                fullrange=TRUE,
                inherit.aes = T) +
    ggtitle(paste("Period 1999 to 2015 - Anomalies")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/1999-2015_model_Anomalies.pdf", sep=""), 
         plot = scatterplot_1999_2015_RCMs_Anomalies, width=11, height=8)
  
  #########################################################################################  
  #### Loop through RCMs to produce data.frame of absolute values ordered accordingly #####
  for (i in 1:length(namelist_RCMs)){
    
    ###################################################################
    #### Scatterplot of time period 1999 - 2015 - colors by comIds ####
    
    #######################################################
    #### Filter for RCMS and select relevant variables #### 
    siloMaize_sim <- siloMaize_sim_all %>% 
      filter(model == namelist_RCMs[[i]]) %>%
      select(comId, year, Y, Y_anomaly)
    
    str(siloMaize_sim)
    
    names(siloMaize_sim)[[3]] <- "siloMaize_sim"
    names(siloMaize_sim)[[4]] <- "MaizeAnomaly_sim"
    
    #### Merge siloMaize_sim and siloMaize_obs - natural / inner join: only years 1999 - 2015 are maintained #####
    Maize_1999_2015 <- merge(siloMaize_sim, siloMaize_obs, by=c("comId", "year"))
    str(Maize_1999_2015)
    
    ##############################################################
    #### Plot of absolute values conditional on the comStates ####
    scatterplot_1999_2015 <-
      ggplot(Maize_1999_2015, aes(x=siloMaize_sim, y=siloMaize_obs, color = comState)) +
      scale_x_continuous(limits = c(100, 800)) + 
      scale_y_continuous(limits = c(100, 800)) + 
      geom_point(shape=1)  +  
      geom_smooth(mapping = aes(x=siloMaize_sim, y=siloMaize_obs),
                  method=lm,  
                  se=T,   
                  fullrange=TRUE,
                  inherit.aes = F) +
      geom_abline(slope = 90, intercept = 100, colour="red") +
      ggtitle(paste("Period 1999 to 2015 - ", namelist_RCMs[[i]])) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/1999-2015_state_", namelist_RCMs[[i]],".pdf", sep=""), 
           plot = scatterplot_1999_2015, width=11, height=8)
    
    #############################################################
    #### Plot of anomaly values conditional on the comStates ####
    scatterplot_1999_2015_Anomalies <-
      ggplot(Maize_1999_2015, aes(x=MaizeAnomaly_sim, y=siloMaizeAnomaly_obs, color = comState)) +
      scale_x_continuous(limits = c(-300, 300)) +
      scale_y_continuous(limits = c(-300, 300)) +
      geom_point(shape=1)  +  
      geom_smooth(mapping = aes(x=MaizeAnomaly_sim, y=siloMaizeAnomaly_obs),
                  method=lm,
                  # se=T,
                  fullrange=TRUE,
                  inherit.aes = F) +
      # geom_abline(slope = 90, intercept = 100, colour="red") +
      ggtitle(paste("Period 1999 to 2015 - ", namelist_RCMs[[i]], "Anomaly")) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/1999-2015_state_", namelist_RCMs[[i]],"_Anomalies.pdf", sep=""), 
           plot = scatterplot_1999_2015_Anomalies, width=11, height=8)
    
    
  }
  
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
    str(siloMaize_sim_all)  
    
    ##################################################################################  
    ##### Extract data of the a subset of the time period 1971 - 2000  of one RCM ####
    siloMaize_sim <- siloMaize_sim_all %>% 
      filter(model == namelist_RCMs[[i]]) %>% # filte for RCM
      filter(year >= period_list[[1]][n] & year <= period_list[[2]][n])    %>% # set subset of time period  
      select(comId, Y, Y_anomaly)
    
    names(siloMaize_sim)[[2]] <- "siloMaize_sim"
    names(siloMaize_sim)[[3]] <- "MaizeAnomaly_sim"
    str(siloMaize_sim)
    
    ######################################################
    #### Merge with comIds derived from observations ####
    ####################################################
    'Hier (simulate data) gibt es 334 comIds, bei den Obs gib es unterschiedlich viele, da es für bestimmte Jahre keine Beobachtungen gibt. 
    Daher brauche auch zusätzlich die Jahre um mit den simulierten Daten mergen-
    '
    ##########################################################################
    #### Make fake year column for merging - time period of observed data ####
    year <- as.data.frame(rep(seq(1999,2015), 334))
    names(year) <- "year"
    str(year)
    
    ########################################################################
    #### Combine simulated data and year from observed data for merging ####
    siloMaize_sim_merge <- cbind(siloMaize_sim, year)
    str(siloMaize_sim_merge)
    # names(siloMaize_sim_merge) <- c("comId", "year")
    
    #########################################################################################
    #### Merge siloMaize_obs with siloMaize_sim_merge to get the same comIds as in observed data ####
    str(siloMaize_obs)
    str(siloMaize_sim)
    str(siloMaize_sim_merge)
    
    siloMaize_obs_merge <- merge(siloMaize_obs, siloMaize_sim_merge,  by=c("comId", "year"))
    str(siloMaize_obs_merge)
    
    # View(siloMaize_obs_merge)
    
    #### Drop rows with NAs ####
    Maize_merge <- na.omit(siloMaize_obs_merge)
    
    ###################################
    #### Sort absolute yield data ####
    #################################
    
    ############################
    #### Sort siloMaize_sim ####
    siloMaize_sim_sorted  <-   Maize_merge[order(Maize_merge$siloMaize_sim), ]
    siloMaize_sim_sorted$siloMaize_obs <-siloMaize_sim_sorted$siloMaizeAnomaly_obs <- siloMaize_sim_sorted$MaizeAnomaly_sim <-  NULL
    str(  siloMaize_sim_sorted )
    
    ##############################
    #### Sort siloMaize_obs_merge ####
    siloMaize_obs_sorted  <-   Maize_merge[order(Maize_merge$siloMaize_obs), ]
    siloMaize_obs_sorted$siloMaize_sim <- siloMaize_obs_sorted$MaizeAnomaly_sim <- siloMaize_obs_sorted$siloMaizeAnomaly_obs <-  NULL
    str(siloMaize_obs_sorted)
    
    #####################################################
    #### Cbind siloMaize_obs_sorted and siloMaize_sim_sorted ####
    Maize_scatter <- merge( siloMaize_obs_sorted, siloMaize_sim_sorted, by=c("comId", "year", "comState"))
    str(Maize_scatter)
    
    ######################
    #### Append model ####
    Maize_scatter$model <- factor(rep(namelist_RCMs[[i]], dim(  Maize_scatter )[1] ))
    
    Maize_scatter_long <- rbind(Maize_scatter_long, Maize_scatter)
    
    
    ##################################
    #### Sort yield anomaly data ####
    ################################
    
    ############################
    #### Sort siloMaize_sim ####
    siloMaize_sim_sorted_anomaly  <-   Maize_merge[order(Maize_merge$MaizeAnomaly_sim), ]
    siloMaize_sim_sorted_anomaly$siloMaize_obs <-siloMaize_sim_sorted_anomaly$siloMaizeAnomaly_obs <- siloMaize_sim_sorted_anomaly$siloMaize_sim <-  NULL
    str(  siloMaize_sim_sorted_anomaly )
    
    ##############################
    #### Sort siloMaize_obs_merge ####
    siloMaize_obs_sorted_anomaly  <-   Maize_merge[order(Maize_merge$siloMaizeAnomaly_obs), ]
    siloMaize_obs_sorted_anomaly$siloMaize_sim <- siloMaize_obs_sorted_anomaly$MaizeAnomaly_sim <- siloMaize_obs_sorted_anomaly$siloMaize_obs <-  NULL
    str(siloMaize_obs_sorted_anomaly)
    
    #####################################################
    #### Cbind siloMaize_obs_sorted and siloMaize_sim_sorted ####
    Maize_scatter_anomaly <- merge( siloMaize_obs_sorted_anomaly, siloMaize_sim_sorted_anomaly, by=c("comId", "year", "comState"))
    str(Maize_scatter)
    
    ######################
    #### Append model ####
    Maize_scatter_anomaly$model <- factor(rep(namelist_RCMs[[i]], dim(  Maize_scatter_anomaly )[1] ))
    
    Maize_scatter_long_anomaly <- rbind(Maize_scatter_long_anomaly, Maize_scatter_anomaly)
    
    
    
    }
  str( Maize_scatter_long )
  str( Maize_scatter_long_anomaly )
  
  # Maize_scatter_long_period[[n]] <- Maize_scatter_long
  # str(Maize_scatter_long_period)
  
  ##############################################
  #### Plot Scatterplot of Absolute Values ####
  ############################################
  scatterplot_RCMs <-
    ggplot( Maize_scatter_long, aes(x=siloMaize_sim, y=siloMaize_obs, color = model)) +
    scale_x_continuous(limits = c(100, 800)) + 
    scale_y_continuous(limits = c(100, 800)) + 
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
  
  ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/","Period_", period_list[[1]][n], "_to_", period_list[[2]][n], ".pdf", sep=""), 
         plot = scatterplot_RCMs, width=11, height=8)
  
  #############################################
  #### Plot Scatterplot of Anomaly Values ####
  ###########################################
  scatterplot_RCMs_anomaly <-
    ggplot( Maize_scatter_long_anomaly, aes(x=MaizeAnomaly_sim, y=siloMaizeAnomaly_obs, color = model)) +
    scale_x_continuous(limits = c(-300, 300)) +
    scale_y_continuous(limits = c(-300, 300)) +
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
  
  ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[r]],"/Scatterplot/","Period_", period_list[[1]][n], "_to_", period_list[[2]][n], "_Anomaly.pdf", sep=""), 
         plot = scatterplot_RCMs_anomaly, width=11, height=8)

  }  
}
