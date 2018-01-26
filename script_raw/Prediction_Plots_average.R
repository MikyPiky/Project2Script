########################################################################
#### Plots of Crop Yield (SM) predicted in Prediction_projection.R  ####
########################################################################

#### Description ####
'
Here I look at plots of yield of different prediction models and RCMs
- Preparation for loop 
- Loop through all 5 climate models to create Means, SDs of the predicted values (siloMaize Anomalies)
for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) 
- Loop to generate data.frame with Means and SDs of Y and Y_anomaly for the three different climate zones
- Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) 
- Export summary statistics
- Plot of yield values conditional on climate period
- Mean of Y for each climate period and reference period
- SD of Y for each climate period and reference period
- Plot of Differences in Mean and SD 

'



#### Input ####
'
- source("./script/script_raw/BaseModel.R")
- Climate_predicted_all <- read_csv(paste("./data/data_proj/output/RCM_average/Climate_predicted_all.csv", sep="")) 

'

#### Output ####
'
Descriptive Statistics:
paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_diff2021.txt", sep="")

Plots:
paste("./figures/figures_exploratory/Proj/RCM_average/plot_mean_diff_YAnomaly_", nameList_climate[[s]],".pdf", sep="")

*nameList_climate[[s]] is prediction model
*namelist_RCMs[[t]] is RCM

'

###################
## Load Packages ##
source("./script/script_raw/Packages.R")

####################################################################################################################################################################
# rm(list=ls())


##############################################################################################################################################################################
##############################################################################################################################################################################
###########################################################################################################
#### Plot Maps of predicted data (averages of climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) ####
#########################################################################################################

###############################
#### Preparation for loop ####
#############################

#############################################################
#### Load BaseModel.R with modelLIsts and nameLists used ####
source("./script/script_raw/BaseModel.R")


########################################################
#### List of start and end dates of climate periods ####
'Those are necessary for the conditioning in filter'
climateyears_list <- list(c(1971,2021,2070), c(2000, 2050, 2099))

##########################################################################
#### Create container lists for predictive models and climate models ####
########################################################################
' These containers store the plots of the yield predictions which are needed for the combined plots.'

# plot_mean_1971_average_list <-   plot_sd_1971_average_list <- plot_sd_diff2021_average_list <- plot_sd_diff2070_average_list <- 
  
  plot_mean_diff2021_average_list <- plot_mean_diff2070_average_list <- 
    nameList_climate_average_store 


######################################################################################################################
#### Create Means and SDs of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) ####
####################################################################################################################

#######################################################################
#### Load tidy data.frame of Yield_Anomaly Predictions for RCM t  ####
#####################################################################
Climate_predicted_all <- read_csv("./data/data_proj/output/Climate_predicted_allRCMs.csv")
Climate_predicted_all$RCM <- rep("RCM_average", dim(Climate_predicted_all ) [1])
# summary(Climate_predicted_all)

## Check predicted anomaly values ##
Climate_predicted_all %>% select(contains("sMA")) %>% summary()

#########################################################################
#### Generate list with data.frame container for each climate period ####
Climate_predicted_all_summaries_list <- list(Climate_predicted_all_summaries_1979 = data.frame(), Climate_predicted_all_summaries_2021= data.frame(),
                                             Climate_predicted_all_summaries_2070 = data.frame())

######################################
#### List to store sf.data.frames ####
Climate_predicted_all_summaries_sf_list <- list(Climate_predicted_all_summaries_sf_1979 = data.frame(), Climate_predicted_all_summaries_sf_2021= data.frame(),
                                                Climate_predicted_all_summaries_sf_2070 = data.frame())


#######################################################################################################################
#### Loop to generate data.frame with Means and SDs, Min, Max for Y_anomaly for the three different climate zones ####
#####################################################################################################################
for (r in 1:3){
  Climate_predicted_all_summaries_list[[r]]  <- 
    Climate_predicted_all  %>% 
    # filter(model == namelist_RCMs[[t]] )    %>%
    filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) %>% 
    group_by(comId)     %>%      
    summarise_at(vars(contains("sMA_")), funs(mean, sd), na.rm=F )
  
  
  #### Merge with Spatial Information ####
  Climate_predicted_all_summaries_sf_list[[r]] <- inner_join(vg2500_krs, Climate_predicted_all_summaries_list[[r]], by = "comId") 
  
}

###########################################################
#### Compare Mean values for the three climate periods ####
Climate_predicted_all_summaries_list[[1]] %>% select(contains("_mean")) %>% summary()
Climate_predicted_all_summaries_list[[2]] %>% select(contains("_mean")) %>% summary()
Climate_predicted_all_summaries_list[[3]] %>% select(contains("_mean")) %>% summary()

###########################################################
#### Compare Mean values for the three climate periods ####
Climate_predicted_all_summaries_list[[1]] %>% select(contains("_sd")) %>% summary()
Climate_predicted_all_summaries_list[[2]] %>% select(contains("_sd")) %>% summary()
Climate_predicted_all_summaries_list[[3]] %>% select(contains("_sd")) %>% summary()

##################################################################################################################################
#### Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) ####
################################################################################################################################
Climate_predicted_all_summaries_diff2021 <- as_tibble(Climate_predicted_all_summaries_list[[2]] - Climate_predicted_all_summaries_list[[1]])
Climate_predicted_all_summaries_diff2070 <- as_tibble(Climate_predicted_all_summaries_list[[3]] - Climate_predicted_all_summaries_list[[1]])

#### Add comIds to allow for merging ####
Climate_predicted_all_summaries_diff2021$comId <- Climate_predicted_all_summaries_list[[2]]$comId
Climate_predicted_all_summaries_diff2070$comId <- Climate_predicted_all_summaries_list[[2]]$comId

#### Merge difference data with vg2500_krs to get Spatial Attributes ####
Climate_predicted_all_summaries_diff2021_sf <- inner_join(vg2500_krs, Climate_predicted_all_summaries_diff2021 , by = "comId", sort=T) 
Climate_predicted_all_summaries_diff2070_sf <- inner_join(vg2500_krs, Climate_predicted_all_summaries_diff2070 , by = "comId", sort=T) 


' The diff. data for Y and Y_anomaly are the same. By differencing the comId specific mean disappears by the means of the substraction, since 
  Y_anomaly =  Y - mean(Y of comId between 1999 and 2015), where Y is time dependent but mean(Y of comId between 1999 and 2015 is a constant) '

####################################
#### Export summary statistics ####
##################################
stargazer(as.data.frame(Climate_predicted_all_summaries_diff2021), type = "text", title="Descriptive statistics", digits=3,
          out = paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_diff2021_mean_sd.txt", sep=""))
stargazer(as.data.frame(Climate_predicted_all_summaries_diff2070), type = "text", title="Descriptive statistics", digits=3,
          out = paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_diff2070_mean_sd.txt", sep=""))
stargazer(as.data.frame(Climate_predicted_all_summaries_list[[1]]), type = "text", title="Descriptive statistics", digits=3,
          out = paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_1971_mean_sd.txt", sep=""))
stargazer(as.data.frame(Climate_predicted_all_summaries_list[[2]]), type = "text", title="Descriptive statistics", digits=3,
          out = paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_2021_mean_sd.txt", sep=""))
stargazer(as.data.frame(Climate_predicted_all_summaries_list[[3]]), type = "text", title="Descriptive statistics", digits=3,
          out = paste("./figures/figures_exploratory/Proj/RCM_average/DeskriptiveStats_2070_mean_sd.txt", sep=""))

##############################################################################################################################################################################
#########################################################################
#### Plot of Anomaly Values for the three different climate periods ####
#######################################################################
##############################################################################################################################################################################

#################################################################
#### Create namelist to loop to siloMaizeAnomaly predictions ####
nameList_climate_sMA_mean <- paste("sMA_", nameList_climate, "_mean", sep="")
nameList_climate_sMA_sd <- paste("sMA_", nameList_climate, "_sd", sep="")

nameList_climate_yield <- paste("Yield - Anomaly: ", nameList_climate, sep="")

##################################################
### Start of loop trough predictive models s ####
################################################
seq_along(nameList_climate)
for (s in 1:1){
  
  # #########################################################
  # #### Plot statistics of sMA for each climate period ####
  # #######################################################
  # Climate_predicted_all_summaries_list[[1]] %>% select(contains("_mean")) %>% summary()
  # Climate_predicted_all_summaries_list[[2]] %>% select(contains("_mean")) %>% summary()
  # Climate_predicted_all_summaries_list[[3]] %>% select(contains("_mean")) %>% summary()
  # 
  # myPalette <- colorRampPalette((brewer.pal(11, "RdYlGn")))
  # sc <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(-50, 50))
  # 
  # ## Plot Mean Y: 1971 - 2000 ##
  # plot_mean_1971_Y <-
  #   ggplot(Climate_predicted_all_summaries_sf_list [[1]]) +
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_mean[[s]] )) +
  #   # ggtitle(paste( nameList_climate[[s]],", Mean: 1971 - 2000", sep="")) + 
  #   ggtitle(paste("Mean: 1971 - 2000", sep="")) +
  #   # sc + 
  #   theme_bw() +       theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_mean_1971_Y
  # 
  # ## Store to allow combined plots
  # plot_mean_1971_list[[s]][[t]] <-  plot_mean_1971_Y
  # 
  # ## Plot Mean Y: 2021 - 2050  ##
  # plot_mean_2021_Y <-
  #   ggplot(Climate_predicted_all_summaries_sf_list [[2]]) +
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_mean[[s]] )) +
  #   # ggtitle(paste( nameList_climate[[s]],", Mean: 2021 - 2050", sep="")) + 
  #   ggtitle(paste("Mean: 2021 - 2050", sep="")) +
  #   sc +
  #   theme_bw() +       theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_mean_2021_Y
  # 
  # ## Plot Mean: 2070 - 2099  ##
  # plot_mean_2070_Y <-
  #   ggplot(Climate_predicted_all_summaries_sf_list [[3]]) +
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_mean[[s]] )) +
  #   # ggtitle(paste( nameList_climate[[s]],", Mean: 2071 - 2099", sep="")) + 
  #   ggtitle(paste("Mean: 2070 - 2099", sep="")) + sc +
  #   theme_bw() +       theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_mean_2070_Y
  # 
  # plot_mean_Y <- grid.arrange(plot_mean_1971_Y, plot_mean_2021_Y, plot_mean_2070_Y, ncol=3, 
  #                             top=textGrob(paste(namelist_RCMs[[t]], nameList_climate[[s]], sep=" & " ), gp=gpar(fontsize=30)))
  # 
  # ggsave(paste("./figures/figures_exploratory/Proj/RCM_average/plot_mean_YAnonmaly_", nameList_climate[[s]],".pdf", sep=""), 
  #        plot=plot_mean_Y, width=21, height=8)
  # 
  # 
  # 
  # ###############################################
  # #### Plot SD of Y for each climate period ####
  # #############################################
  # 
  # ## Define colorRamp ##
  # str(Climate_predicted_all_summaries_sf_list,1)
  # summary(Climate_predicted_all_summaries_sf_list [[1]]$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_sd)
  # summary(Climate_predicted_all_summaries_sf_list [[2]]$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_sd)
  # summary(Climate_predicted_all_summaries_sf_list [[3]]$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_sd)
  # 
  # 
  # myPalette <- colorRampPalette((brewer.pal(9, "Purples")))
  # sc <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(0, 100))
  # ' Here I take the mean of the period 1971 - 2000 as reference for colouring'
  # 
  # 
  # ## Plot SD Y: 1971 - 2000 ##
  # plot_sd_1971_Y <- 
  #   ggplot(Climate_predicted_all_summaries_sf_list [[1]]) + 
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_sd[[s]] )) +
  #   # ggtitle(paste( nameList_climate_yield[[s]],", SD: 1971 - 2000", sep="")) + 
  #   ggtitle(paste("SD: 1971 - 2000")) + 
  #   
  #   sc + 
  #   theme_bw() +       
  #   theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_sd_1971_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_1971_list[[s]][[t]] <-  plot_sd_1971_Y
  # 
  # ## Plot SD Y: 2021 - 2050  ##
  # plot_sd_2021_Y <- 
  #   ggplot(Climate_predicted_all_summaries_sf_list [[2]]) + 
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_sd[[s]] )) +
  #   # ggtitle(paste( nameList_climate_yield[[s]],", SD: 2021 - 2050", sep="")) + 
  #   ggtitle("SD: 2021 - 2050") + 
  #   sc + 
  #   theme_bw() +       
  #   theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_sd_2021_Y
  # 
  # ## Plot SD: 2070 - 2099  ##
  # plot_sd_2070_Y <- 
  #   ggplot(Climate_predicted_all_summaries_sf_list [[3]]) + 
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_sd[[s]] )) +
  #   # ggtitle(paste( nameList_climate_yield[[s]],", SD: 2070 - 2099", sep="")) + 
  #   ggtitle("SD: 2070 - 2099") + 
  #   sc + 
  #   theme_bw() +       
  #   theme(plot.title = element_text(hjust = 0.5))
  # 
  # # plot_sd_2070_Y
  # 
  # plot_sd_Y <- grid.arrange(plot_sd_1971_Y, plot_sd_2021_Y, plot_sd_2070_Y, ncol=3, 
  #                           top=textGrob(paste(namelist_RCMs[[t]], nameList_climate[[s]], sep=" & " ),gp=gpar(fontsize=30))) 
  # 
  # ggsave(paste("./figures/figures_exploratory/Proj/RCM_average/plot_sd_YAnonmaly_", nameList_climate[[s]],".pdf", sep=""), 
  #        plot=plot_sd_Y, width=21, height=8)
  # 
  ##############################################################################################################################################################################
  #### Plot Differences of climate periods compared to reference period ####
  ##############################################################################################################################################################################
  
  ###########################################################
  #### Plot difference in mean of climate periods of YD #####
  ###########################################################
  Climate_predicted_all_summaries_list[[1]] %>% select(contains("_mean")) %>% summary()
  Climate_predicted_all_summaries_list[[2]] %>% select(contains("_mean")) %>% summary()
  Climate_predicted_all_summaries_list[[3]] %>% select(contains("_mean")) %>% summary()

  
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn("dt/ha", colours = myPalette(100), limits=c(-70, 70))
  
  #### Plot Difference of Mean (2070-2099) - (1971-2000) ####
  plot_mean_diff2070_Y <-
    ggplot(Climate_predicted_all_summaries_diff2070_sf) +
    geom_sf(data=vg2500_krs, fill="gray", color="white")  +
    geom_sf(aes_string(fill = nameList_climate_sMA_mean[[s]] )) +
    # ggtitle(paste( nameList_climate_yield[[s]],", Mean (2070-2099) - (1971-2000) ", sep="")) +
    sc +
    ggtitle(paste( "Average of all RCMs, 2070 - 2099" , sep="")) +
    theme_bw() +   
    theme(legend.position="right") +
    # theme(legend.title = element_blank()) +
    theme(title = element_text(color="black") ) +
    theme(plot.title = element_text(hjust = 0.5))

  
  # plot_mean_diff2070_Y
  
  ## Store to allow combined plots
  plot_mean_diff2070_average_list[[s]] <-  plot_mean_diff2070_Y

  
  
  ###########################################################
  #### Plot Difference of Mean (2021-2050) - (1971-2000) ####
  plot_mean_diff2021_Y <-
    ggplot(Climate_predicted_all_summaries_diff2021_sf) +
    geom_sf(data=vg2500_krs, fill="gray", color="white")  +
    geom_sf(aes_string(fill = nameList_climate_sMA_mean[[s]] )) +
    # ggtitle(paste( nameList_climate_yield[[s]],", Mean (2021-2050) - (1971-2000)", sep=""))  + 
    sc +
    ggtitle(paste( "Average of all RCMs, 2021 - 2050" , sep="")) +
    theme_bw() +   
    theme(legend.position="right") +
    theme(title = element_text(color="black") ) +
    theme(plot.title = element_text(hjust = 0.5))
  

  # plot_mean_diff2021_Y
  
  
  ## Store to allow combined plots
  plot_mean_diff2021_average_list[[s]] <-   plot_mean_diff2021_Y

  plot_mean_diff_Y <- grid.arrange(plot_mean_diff2021_Y, plot_mean_diff2070_Y, ncol=2, 
                                   top = textGrob(paste(namelist_RCMs[[t]], nameList_climate[[s]], sep=" & " ), gp=gpar(fontsize=30)))
  # plot_mean_diff_Y
  
  ggsave(paste("./figures/figures_exploratory/Proj/RCM_average/plot_mean_diff_YAnomaly_", nameList_climate[[s]],".pdf", sep=""), 
         plot = plot_mean_diff_Y, width=14, height=8)
  
  # ###########################################################
  # #### Plot difference in sd of climate periods of YD #####
  # ##########################################################
  # 
  # #### Define colorRamp for Y_sd ####
  # summary(Climate_predicted_all_summaries_diff2070_sf$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_modelmatrix_mean)
  # summary(Climate_predicted_all_summaries_diff2021_sf$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_modelmatrix_mean)
  # 
  # myPalette <- colorRampPalette((brewer.pal(11, "PiYG")))
  # sc <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(-100, 100))
  # 'Here I take zero a reference'
  # 
  # #### Plot Difference of SD (2070-2099) - (1971-2000) ####
  # plot_sd_diff2070_Y <- 
  #   ggplot(Climate_predicted_all_summaries_diff2070_sf) + 
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_sd[[s]] )) +
  #   # ggtitle(paste( nameList_climate_yield[[s]],", SD: (2021-2050) - (1971-2000)", sep=""))  + 
  #   ggtitle(paste("SD: (2070-2099) - (1971-2000)", sep="")) +
  #   sc +
  #   theme_bw() +       
  #   theme(plot.title = element_text(hjust = 0.5))
  # # plot_sd_diff2070_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_diff2070_average_list[[s]][[t]] <-  plot_sd_diff2070_Y
  # 
  # #### Plot Difference of SD (2021-2050) - (1971-2000) ####
  # plot_sd_diff2021_Y <- 
  #   ggplot(Climate_predicted_all_summaries_diff2021_sf) + 
  #   geom_sf(data=vg2500_krs, fill="gray", color="white")  +
  #   geom_sf(aes_string(fill = nameList_climate_sMA_sd[[s]] )) +
  #   # ggtitle(paste( nameList_climate_yield[[s]],", SD: (2021-2050) - (1971-2000)", sep=""))  + 
  #   ggtitle(paste("SD: (2070-2099) - (1971-2000)", sep="")) +
  #   sc +
  #   theme_bw() +       
  #   theme(plot.title = element_text(hjust = 0.5))
  # # plot_sd_diff2021_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_diff2021_average_list[[s]][[t]] <-   plot_sd_diff2021_Y
  # 
  # plot_sd_diff_Y <- grid.arrange(plot_sd_diff2021_Y, plot_sd_diff2070_Y, ncol=2, 
  #                                top = textGrob(paste(namelist_RCMs[[t]], nameList_climate[[s]], sep=" & " ), gp=gpar(fontsize=30)))
  # # plot_sd_diff_Y
  # 
  # ggsave(paste("./figures/figures_exploratory/Proj/RCM_average/plot_sd_diff_YAnomaly_", nameList_climate[[s]],".pdf", sep=""), 
  #        plot = plot_sd_diff_Y, width=14, height=8)
} # End of loop through predictive models

plot_mean_diff2021_average_list[[1]]
plot_mean_diff2070_average_list[[1]]
s

