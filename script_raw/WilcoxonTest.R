#### Test for significant changes in the predicted yield values for the climate period ####
'
- Load the tidy data
- Filter for climate models
- Filter for the reference and climate periods
- Make on data.frame including the observations for all three climate periods
- Group by comIds and apply test to each cohort
- Combine with spatial information

'
#### Input ####
'
Spatial Information: Shapefile of comdIDs ("vg2500_krs")
BasePrediction.R: tidy.data.frames of yield and yield anomaly predictions based on different estimation models

'

###################
## Load Packages ##

####################################################################################################################################################################



##############################
#### Preparation for loop ####
source("./script/script_raw/BaseModel.R")

#### Create Container to store plots in according to their predictive model #####
for (s in seq_along(nameList_climate)){
dir.create(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]] ,sep=""), showWarnings = FALSE)
}

#### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
PredictData_df_tidy <- read_csv(paste("./data/data_proj/output/Climate_predicted_allRCMs.csv", sep="") )
PredictData_df_tidy

#################################################################################################
#### Loop to create one data.frame for each climate period of 30 yeears,                    ####
#### i.e the reference period 1971 - 2000 and the climate periods 2021-2050 and 2070 - 2099 ###
##############################################################################################

## Select variables needed for test on Yield Anomalies ##
PredictData_df_tidy <-  PredictData_df_tidy %>% select("RCM", "comId", "year", contains("sMA"))

#### Generate list with data.frame container for each climate period: ####
PredictData_df_tidy_test_list <- list(PredictData_df_tidy_test_1971 = data.frame(), PredictData_df_tidy_test_2021= data.frame(),
                                      PredictData_df_tidy_test_2070 = data.frame())

#### Start of loop through three time periods ####
for (r in 1:3){
  PredictData_df_tidy_test_list[[r]]  <- 
    PredictData_df_tidy  %>% 
    filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) 
}

str(PredictData_df_tidy_test_list[[1]], 1)
str(PredictData_df_tidy_test_list[[2]], 1)
str(PredictData_df_tidy_test_list[[3]], 1)

#### Rename y and y_anomaly of each climate period accordingly ####
names(PredictData_df_tidy_test_list[[1]])[4:7] <- paste(names(PredictData_df_tidy_test_list[[1]])[4:7] , "1971", sep="_" )
names(PredictData_df_tidy_test_list[[2]])[4:7] <- paste(names(PredictData_df_tidy_test_list[[2]])[4:7] , "2021", sep="_" )
names(PredictData_df_tidy_test_list[[3]])[4:7] <- paste(names(PredictData_df_tidy_test_list[[3]])[4:7] , "2070", sep="_" )

#### Make of large data.frane of the data of the three climate periods used in Wilcoxon Test ####
test_data <- bind_cols(PredictData_df_tidy_test_list[[1]], bind_cols(PredictData_df_tidy_test_list[[2]][,4:7], PredictData_df_tidy_test_list[[3]][,4:7]))
str( test_data,1)
# test_data$year <- NULL

# #### Compare columns by WilcoxonText #####
# (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))
# (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))$p.value

###########################################################################################
#### Loop though five climate models to provide maps of p-values of the Wilcoxon Test ####
#########################################################################################

##############################
#### Preparation for loop ####

#### Create Container to store p-values and plots of the test results in ####
test_data_grouped_2021_anomaly_list <- 
  test_data_grouped_2070_anomaly_list <- 
  test_data_grouped_2021_anomaly_plots_list <- 
  test_data_grouped_2070_anomaly_plots_list <- 
  test_data_grouped_2021_anomaly_plots_paired_list <- 
  test_data_grouped_2070_anomaly_plots_paired_list <- 
  test_data_grouped_2021_anomaly_list_noTitle <- 
  test_data_grouped_2070_anomaly_list_noTitle <- 
  test_data_grouped_2021_anomaly_plots_list_noTitle <- 
  test_data_grouped_2070_anomaly_plots_list_noTitle <- 
  test_data_grouped_2021_anomaly_plots_paired_list_noTitle <- 
  test_data_grouped_2070_anomaly_plots_paired_list_noTitle <- 
  test_data_grouped_2021_anomaly_list_noTitle_noLegend <- 
  test_data_grouped_2070_anomaly_list_noTitle_noLegend <- 
  test_data_grouped_2021_anomaly_plots_list_noTitle_noLegend <- 
  test_data_grouped_2070_anomaly_plots_list_noTitle_noLegend <- 
  test_data_grouped_2021_anomaly_plots_paired_list_noTitle_noLegend <- 
  test_data_grouped_2070_anomaly_plots_paired_list_noTitle_noLegend <- 
  list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())

#### Lists Names used in figures ####
nameList_climate

s=1
s

#######################################
#### Define Function used in Loop ####
#####################################
## time Period ##
timePeriod <- list("2021 - 2050", "2070 - 2099")

## Paired ##
testPaired <- list( "non paired Wilcoxon - Test", "paired Wilcoxon - Test")

## Legend List ##
list_legend_Variables <- c("none", "right")
list_legend_export <- c("noLegend", "legend")

##  title ##
list_titleVariables <- list(element_text(color="white") , element_text(color="black") )
list_title_export <- list("noTitle", "title")
nameList_climate

plot_variables = function (dataSet, timeP, paired,  Var, Tit, Leg){
  ggplot(dataSet) +
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(dataSet[[5 + Var]], c(-0.1,0.05,0.1,1), m=0) )) + 
    ggtitle(paste(timePeriod[[timeP]],  ": " ,namelist_RCMs_total[[RCMs]], " - ", testPaired[[paired]], sep=""))  +
    # ggtitle("2021 - Anomalies - non paired") 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) + 
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean"))  +
    theme_bw()  +
    theme(legend.position = list_legend_Variables[Leg]) +
    # theme(legend.title=element_blank()) +
    theme(plot.title =list_titleVariables [[Tit]] )
  
}



#### Start of loop trough the five RCMs ####
for (l in seq_along( namelist_RCMs)){
  
  #### Create directory for output of this loop ####

  # 
  # cluster <- create_cluster(4)
  # set_default_cluster(cluster)
  # 
  # by_group <- test_data %>% filter(RCM == namelist_RCMs[[l]])   %>% partition(comId, cluster = cluster)
  # # cluster_get(by_group, "test_data")
  # 
  ## Compare Anomalies of 1971 -2000  to 2070 - 2099 ## 
  test_data_grouped_2070_anomaly_list[[l]]  <- 
    test_data %>% 
    filter(RCM == namelist_RCMs[[l]])   %>%
    group_by(comId) %>%
    summarise(test_sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean                  = wilcox.test(sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2070)$p.value,
              test_sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_paired           = wilcox.test(sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2070, paired=T)$p.value, 
              test_sMA_lm.fit_SMI_6_Jul_anomaly_demean                      = wilcox.test(sMA_lm.fit_SMI_6_Jul_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jul_anomaly_demean_2070)$p.value, 
              test_sMA_lm.fit_SMI_6_Jul_anomaly_demean_paired               = wilcox.test(sMA_lm.fit_SMI_6_Jul_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2070, paired=T)$p.value, 
              test_sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean        = wilcox.test(sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_1971,  
                                                                                 sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_2070)$p.value, 
              test_sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_paired = wilcox.test(sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_1971,  
                                                                                 sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_2070, paired=T)$p.value, 
              test_sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean                    = wilcox.test(sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_2070)$p.value, 
              test_sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_paired             = wilcox.test(sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_2070, paired=T)$p.value) %>% collect()
  
  
  ## Compare Anomalies of 1971 - 2000 to 2021 - 2050 ##
  test_data_grouped_2021_anomaly_list[[l]]  <- 
    test_data %>% 
    filter(RCM == namelist_RCMs[[l]])   %>%
    group_by(comId) %>%
    summarise(test_sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean                  = wilcox.test(sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2021)$p.value, 
              test_sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_paired           = wilcox.test(sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2021, paired=T)$p.value, 
              test_sMA_lm.fit_SMI_6_Jul_anomaly_demean                      = wilcox.test(sMA_lm.fit_SMI_6_Jul_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jul_anomaly_demean_2021)$p.value, 
              test_sMA_lm.fit_SMI_6_Jul_anomaly_demean_paired               = wilcox.test(sMA_lm.fit_SMI_6_Jul_anomaly_demean_1971,  
                                                                                 sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_2021, paired=T)$p.value, 
              test_sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean        = wilcox.test(sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_1971,  
                                                                                 sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_2021)$p.value, 
              test_sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_paired = wilcox.test(sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_1971,  
                                                                                 sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_2021, paired=T)$p.value, 
              test_sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean                   = wilcox.test(sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_2021)$p.value, 
              test_sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_paired            = wilcox.test(sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_1971,  
                                                                                 sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_2021, paired=T)$p.value
              
    )
  
 
  
  #############################
  #### Add on Spatial Data ####
  test_data_grouped_2021_anomaly_spatial <- inner_join(vg2500_krs, test_data_grouped_2021_anomaly_list[[l]], by = "comId")
  test_data_grouped_2070_anomaly_spatial <- inner_join(vg2500_krs, test_data_grouped_2070_anomaly_list[[l]], by = "comId")

  #################################
  #### Take a look at p-values ####
  # View(test_data_grouped_2070_anomaly_spatial)
  # View(test_data_grouped_2021_anomaly_spatial)
  # View(test_data_grouped_2021_spatial)
  # View(test_data_grouped_2070_spatial)
  # 
  ##############  
  #### Maps ####
  

  

  print(names(test_data_grouped_2021_anomaly_spatial[5 + Var]))

  #### non paired ####
  Var <- 1
  paired <- 1
  Tit <- 2
  Leg <- 2
  
  timeP <- 1
  test_data_grouped_2021_anomaly_plots_list[[l]] <- plot_variables(test_data_grouped_2021_anomaly_spatial, timeP, paired,  Var, Tit, Leg)
  
  timeP <- 2
  test_data_grouped_2070_anomaly_plots_list[[l]] <- plot_variables(test_data_grouped_2070_anomaly_spatial, timeP, paired,  Var, Tit, Leg)

  
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2070_anomaly_",namelist_RCMs[[l]],".pdf", sep="") ,  
         test_data_grouped_2070_anomaly_plots_list[[l]] , width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2021_anomaly_",namelist_RCMs[[l]],".pdf", sep="") , 
         test_data_grouped_2021_anomaly_plots_list[[l]] , width=16, height=9) 
  # ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2070_",namelist_RCMs[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  # ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2021_",namelist_RCMs[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
  # 
  
  
  #### paired ####
  Var <- 2
  timeP <- 2
  Tit <- 2
  Leg <- 2
  
  timeP <- 1
  test_data_grouped_2021_anomaly_plots_paired_list[[l]] <- plot_variables(test_data_grouped_2021_anomaly_spatial, timeP, paired,  Var, Tit, Leg)
 
  timeP <- 2
  test_data_grouped_2070_anomaly_plots_paired_list[[l]]  <-  plot_variables(test_data_grouped_2070_anomaly_spatial, timeP, paired,  Var, Tit, Leg)

  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2070_anomaly_paired_",namelist_RCMs[[l]],".pdf", sep="") ,  
         test_data_grouped_2070_anomaly_plots_paired_list[[l]], width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2021_anomaly_paired_",namelist_RCMs[[l]],".pdf", sep="") ,  
         test_data_grouped_2021_anomaly_plots_paired_list[[l]], width=16, height=9) 
  # ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2070_paired_",namelist_RCMs[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  # ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", nameList_climate[[s]],"/Wilcoxon_2021_paired_",namelist_RCMs[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
  # 
  
}
# }
   
# rm(list=ls())

DMI_annotated <-  annotate_figure(  ggarrange(test_data_grouped_2021_anomaly_plots_paired_list[[1]], test_data_grouped_2070_anomaly_plots_paired_list[[1]], labels = c("a1)", "a2)"),
                                              ncol=1, nrow=2) , top = text_grob("DMI", color = "black", face = "bold", size = 20, family= " Arial"))
ICTP_annotated <-  annotate_figure(  ggarrange(test_data_grouped_2021_anomaly_plots_paired_list[[2]], test_data_grouped_2070_anomaly_plots_paired_list[[2]], labels = c("b1)", "b2)"),
                                              ncol=1, nrow=2) , top = text_grob("ICTP", color = "black", face = "bold", size = 20, family= " Arial"))
KNMI_annotated <-  annotate_figure(  ggarrange(test_data_grouped_2021_anomaly_plots_paired_list[[3]], test_data_grouped_2070_anomaly_plots_paired_list[[3]], labels = c("c1)", "c2)"),
                                              ncol=1, nrow=2) , top = text_grob("KNMI", color = "black", face = "bold", size = 20, family= " Arial"))
MPI_annotated <-  annotate_figure(  ggarrange(test_data_grouped_2021_anomaly_plots_paired_list[[4]], test_data_grouped_2070_anomaly_plots_paired_list[[4]], labels = c("d1)", "d2)"),
                                              ncol=1, nrow=2) , top = text_grob("MPI", color = "black", face = "bold", size = 20, family= " Arial"))
SMHI_annotated <-  annotate_figure(  ggarrange(test_data_grouped_2021_anomaly_plots_paired_list[[5]], test_data_grouped_2070_anomaly_plots_paired_list[[5]], labels = c("e1)", "e2)"),
                                              ncol=1, nrow=2) , top = text_grob("SMHI", color = "black", face = "bold", size = 20, family= " Arial"))


test_data_grouped_2021_anomaly_paired_list_allPlots <- 
  ggarrange( DMI_annotated, ICTP_annotated, KNMI_annotated, MPI_annotated, SMHI_annotated ,  ncol=5, nrow = 1, 
             common.legend = TRUE, legend = "right", align ="v")
