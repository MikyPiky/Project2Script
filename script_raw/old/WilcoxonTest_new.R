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

###########################################################################
#### Loop through different models on which the predictions are based ####
#########################################################################

#### Start of loop through prediction models #####
# for (s in 1:length(modelListMatrixNames)){
  
  #### Create directory for output of this loop ####
  dir.create(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]] ,sep=""), showWarnings = FALSE)
  
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy_Anomaly.csv", sep="") )
  PredictData_df_tidy$X <- NULL
  str(PredictData_df_tidy) 
  
  #################################################################################################
  #### Loop to create one data.frame for each climate period of 30 yeears,                    ####
  #### i.e the reference period 1971 - 2000 and the climate periods 2021-2050 and 2070 - 2099 ###
  ##############################################################################################

  #### Generate list with data.frame container for each climate period: ####
  PredictData_df_tidy_test_list <- list(PredictData_df_tidy_test_1971 = data.frame(), PredictData_df_tidy_test_2021= data.frame(),
                                        PredictData_df_tidy_test_2070 = data.frame())
 
  #### Start of loop through three time periods ####
  for (r in 1:3){
    PredictData_df_tidy_test_list[[r]]  <- 
      PredictData_df_tidy  %>% 
      filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) 
  }
  
  str(PredictData_df_tidy_test_list[[1]])
  str(PredictData_df_tidy_test_list[[2]])
  str(PredictData_df_tidy_test_list[[3]])
  
  #### Rename y and y_anomaly of each climate period accordingly ####
  names(PredictData_df_tidy_test_list[[1]]) <- c("model", "comId", "year",  "Y_1971", "Y_anomaly_1971")
  names(PredictData_df_tidy_test_list[[2]]) <- c("model", "comId", "year",  "Y_2021", "Y_anomaly_2021")
  names(PredictData_df_tidy_test_list[[3]]) <- c("model", "comId", "year",  "Y_2070", "Y_anomaly_2070")
  
  #### Make of large data.frane of the data of the three climate periods used in Wilcoxon Test ####
  test_data <- cbind(PredictData_df_tidy_test_list[[1]], cbind(PredictData_df_tidy_test_list[[2]][,4:5], PredictData_df_tidy_test_list[[3]][,4:5]))
  str( test_data)
  # test_data$year <- NULL
  
  # #### Compare columns by WilcoxonText #####
  # (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))
  # (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))$p.value
  
  ###########################################################################################
  #### Loop though five climate models to provide maps of p-values of the Wilcoxon Test ####
  #########################################################################################
  
  ##############################
  #### Preparation for loop ####
  
  #### Create Container to store p-values of the test results in ####
  test_data_grouped_2070_anomaly_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2021_anomaly_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2070_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2021_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  
  #### Lists Names used in figures ####
  modelListYieldNames <-list("Model: combined", "Model: July")
  
  #### Start of loop trough the five RCMs ####
  for (l in 1:5){
  
  ## Compare Anomalies of 1971 -2000  to 2070 - 2099 ## 
  test_data_grouped_2070_anomaly_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
      summarise(test         = wilcox.test(test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070)$p.value, 
                test_paired  = wilcox.test(test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070, paired=T)$p.value )
  
  ## Compare Anomalies of 1971 - 2000 to 2021 - 2050 ##
  test_data_grouped_2021_anomaly_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test        = wilcox.test(test_data$Y_anomaly_1971,  test_data$Y_anomaly_2021)$p.value,
              test_paired = wilcox.test(test_data$Y_anomaly_1971,  test_data$Y_anomaly_2021, paired=T)$p.value)
  
  ## Compare Absolute Values of 1971 -2000  to 2070 - 2099 ##
  test_data_grouped_2070_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test        = wilcox.test( test_data$Y_1971,  test_data$Y_2070)$p.value,
              test_paired = wilcox.test( test_data$Y_1971,  test_data$Y_2070, paired=T)$p.value)
  
  ## Compare Absolute Values of 1971 - 2000 to 2021 - 2050 ##
  test_data_grouped_2021_list[[l]] <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test = wilcox.test(Y_1971,  Y_2021)$p.value,
              test_paired = wilcox.test(Y_1971,  Y_2021, paired=T)$p.value)
  


  #############################
  #### Add on Spatial Data ####
  test_data_grouped_2021_anomaly_spatial <- merge(vg2500_krs, test_data_grouped_2021_anomaly_list[[l]], by.x="RS", by.y="comId")
  test_data_grouped_2070_anomaly_spatial <- merge(vg2500_krs, test_data_grouped_2070_anomaly_list[[l]], by.x="RS", by.y="comId")
  test_data_grouped_2021_spatial <- merge(vg2500_krs, test_data_grouped_2021_list[[l]] , by.x="RS", by.y="comId")
  test_data_grouped_2070_spatial <- merge(vg2500_krs, test_data_grouped_2070_list[[l]], by.x="RS", by.y="comId")

  #################################
  #### Take a look at p-values ####
  # View(test_data_grouped_2070_anomaly_spatial)
  # View(test_data_grouped_2021_anomaly_spatial)
  # View(test_data_grouped_2021_spatial)
  # View(test_data_grouped_2070_spatial)
  # 
  ##############  
  #### Maps ####
  
  #### non paired ####
  test_data_grouped_2021_anomaly_spatial_plot <-
  ggplot(test_data_grouped_2021_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2021 - Anomalies - non paired")
  
  test_data_grouped_2070_anomaly_spatial_plot <-
  ggplot(test_data_grouped_2070_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test, c(-0.1, 0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2070 - Anomalies - non paired")
  
  test_data_grouped_2021_spatial_plot <-
  ggplot(test_data_grouped_2021_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type="seq",palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2021 - non paired")
  
  test_data_grouped_2070_spatial_plot <- 
  ggplot(test_data_grouped_2070_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) + 
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2070 - non paired")
  
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_anomaly_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_anomaly_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
 
  #### paired ####
  test_data_grouped_2021_anomaly_spatial_plot <-
    ggplot(test_data_grouped_2021_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2021 - Anomalies - paired")
  
  test_data_grouped_2070_anomaly_spatial_plot <-
    ggplot(test_data_grouped_2070_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2070 - Anomalies - paired")
  
  test_data_grouped_2021_spatial_plot <-
    ggplot(test_data_grouped_2021_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type="seq",palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2021- paired")
  
  test_data_grouped_2070_spatial_plot <- 
    ggplot(test_data_grouped_2070_spatial) + 
    geom_sf(data = vg2500_krs, colour="white", fill="black") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values - \nH0: no shift in mean")) +
    ggtitle("2070 - paired")
  
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_anomaly_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_anomaly_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
  
  
  }
}
   
rm(list=ls())

