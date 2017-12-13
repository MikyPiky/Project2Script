#### Boxplots ####
' Script to make boxplots or similiar graphics which represent the distribution of the difference 
in predictions for the two climate periode (20)  compared to the reference period.
  1) boxplot of the difference in the means, that means the distribution is 
     defined by the average of each spatial unit. Since only means are considered, it is not 
     possible to judge about the distribution of extreme deviations. 
  2) Boxplot of the difference of the levels of the climate periods and the mean of the reference period
  3) Boxplot of the difference of the levels of the climate periods and the levels of the reference period

  Caveat: yield and yield anomalies have the same results, because 
  Y_SubY_mean_ref_anomaly = Y_anomaly - Y_anomaly_mean_ref = Y - mean(Y_19992015|comId) - mean(Y_ref - mean(Y_19992015|comId)|comId) =
                                                             Y -  mean(Y_ref|comId) 

'
#### Input ####
'
- source("./script/script_raw/BaseModel.R")
- PredictData_df_tidy[[s]] <- read_csv(paste("./data/data_proj/output/",  namelist_RCMs[[s]],"/Climate_predicted.csv", sep="") )

'

#### Output ####
'
- ./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]],"/ViolinPlot_*.pdf,  * = various boxplots
'

#######################
#### Load Packages ####
# source("./script/script_raw/Packages.R")

##############################################################################################################################################
##############################################################################################################################################
# rm(list=ls())
# source("./script/script_raw/BaseModel.R")

################################
#### Preparation for loops ####
##############################

##########################################
#### Generate lists used in the loop #####

#### start and end dates of climate periods ####
climateyears_list <- list(c(1971, 2021, 2070), c(2000, 2050, 2099))
climateyears_dummy_list <- list("1971 - 2000", "2021-2050", "2070-2099")

#### List of Names used to store the figures ####
## List of predictive models
modelList_climate
nameList_climate 

#### List to store the loaded data ###
PredictData_df_tidy <- list()

#### Define predictive models start and end ####
predictive_models <- list("sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean", "sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean")


#### Indeces ####
' s loops through RCMs
  r loops trough climate periods
  i loops trough prediction models
  '

#############################################################
#### Load yield predictions derived from different RCms ####
############################################################
for (s in seq_along(namelist_RCMs)){

  PredictData_df_tidy[[s]] <- read_csv(paste("./data/data_proj/output/",  namelist_RCMs[[s]],"/Climate_predicted.csv", sep="") )
}

##############################################################
#### Make one large data.frame representing all the RCMS ####
############################################################
PredictData_df_tidy <- bind_rows(bind_rows(bind_rows(bind_rows(PredictData_df_tidy[[1]], 
                                                                PredictData_df_tidy[[2]]), 
                                                                  PredictData_df_tidy[[3]]), 
                                                                    PredictData_df_tidy[[4]]), 
                                                                      PredictData_df_tidy[[5]])

# ##################################################################################################
# #### Make one large data,frame and adding to large data.frame tagging all 5 RCMs as Avg_RCMs ####
# ################################################################################################
# PredictData_df_tidy_avgRCMs <- PredictData_df_tidy %>% mutate(RCM = "Avg_RCMs")
# unique(PredictData_df_tidy_avgRCMs$RCM)
# unique(PredictData_df_tidy$RCM)
# 
# PredictData_df_tidy <- bind_rows(PredictData_df_tidy ,PredictData_df_tidy_avgRCMs )
# unique(PredictData_df_tidy$RCM) 

##############################################################################################
#### Generate list with data.frame container for each climate period: Summary Statistics ####
############################################################################################
PredictData_df_tidy_summaries_list <- 
  # PredictData_df_tidy_summaries_list2 <- 
  # PredictData_df_tidy_summaries_list3 <- 
  # PredictData_df_tidy_summaries_list4 <-
  list(PredictData_df_tidy_summaries_1979 = data.frame(), 
       PredictData_df_tidy_summaries_2021 = data.frame(),
        PredictData_df_tidy_summaries_2070 = data.frame())


PredictData_df_tidy_summaries_list_diff <- list(PredictData_df_tidy_summaries_1979_diff = data.frame(), 
                                                PredictData_df_tidy_summaries_2021_diff = data.frame(),
                                                PredictData_df_tidy_summaries_2070_diff = data.frame())

PredictData_df_tidy_summaries_list_allModels <- list(PredictData_df_tidy_summaries_1979_allModels = data.frame(), 
                                                     PredictData_df_tidy_summaries_2021_allModels = data.frame(),
                                                     PredictData_df_tidy_summaries_2070_allModels = data.frame())

###############################################################################################################################
#### Loop to split entire time in three periods and create dummy for each time period, i.e. reference and climate periods ####
#############################################################################################################################
for (r in 1:3){
  PredictData_df_tidy_summaries_list[[r]]  <- 
    PredictData_df_tidy  %>% 
    filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) %>% 
    mutate(climate_period = climateyears_dummy_list[[r]])
}

#############################################################################################################################
#### Loop to generate the mean conditional on the RCMs (and all models) and the administrative district for each period ####
###########################################################################################################################
for (r in 1:3){
  PredictData_df_tidy_summaries_list[[r]]  <- 
    PredictData_df_tidy_summaries_list[[r]]  %>%
    group_by(comId, RCM) %>% 
    select (comId:year, sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean : climate_period ) %>% 
    mutate_at(vars(sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean : sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean  ), 
              funs( mean=mean(.))  ) 
}

# View(PredictData_df_tidy_summaries_list[[2]])
PredictData_df_tidy_summaries_list
unique(PredictData_df_tidy_summaries_list$PredictData_df_tidy_summaries_1979$RCM)

####################################
#### Change order of variables ####
##################################

for (r in 1:3){
  PredictData_df_tidy_summaries_list[[r]] <- PredictData_df_tidy_summaries_list[[r]] %>%
                      select(RCM, climate_period, comId:year,
                             sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean  : sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_mean)
}

PredictData_df_tidy_summaries_list[[r]]

################################################################################################################################
#### Create difference between Y and Y_mean_ref -> YSubY_mean_ref and between Y_mean and Y_mean_ref -> Y_meanSubY_mean_ref ####
##############################################################################################################################

## ungroup tibble -> otherwise group variables are created which do no allow for substracting ##
PredictData_df_tidy_summaries_list[[1]] <- ungroup(PredictData_df_tidy_summaries_list[[1]])
PredictData_df_tidy_summaries_list[[2]] <-ungroup(PredictData_df_tidy_summaries_list[[2]])
PredictData_df_tidy_summaries_list[[3]] <-ungroup(PredictData_df_tidy_summaries_list[[3]])

for (r in 1:3){
  climatePeriodMinusReferencePeriod_bothMeans     <-  as.tibble(
                                                      PredictData_df_tidy_summaries_list[[r]] %>% select(paste("sMA_", nameList_climate, "_mean", sep="")) -  
                                                      PredictData_df_tidy_summaries_list[[1]] %>% select(paste("sMA_", nameList_climate, "_mean", sep="")) )
  climatePeriodMinusReferencePeriod_onlyLastMeans <-  as.tibble(
                                                      PredictData_df_tidy_summaries_list[[r]] %>% select(paste("sMA_", nameList_climate, sep="")) -  
                                                      PredictData_df_tidy_summaries_list[[1]] %>% select(paste("sMA_", nameList_climate, "_mean", sep="")) )
  climatePeriodMinusReferencePeriod_noMeans       <-  as.tibble(
                                                      PredictData_df_tidy_summaries_list[[r]] %>% select(paste("sMA_", nameList_climate, sep="")) -  
                                                      PredictData_df_tidy_summaries_list[[1]] %>% select(paste("sMA_", nameList_climate, sep="")) )

names(climatePeriodMinusReferencePeriod_bothMeans )     <- paste(names(climatePeriodMinusReferencePeriod_bothMeans), "MinusMean", sep="" )
names(climatePeriodMinusReferencePeriod_onlyLastMeans)  <- paste(names(climatePeriodMinusReferencePeriod_onlyLastMeans), "_levelMinusMean", sep="" )
names(climatePeriodMinusReferencePeriod_noMeans )       <- paste(names(climatePeriodMinusReferencePeriod_noMeans ), "_levelMinusLevel", sep="" )


PredictData_df_tidy_summaries_list_diff[[r]]  <- bind_cols( bind_cols( bind_cols(PredictData_df_tidy_summaries_list[[r]][, 1:7], 
                                                                                climatePeriodMinusReferencePeriod_noMeans),
                                                                                  climatePeriodMinusReferencePeriod_bothMeans), 
                                                                                    climatePeriodMinusReferencePeriod_onlyLastMeans)
}





##########################################################################
#### Combine data.frame to a large one covering all climate periods  ####
########################################################################

## Large data.frame considering all three climate period
PredictData_df_tidy_climate_diff <- bind_rows(bind_rows(PredictData_df_tidy_summaries_list_diff[[1]], 
                                                        PredictData_df_tidy_summaries_list_diff[[2]]), 
                                                          PredictData_df_tidy_summaries_list_diff[[3]])

## Data.frame considering the climate periods 2021 - 2050 and 2070 - 2099
PredictData_df_tidy_climate20212070_diff <- bind_rows(PredictData_df_tidy_summaries_list_diff[[2]], 
                                                      PredictData_df_tidy_summaries_list_diff[[3]])

PredictData_df_tidy_climate_diff 
PredictData_df_tidy_climate20212070_diff

#########################################################################################################
#### Make data.frames which does not differentiate between modell - allow for "All Models" - facet #####
#######################################################################################################
## Filter for RCMs -> avoid average
PredictData_df_tidy_climate_allModels         <- as.data.frame(PredictData_df_tidy_climate_diff         %>% filter(RCM %in% (namelist_RCMs)))
PredictData_df_tidy_climate20212070_allModels <- as.data.frame(PredictData_df_tidy_climate20212070_diff %>% filter(RCM %in% (namelist_RCMs)))

PredictData_df_tidy_climate_allModels         <- as.tibble(PredictData_df_tidy_climate_allModels         %>% mutate(RCM = "All Models"))
PredictData_df_tidy_climate20212070_allModels <- as.tibble(PredictData_df_tidy_climate20212070_allModels %>% mutate(RCM = "All Models"))

PredictData_df_tidy_climate_complete          <- bind_rows(PredictData_df_tidy_climate_diff, PredictData_df_tidy_climate_allModels)
PredictData_df_tidy_climate20212070_complete  <- bind_rows(PredictData_df_tidy_climate20212070_diff, PredictData_df_tidy_climate20212070_allModels)

########################################################
#### Define Data Summary Statistic used in ggplots ####
######################################################
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)*2
  ymax <- m+sd(x)*2
  return(c(y=m,ymin=ymin,ymax=ymax))
} # End of function

######################################################################################
#### Violin Plot for comparing differences between reference and climate periods ####
####################################################################################

for (i in seq_along(nameList_climate) ) {

## Mean muinus mean ##  
pMeanMinusMean <- ggplot(PredictData_df_tidy_climate20212070_complete,  
                         aes_string("climate_period", paste("sMA",nameList_climate[[i]], "meanMinusMean", sep="_" ) ) )

pMeanMinusMean_plot <-  
  pMeanMinusMean + 
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_violin(aes(fill = RCM), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + 
  facet_grid(. ~ RCM)  +
  stat_summary(fun.data=data_summary, color="orange")   + 
  theme_minimal(base_size = 14) +  
  theme(legend.position="none")  + 
  scale_fill_brewer(palette="Greys")  +
  # ggtitle(paste(nameList_climate[[i]]))  +
  ylab("Mean(Y) of climate period - Mean(Y) of reference period") + 
  xlab("Climate Period") +
  scale_y_continuous(limits=c(-80, 50))  + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)


## Export data ##
dir.create(paste("./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]], sep=""), showWarnings = FALSE)
ggsave    (paste("./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]], "/ViolinPlot_meanMinusMean.pdf", sep=""),
            pMeanMinusMean_plot, width=16, height=9) 



## level minus mean ##
pLevelMinusMean <- ggplot(PredictData_df_tidy_climate_complete, 
                          aes_string("climate_period", paste("sMA",nameList_climate[[i]], "levelMinusMean", sep="_" ) ) )

pLevelMinusMean_plot <-  pLevelMinusMean + geom_hline(yintercept=0, color="gray", size=1) +
  geom_violin(aes(fill = RCM), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue") +
  facet_grid(. ~RCM) +
  stat_summary(fun.data=data_summary, color="orange") + 
  theme_minimal(base_size = 14) + 
  theme(legend.position="none", axis.text.y = element_text(size = 15)) + 
  scale_fill_brewer(palette="Greys") +
  # ggtitle(paste(nameList_climate[[i]])) + 
  ylab("Y of climate period - Mean(Y) of reference period ") + 
  xlab("Climate Period") +
  scale_y_continuous(limits=c(-500, 500))  + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)

ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]],"/ViolinPlot_levelMinusMean.pdf", sep=""), 
       pLevelMinusMean_plot, width=16, height=9)


## level minus mean - noLimit ##
pLevelMinusMean <- ggplot(PredictData_df_tidy_climate_complete, 
                          aes_string("climate_period", paste("sMA",nameList_climate[[i]], "levelMinusMean", sep="_" ) ) )

pLevelMinusMean_plot <-  pLevelMinusMean + geom_hline(yintercept=0, color="gray", size=1) +
  geom_violin(aes(fill = RCM), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue") + 
  facet_grid(. ~RCM)  +
  stat_summary(fun.data=data_summary, color="orange")  + 
  theme_minimal(base_size = 14) + 
  theme(legend.position="none", axis.text.y = element_text(size = 15)) + 
  scale_fill_brewer(palette="Greys") +
  # ggtitle(paste(nameList_climate[[i]])) + 
  ylab("Y of climate period - Mean(Y) of reference period ") + 
  xlab("Climate Period") +
  # scale_y_continuous(limits=c(-500, 500))  + 
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)

ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]],"/ViolinPlot_levelMinusMean_noLimit.pdf", sep=""), 
       pLevelMinusMean_plot, width=16, height=9)



## level minus level ##
pLevelMinusLevel <- ggplot(PredictData_df_tidy_climate_complete, 
                           aes_string("climate_period", paste("sMA",nameList_climate[[i]], "levelMinusLevel", sep="_" ) ) )
pLevelMinusLevel_plot <- pLevelMinusLevel + 
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_violin(aes(fill = RCM), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + 
  facet_grid(. ~RCM)  +
  stat_summary(fun.data=data_summary, color="orange")  + 
  theme_minimal(base_size = 14) +  theme(legend.position="none", axis.text.y = element_text(size = 15)) + 
  scale_fill_brewer(palette="Greys")  +
  # ggtitle(paste(nameList_climate[[i]])) + 
  ylab("Y of climate period - Y of reference period ") + 
  xlab("Climate Period") +
  # scale_y_continuous(limits=c(-1000, 500))  +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)

ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]],"/ViolinPlot_levelMinusLevel.pdf", sep="") , pLevelMinusLevel_plot, width=16, height=9)



}  ## End of loop trough prediction models


# rm(list=ls())

