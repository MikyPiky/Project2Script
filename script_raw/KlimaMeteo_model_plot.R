###################################################################################################
#### Make Plots of the absolute values of the periods and the differences between the periods ####
#################################################################################################

#### Description ####
'
Here I look at climate model specific plots ("MPI","DMI","KNMI","ICTP","SMHI")

Loop through five climate models 
- read in tidy data for each climate model
- Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) -> output is list of data.frames
- Create differences in mean and sd between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099)
- Export summary statistics of the reference and climate periods and the difference in the climate vs. reference period via stargazer
- Produce Plots
  Plots:
    Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
    - T in July
    - P in July
    - SMI in June
    - SMI in July
    - SMI in August

    # Plot of absolute values (same for Mean and SD)  for each climate period 
    # - T in July
    # - P in July 
    # - SMI in June
    # - SMI in July
    # - SMI in August
'

#### Output ####
## Plots
'/Proj2/figures/figures_exploratory/Proj/MeteoVar/'

## Descriptive Statistics of MeteoVar
'/Proj2/figures/figures_exploratory/Proj/MeteoVar/'

#### Dependencies and Input ####
'-  Meteorological Data in tidy format: MeteoMonth_df_tidy_* (* is climate model) -> /Proj2/data/data_proj/ (from KlimaMeteo_netcdfTo_sf&Tidy.R)
 - vg2500_krs -> data_proj_Input/CLC 
'

#### Packages ####
# source("script/script_raw/Packages.R")
source("script/script_raw/BaseModel.R")



##########################################################################################################################################
#### Calculate Averages and SD for the Climate Periods (1971 - 2000, 2021 - 2050, 2070 - 2099) for each Meteo Variables within comId ####
########################################################################################################################################

######################################################################################
#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
' is loaded in BaseMode.R '

## Create List ##
# namelist_RCMs <- c("DMI","ICTP", "KNMI","MPI","SMHI", "All RCMs")
# MeteoMonth_df_tidy <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMHI=list())


###########################################################################
#### Load Meteo_df_tidy data.frames including a column for the models ####
#########################################################################
MeteoMonth_df_tidy_total <- read_csv("./data/data_proj/output/Climate_predicted_allRCMs.csv")

###################################################################
#### Make container which stores the results of each loop run ####

#### RCM container ####
' These containers store the plots of the yield predictions which are needed for the combined plots.'
# plot_mean_diff2070_TJul_list <- 
# plot_mean_diff2070_PJul_list <- 
# plot_mean_diff2070_SMIJun_list <- 
# plot_mean_diff2070_SMIAug_list <-
# plot_mean_diff2070_SMIJul_list <- 
#   plot_mean_diff2070_SMIJul_list <- 
#   plot_mean_diff2070_SMIJul_list <- 
RCM_list <- 
  list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMHI=list()) 

#### Create different container depend on title, legend, and time period ####

## 2070 - 2099
MeteoMonth_df_tidy_summaries_diff2070_sf_short
plot_diff2070_list <- 
  plot_diff2070_list_notitle_legend <- 
  plot_diff2070_list_title_legend <- 
  plot_diff2070_list_notitle_nolegend <- 
  plot_diff2070_list_title_nolegend <- 
    list(TJul_Mean = RCM_list, 
         PJul_Mean = RCM_list, 
         SMIJun_Mean = RCM_list,
         SMIAug_Mean = RCM_list, 
         SMIJul_Mean = RCM_list,
         
         TJul_SD = RCM_list, 
         PJul_SD = RCM_list, 
         SMIJun_SD = RCM_list, 
         SMIAug_SD = RCM_list, 
         SMIJul_SD = RCM_list, 
         
         sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Mean = RCM_list,
         sMA_lm.fit_SMI_6_Jul_anomaly_demean_Mean = RCM_list,
         sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_Mean = RCM_list,
         sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_Mean = RCM_list,
         
         sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Sd = RCM_list,
         sMA_lm.fit_SMI_6_Jul_anomaly_demean_Sd = RCM_list,
         sMA_mgcv_bestEARTH_noInteraction_T_anomaly_demean_Sd = RCM_list, 
         sMA_mgcv_SMI_6_Jun_Aug_anomaly_demean_Sd = RCM_list)

str(plot_diff2070_list )

# plot_diff2021_list <- 
#   plot_diff2021_list_notitle_legend <- 
#   plot_diff2021_list_title_legend <- 
#   plot_diff2021_list_notitle_nolegend <- 
#   plot_diff2021_list_title_nolegend <- 
#   list(TJul = plot_mean_diff2070_TJul_list, 
#        PJul = plot_mean_diff2070_PJul_list, 
#        SMIJun = plot_mean_diff2070_SMIJun_list, 
#        SMIAug = plot_mean_diff2070_SMIAug_list, 
#        SMIJul = plot_mean_diff2070_SMIJul_list)
  
  # list(TJul = plot_sd_diff2070_TJul_list, PJul = plot_sd_diff2070_PJul_list, SMIJun = plot_sd_diff2070_SMIJun_list, 
                              # SMIAug = plot_sd_diff2070_SMIAug_list, SMIJul = plot_sd_diff2070_SMIJul_list)

# #### Combine mean and sd container ##
# plot_diff2070_list_statistics <- list(mean = plot_mean_diff2070_list, sd= plot_sd_diff2070_list)
# str(plot_diff2070_list_statistics)
# 
# #### Create list for plots with and without titles ####
# plot_diff2070_list_title <- list(notitle = plot_diff2070_list_statistics, title = plot_diff2070_list_statistics )
# str(plot_diff2070_list_title)
# 
# #### Create list for plots with legend at bottom and none ####
# plot_diff2070_list_total <- plot_diff2021_list_total <-list(bottomLegend = plot_diff2070_list_title, noLegend = plot_diff2070_list_title)
# str(plot_diff2070_list)

############################################################################################################################################################################
############################################################################################################################################################################

###########################################
#### Define colorRamps of Differences ####
#########################################


##############
#### Mean ####

#### Temp July ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$T_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$T_Jul_demeaned_Mean)

myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
sc_T_Jul_Mean <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(0,4))

#### Prec July ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$P_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$P_Jul_demeaned_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "PuOr")))
sc_P_Jul_Mean <- scale_fill_gradientn("July Pc.",colours = myPalette(100), limits=c(-52, 52))

#### June SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Jun_Mean)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Jun_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc_SMI_Jun_Mean <- scale_fill_gradientn("June SMI", colours = myPalette(100), limits=c(-0.4, 0.4))

#### July SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Jul_Mean)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Jul_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc_SMI_Jul_Mean <- scale_fill_gradientn("July SMI", colours = myPalette(100), limits=c(-0.4, 0.4))

#### August SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Aug_Mean)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Aug_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc_SMI_Aug_Mean <- scale_fill_gradientn("August SMI", colours = myPalette(100), limits=c(-0.4, 0.4))

# ############
# #### SD ####


#### July Temperature ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$T_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$T_Jul_demeaned_Sd)

myPalette <- colorRampPalette((brewer.pal(9, "PRGn")))
sc_T_Jul_Sd <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(-0.7, 0.7))

#### July Prec ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$P_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$P_Jul_demeaned_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc_P_Jul_Sd <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(-25, 25))

#### June SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Jun_Sd)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Jun_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc_SMI_Jun_Sd <- scale_fill_gradientn("June SMI", colours = myPalette(100), limits=c(-0.25, 0.25))

#### July SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Jul_Sd)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Jul_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc_SMI_Jul_Sd <- scale_fill_gradientn("July SMI", colours = myPalette(100), limits=c(-0.25, 0.25))

#### August SMI ####
# summary(MeteoMonth_df_tidy_summaries_diff2070_sf$SMI_Aug_Sd)
# summary(MeteoMonth_df_tidy_summaries_diff2021_sf$SMI_Aug_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc_SMI_Aug_Sd <- scale_fill_gradientn("August SMI", colours = myPalette(100), limits=c(-0.25, 0.25))

#### Yield Anomaly Mean ####
myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
sc_Yield_Mean <- scale_fill_gradientn("dt/ha", colours = myPalette(100),  limits=c(-70, 70))

#### Yield Anomaly SD ####
myPalette <- colorRampPalette((brewer.pal(11, "PiYG")))
sc_Yield_Sd <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(-30, 30))

# ################################################
# #### Define ColorRamps for absolute values ####
# ##############################################
# 
# ##############
# #### Mean ####
# 
# #### T in July ####
# # str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$T_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$T_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$T_Jul_demeaned_Mean)
# 
# myPalette <- colorRampPalette((brewer.pal(9, "RdPu")))
# sc_abs_T_Jul_Mean <- scale_fill_gradientn("July Temp.",colours = myPalette(100), limits=c(-2, 2))
# #   sc_abs_T_Jul_Mean <- scale_fill_gradientn("July Temp.",colours = myPalette(100), limits=c(11, 24))
# 
# 
# #### P in July ####
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$P_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$P_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$P_Jul_demeaned_Mean)
# 
# myPalette <- colorRampPalette((brewer.pal(9, "PuBu")))
# sc_abs_P_Jul_Mean <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(-40, 40))
# 
# #### SMI  ####
# ## June
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Jun_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Jun_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Jun_demeaned_Mean)
# 
# ## July ##
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Jul_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Jul_demeaned_Mean)
# 
# 
# ## August ##
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Aug_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Aug_demeaned_Mean)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Aug_demeaned_Mean)
# 
# #### Set common colorRamp for all SMI ####
# myPalette <- colorRampPalette((brewer.pal(9, "BrBG")))
# sc_abs_SMI_Mean <- scale_fill_gradientn(colours = myPalette(100), limits=c(0, 1))
# 
# ############################
# #### Standard Deviation ####
# 
# ####  T in July ####
# 
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$T_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$T_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$T_Jul_demeaned_Sd)
# 
# 
# myPalette <- colorRampPalette((brewer.pal(9, "RdPu")))
# sc_abs_T_Jul_Sd <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(0, 2.5))
# 
# ####  P in July ####
# 
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$P_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$P_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$P_Jul_demeaned_Sd)
# 
# myPalette <- colorRampPalette((brewer.pal(9, "PuBu")))
# sc_abs_P_Jul_Sd <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(0, 100))
# 
# 
# ####  SMI ####
#   
# ## June 
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Jun_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Jun_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Jun_demeaned_Sd)
# 
# ## July 
# 
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Jul_demeaned_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Jul_demeaned_Sd)
# 
# ## August 
# 
# str(MeteoMonth_df_tidy_summaries_sf_list,1)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[1]]$SMI_Aug_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[2]]$SMI_Aug_Sd)
# summary(MeteoMonth_df_tidy_summaries_sf_list [[3]]$SMI_Aug_Sd)
# 
# 
# ## Set shared colorRamp for all SMI ## 
# myPalette <- colorRampPalette((brewer.pal(9, "GnBu")))
# sc_abs_SMI_Sd <- scale_fill_gradientn(colours = myPalette(100), limits=c(0.1, 0.4))
####



#############################################
#### Combine color variables in one list ####
list_sc <- c( sc_T_Jul_Mean, sc_P_Jul_Mean, sc_SMI_Jun_Mean, sc_SMI_Jul_Mean, sc_SMI_Aug_Mean ,sc_T_Jul_Sd, sc_P_Jul_Sd, sc_SMI_Jun_Sd, sc_SMI_Jul_Sd, sc_SMI_Aug_Sd, 
              sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Sd, sc_Yield_Sd, sc_Yield_Sd, sc_Yield_Sd )
# list_sc_abs <- c( sc_abs_T_Jul_Mean, sc_abs_P_Jul_Mean, sc_abs_SMI_Mean, sc_abs_SMI_Mean, sc_abs_SMI_Mean,
#                   sc_abs_T_Jul_Sd, sc_abs_P_Jul_Sd, sc_abs_SMI_Sd, sc_abs_SMI_Sd, sc_abs_SMI_Sd)






#############################################
#### Create Variables used in the loops ####
###########################################

###########################################
#### Faktor to accaunt for Mean and SD ####


###########################################
#### Create list of variables to plot ####
#########################################
## List of colorRamps ##

# ## Statistic
# list_statistic <- c("Mean", "Standard Deviation")
# list_statistic_export <- c("Mean", "StandardDeviation")
# Mean_Fac <- 5
# Sd_Fac <- 10
# list_statistics_Fac <- c(Mean_Fac, Sd_Fac)

## Color List
# list_sc_Fac <- c(0,5)

## Variable List
list_variableName <-c(paste("Mean of", c("July Temp.", "July Prec.", "June SMI", "July SMI", "Aug. SMI")),
                      paste("SD of", c("July Temp.", "July Prec.", "June SMI", "July SMI", "Aug. SMI")) , 
                      paste("Mean of", nameList_climate),
                      paste("SD of", nameList_climate) )

list_variableName_export <- c(paste(c("July_Temp", "July_Prec", "June_SMI", "July_SMI", "Aug_SMI"), "Mean", sep="_"), 
                            paste(c("July_Temp", "July_Prec", "June_SMI", "July_SMI", "Aug_SMI"), "SD", sep="_"), 
                            paste( nameList_climate, "Mean", sep="_"), 
                            paste( nameList_climate, "SD", sep="_"))

# ## Legend List
list_legend_Variables <- c("none", "bottom")
list_legend_export <- c("noLegend", "legend")
# 
list_titleVariables <- list(element_text(color="white") , element_text(color="black") )
list_title_export <- list("noTitle", "title")

#######################################
#### Define Function used in Loop ####
#####################################
plot_variables = function (TimePeriod, ExVar, RCMs, Tit, Leg){
  ggplot(TimePeriod) +
    geom_sf(data=vg2500_krs, fill="gray", color="white")  +
    geom_sf(aes(fill = TimePeriod[[5 + ExVar]])) +
    ggtitle(paste(unique(TimePeriod$time),  ": ", list_variableName[[ ExVar]], " - ",namelist_RCMs[[RCMs]],  sep="") ) +
    list_sc[[ExVar]] +
    theme_bw()  +
    theme(legend.position=list_legend_Variables[Leg]) +
    theme(legend.title=element_blank()) +
    theme(title =list_titleVariables [[Tit]] )
}

# rm(Stat)
# TimePeriod <- MeteoMonth_df_tidy_summaries_diff2021_sf_short
# paste(unique(TimePeriod$time),  ": ", list_variableName[[ ExVar]], " - ",namelist_RCMs[[RCMs]],  sep="")

# plot_variables(MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  2, 2)


######################
#### Start Loops ####
####################

## Variables for Statistics used: 1 = Mean, 2 = SD
# Stat = 1 # Mean
## Variables for notitle of title: 1 = no title, 2 = title
# Tit = 2 # title
## Variablles for no legend of legend ##
# Leg = 2 # legend

# m is for Variable used, t is for RCM




# seq_along(namelist_RCMs)
###########################################
#### Loop through all 5 climate models ####
for (RCMs in seq_along(namelist_RCMs))
  # local(
    {
  # RCMs <- RCMs
  ## Read in the tidy data frames ##
  MeteoMonth_df_tidy <- MeteoMonth_df_tidy_total %>% filter(RCM == namelist_RCMs[[RCMs]])
  print(unique(MeteoMonth_df_tidy$RCM))
  # summary(MeteoMonth_df_tidy)
  # str(MeteoMonth_df_tidy)
  # str(MeteoMonth_df_tidy_total)
  
  ## Generate list of start and end dates of climate periods ##
  'Those are necessary for the conditioning in filter'
  climateyears_list <- list(c(1971,2021,2070), c(2000, 2050, 2099))
  
  ############################################################################################################################
  #### Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) ####
  
  #### Generate list with data.frame container for each climate period: Mean ####
  MeteoMonth_df_tidy_summaries_list <- list(MeteoMonth_df_tidy_summaries_1979 = data.frame(), MeteoMonth_df_tidy_summaries_2021= data.frame(),
                                            MeteoMonth_df_tidy_summaries_2070 = data.frame())
  
  #### List to store sf.data.frames ####
  MeteoMonth_df_tidy_summaries_sf_list <- list(MeteoMonth_df_tidy_summaries_sf_1979 = data.frame(), MeteoMonth_df_tidy_summaries_sf_2021= data.frame(),
                                               MeteoMonth_df_tidy_summaries_sf_2070 = data.frame())
  #### Start of loop ####  
  for (i in 1:3){
    MeteoMonth_df_tidy_summaries_list[[i]] <- MeteoMonth_df_tidy %>%  
      filter(year >=  climateyears_list[[1]][i] & year <= climateyears_list[[2]][i]) %>% 
      group_by(comId) %>%
     select_if(is.numeric)  %>% 
      summarise_all(.funs = c (Mean="mean", Sd="sd"))
    #### Merge with Spatial Information ####
    MeteoMonth_df_tidy_summaries_sf_list[[i]] <- inner_join(vg2500_krs, MeteoMonth_df_tidy_summaries_list[[i]], by=c("comId")) 
    
  } ## End of loop
  
  
  #### Check data.frame created ####
  summary(MeteoMonth_df_tidy_summaries_list[[2]])
  summary(MeteoMonth_df_tidy_summaries_sf_list[[2]])
  
  
  ##################################################################################################################################
  #### Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) ####
  ################################################################################################################################
  'Zwar werden hier demeaned data benutz mit der Referenzperiod 1971 - 2000. Jedoch ziehe ich nochmals die Werte der Referenzperiode ab. Diese sind für 
  alle außer SMI Werte sehr nahe an Null. Für SMI wird dadurch die Abweichung vom Mean ausgeben, und damit auch diese Daten Null zentriert.
  Auch macht das Wohl für die yield anomalien Sinn, da daduch die Entwicklung dieser angezeigt wird. 
  Grundsätzlich stellt sich die Frage, wie dies auf SD übertragen werden kann. 
  '
  
  MeteoMonth_df_tidy_summaries_list[[1]]
  MeteoMonth_df_tidy_summaries_diff2021 <- MeteoMonth_df_tidy_summaries_list[[2]] - MeteoMonth_df_tidy_summaries_list[[1]]
  MeteoMonth_df_tidy_summaries_diff2070 <- MeteoMonth_df_tidy_summaries_list[[3]] - MeteoMonth_df_tidy_summaries_list[[1]] 
  MeteoMonth_df_tidy_summaries_diff2021
  
  
  MeteoMonth_df_tidy_summaries_diff2021$comId <- MeteoMonth_df_tidy_summaries_list[[2]]$comId
  MeteoMonth_df_tidy_summaries_diff2070$comId <- MeteoMonth_df_tidy_summaries_list[[2]]$comId
  
  MeteoMonth_df_tidy_summaries_diff2021$RCM <- rep(unique(MeteoMonth_df_tidy$RCM), 406)
  MeteoMonth_df_tidy_summaries_diff2070$RCM <- rep(unique(MeteoMonth_df_tidy$RCM), 406)
  
  MeteoMonth_df_tidy_summaries_diff2021$time <- rep("(2021 - 2099) - (1971 - 2000)", 406)
  MeteoMonth_df_tidy_summaries_diff2070$time <- rep("(2070 - 2099) - (1971 - 2000)", 406)
  
  summary(MeteoMonth_df_tidy_summaries_list[[1]])
  summary(MeteoMonth_df_tidy_summaries_diff2021)
  summary(MeteoMonth_df_tidy_summaries_diff2070)
  
  
  ##########################################################################
  #### Merge difference data with vg2500_krs to get Spatial Attributes ####
  MeteoMonth_df_tidy_summaries_diff2021_sf <-  inner_join(vg2500_krs, MeteoMonth_df_tidy_summaries_diff2021, by = c("comId"))
  MeteoMonth_df_tidy_summaries_diff2070_sf <-  inner_join(vg2500_krs, MeteoMonth_df_tidy_summaries_diff2070, by = c("comId"))
  
  # MeteoMonth_df_tidy_summaries_diff2021_sf$comId ## order of vg2500_krs is kept (sort=F)
  # 
  # MeteoMonth_df_tidy_summaries_diff2021_sf
  # 
  # summary(MeteoMonth_df_tidy_summaries_diff2070_sf)
  
  ############################################################################################################################################################################
  ############################################################################################################################################################################
  # ####################################
  # #### Export summary statistics ####
  # ##################################
  
  # stargazer(MeteoMonth_df_tidy_summaries_diff2021, type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/","DeskriptiveStats_diff2021_", namelist_RCMs[[t]], ".txt", sep=""))
  # stargazer(MeteoMonth_df_tidy_summaries_diff2070, type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_diff2070_",  namelist_RCMs[[t]],".txt", sep=""))
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[1]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_1970_", namelist_RCMs[[t]],".txt", sep=""))
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[2]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2021_", namelist_RCMs[[t]],".txt", sep=""))
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[3]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2070_", namelist_RCMs[[t]],".txt", sep=""))
  # 
  
  

  ##############################################################################################################################################################################
  ##############################################################################################################################################################################
  #### Start of Plotting ####
  ##############################################################################################################################################################################
  ##############################################################################################################################################################################
  
  ##############################################################################################################################################################################
  #### Plot of Differences (Climate Period - Reference Period) and absolute plots of Climate Periods and Reference Periods ####
  ##############################################################################################################################################################################
  ' Variables:
  - T in July
  - P in July
  - SMI in June
  - SMI in July
  - SMI in August
  '

  #########################################################################
  #### Subset data.frame for Mean and Sd Values of relevant variables ####
  #######################################################################
  
  #### Differences ####
  MeteoMonth_df_tidy_summaries_diff2070_sf$RCM
  summary(MeteoMonth_df_tidy_summaries_diff2070_sf$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Mean)
  MeteoMonth_df_tidy_summaries_diff2070_sf_short <- MeteoMonth_df_tidy_summaries_diff2070_sf %>%
    select(comId , USE ,GEN , SHAPE_LENG,SHAPE_AREA ,  T_Jul_demeaned_Mean, P_Jul_demeaned_Mean, SMI_Jun_Mean, SMI_Jul_Mean, SMI_Aug_Mean,
                                                    T_Jul_demeaned_Sd, P_Jul_demeaned_Sd, SMI_Jun_Sd, SMI_Jul_Sd, SMI_Aug_Sd, contains("sMA"), time)
  
  summary( MeteoMonth_df_tidy_summaries_diff2070_sf_short )
  
  MeteoMonth_df_tidy_summaries_diff2021_sf_short <- MeteoMonth_df_tidy_summaries_diff2021_sf  %>%
    select(comId , USE ,GEN , SHAPE_LENG,SHAPE_AREA ,  T_Jul_demeaned_Mean, P_Jul_demeaned_Mean, SMI_Jun_Mean, SMI_Jul_Mean, SMI_Aug_Mean,
           T_Jul_demeaned_Sd, P_Jul_demeaned_Sd, SMI_Jun_Sd, SMI_Jul_Sd, SMI_Aug_Sd, contains("sMA"), time)
   
  #### Absolute Values ####
  # MeteoMonth_df_tidy_summaries_absolute1970_sf_short  <- MeteoMonth_df_tidy_summaries_sf_list [[1]] %>%
  #   select(comId , USE ,GEN , SHAPE_LENG,SHAPE_AREA ,  T_Jul_demeaned_Mean, P_Jul_demeaned_Mean, SMI_Jun_Mean, SMI_Jul_Mean, SMI_Aug_Mean,
  #          T_Jul_demeaned_Sd, P_Jul_demeaned_Sd, SMI_Jun_Sd, SMI_Jul_Sd, SMI_Aug_Sd)
  # 
  # MeteoMonth_df_tidy_summaries_absolute2021_sf_short  <- MeteoMonth_df_tidy_summaries_sf_list [[2]]  %>%
  #   select(comId , USE ,GEN , SHAPE_LENG,SHAPE_AREA ,  T_Jul_demeaned_Mean, P_Jul_demeaned_Mean, SMI_Jun_Mean, SMI_Jul_Mean, SMI_Aug_Mean,
  #          T_Jul_demeaned_Sd, P_Jul_demeaned_Sd, SMI_Jun_Sd, SMI_Jul_Sd, SMI_Aug_Sd)
  # 
  # MeteoMonth_df_tidy_summaries_absolute2050_sf_short  <- MeteoMonth_df_tidy_summaries_sf_list [[3]]  %>%
  #   select(comId , USE ,GEN , SHAPE_LENG,SHAPE_AREA ,  T_Jul_demeaned_Mean, P_Jul_demeaned_Mean, SMI_Jun_Mean, SMI_Jul_Mean, SMI_Aug_Mean,
  #          T_Jul_demeaned_Sd, P_Jul_demeaned_Sd, SMI_Jun_Sd, SMI_Jul_Sd, SMI_Aug_Sd)
  
 # summary(MeteoMonth_df_tidy_summaries_absolute2021_sf_short$SMI_Jun_Sd)
  
  # names(MeteoMonth_df_tidy_summaries_absolute2021_sf_short)
  
 
  # Tit <- 2
  # Leg <- 2
  
  print(namelist_RCMs[[RCMs]])
  # ###################################
  # #### (2070-2099) - (1971-2000) ####
  #### loop through Variables
  for (ExVar in seq_along(list_variableName))
    # local(
      { # seq_along(list_variableName)
      # ExVar <-1
    #   #### loop trough title
    #   for(Tit in seq_along(list_title_export ))
    #     local({
    #       Tit <-Tit
    #     #### loop trough legend
    #     for(Leg in seq_along(list_legend_export) )
    #       local({ 
    #         Leg <- Leg

          ##################################
          ### (2071-2099) - (1971-2000) ####
          print(list_variableName[[ ExVar]])

          Tit=1; Leg=2
          plot_diff2070_list_notitle_legend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
          Tit=2; Leg=2
          plot_diff2070_list_title_legend[[ExVar]][[RCMs]] <-  plot_variables(MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
          Tit=1; Leg=1
          plot_diff2070_list_notitle_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
          Tit=2; Leg=1
          plot_diff2070_list_title_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
         
        TimePeriod <-   MeteoMonth_df_tidy_summaries_diff2021_sf_short  
          
          # ExVar;           RCMs;            Tit;           Leg
          # 
          # plot_diff2021_list[[Leg]][[Tit]][[Stat]][[ExVar]][[RCMs]] <<- plot_variables(data, MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  Tit, Leg)
          # plot_diff2070_list[[Leg]][[Tit]][[Stat]][[ExVar]][[RCMs]] <<- plot_variables(data, MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
          # MeteoMonth_df_tidy_summaries_diff2021_sf_short
       

          # ###################################
          # #### (2021-2050) - (1971-2000) ####
          # plot_diff2021 <-
          #   ggplot(MeteoMonth_df_tidy_summaries_diff2021_sf_short) +
          #   geom_sf(aes(fill =  MeteoMonth_df_tidy_summaries_diff2021_sf_short[[list_statistics_Fac[[g]] + m]])) +
          #   ggtitle(paste("(2021-2050) - (1971- 2000)",  ": ", list_statistic[[g]], " of ", list_variableName[[m]],  sep="")) +
          #   list_sc[[list_sc_Fac[[g]] + m]] +
          #   theme_bw() + 
          #   theme(legend.position=list_legend_Variables[k]) +
          #   theme(legend.title=element_blank()) + 
          #   theme(title = list_titleVariables [[n]] )
          # 
          # 
          # # plot_diff2021
          # 
          # #########################################################
          # #### Generate combined plots of the difference plots ####
          # plot_diff <-grid.arrange(plot_diff2021, plot_diff2070, ncol=2, top=textGrob(paste(namelist_RCMs[[t]]), gp=gpar(fontsize=20)))
          # 
          # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/Differences/", list_title_export[[n]], "/", list_legend_export[[k]], "/plot_", 
          #              list_statistic_export[[g]] , "_diff_", list_variableName_export[[m]],"_", 
          #              namelist_RCMs[[t]], "_", list_title_export[[n]], "_", list_legend_export[[k]], ".pdf", sep=""), plot=plot_diff, "pdf",width=14, height=8)
        
          ##########################################
          #### Combine plots of absolute values ####
          # plot_abs <- grid.arrange(plot_1970, plot_2021, plot_2070, ncol=3, top=textGrob(paste(namelist_RCMs[[t]]),gp=gpar(fontsize=30)))  
          
          # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/Absolutes/", list_title_export[[n]], "/", list_legend_export[[k]], "/plot_", 
          #              list_statistic_export[[g]] , "_abs_", list_variableName_export[[m]],"_", 
          #              namelist_RCMs[[t]], "_", list_title_export[[n]], "_", list_legend_export[[k]], ".pdf", sep=""), plot=plot_abs, "pdf",width=14, height=8)
          # 
          #####
          
  #       }) ## End of loop trough legend
  # #     
  #     }) ## End of loop trough title
  # 
  }
  # ) ## Loop trough variables
  
}
# ) ##  Loop trough climate models

# Leg = 2
# Tit = 2
# Stat = 2
ExVar = 1 
RCMs =2
Tit=1; Leg=2
plot_diff2070_list_notitle_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=1; Leg=1
plot_diff2070_list_notitle_nolegend[[ExVar]][[RCMs]] 
Tit=2; Leg=1
plot_diff2070_list_title_nolegend[[ExVar]][[RCMs]]

Tit=2; Leg=2; ExVar = 1; RCMs =1
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =2
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =3
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =4
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =5
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]

Tit=2; Leg=2; ExVar = 1; RCMs =1
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =2
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =3
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =4
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]
Tit=2; Leg=2; ExVar = 1; RCMs =5
plot_diff2070_list_title_legend[[ExVar]][[RCMs]]





