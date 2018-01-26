###################################################################################################
#### Make Plots of the absolute values of the periods and the differences between the periods ####
#################################################################################################

#### Description ####
'
Here I look at climate model specific plots ("MPI","DMI","KNMI","ICTP","SMHI")

Loop through five climate models 
- read in tidy data including all climate models: use of data which also have a RCM Indikator for "All_RCMs"
- Make container where to store the plots in for the climate period 2070 - 2099, depending on whether with or without: title, legend
- Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) -> output is list of data.frames
- Define colorRamps used in the different plots
- Create Variables used in Loop
- Define Plotting Function used in Loops
- Start Loop through RCMs
  - Create differences in mean and sd between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099)
  - Export summary statistics of the reference and climate periods and the difference in the climate vs. reference period via stargazer
  - Start Loop Throughs Variables Plotted 
    - Produce Plots with or without: title, legend
      Plots:
        Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
        - T in July
        - P in July
        - SMI in June
        - SMI in July
        - SMI in August
        - Output of the different predictive models

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


########################################################################################
#### Load Meteo_df_tidy data.frames including a column for the RCMs and "All RCMs" ####
######################################################################################
MeteoMonth_df_tidy_total <- read_csv("./data/data_proj/output/Climate_predicted_allRCMs_total.csv")
summary(MeteoMonth_df_tidy_total)

###################################################################
#### Make container which stores the results of each loop run ####
#################################################################

#### RCM container ####
' These containers store the plots of the yield predictions which are needed for the combined plots.'

RCM_list <- 
  list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMHI=list(), All_RCMs = list()) 

#### Create different container depend on title, legend, and time period ####
plot_diff2070_list_notitle_legend <- 
plot_diff2070_list_title_legend <- 
plot_diff2070_list_notitle_nolegend <- 
plot_diff2070_list_title_nolegend <- 
plot_diff2021_list_notitle_legend <- 
plot_diff2021_list_title_legend <- 
plot_diff2021_list_notitle_nolegend <- 
plot_diff2021_list_title_nolegend <- 
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
sc_Yield_Mean <- scale_fill_gradientn("dt/ha", colours = myPalette(100),  limits=c(-60, 60))

#### Yield Anomaly SD ####
myPalette <- colorRampPalette((brewer.pal(11, "PiYG")))
sc_Yield_Sd <- scale_fill_gradientn("Yield Anomaly", colours = myPalette(100), limits=c(-30, 30))



#############################################
#### Combine color variables in one list ####
list_sc <- c( sc_T_Jul_Mean, sc_P_Jul_Mean, sc_SMI_Jun_Mean, sc_SMI_Jul_Mean, sc_SMI_Aug_Mean ,sc_T_Jul_Sd, sc_P_Jul_Sd, sc_SMI_Jun_Sd, sc_SMI_Jul_Sd, sc_SMI_Aug_Sd, 
              sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Mean, sc_Yield_Sd, sc_Yield_Sd, sc_Yield_Sd, sc_Yield_Sd )



#############################################
#### Create Variables used in the loops ####
###########################################

###########################################
#### Faktor to accaunt for Mean and SD ####


###########################################
#### Create list of variables to plot ####
#########################################
## Add All_RCMs to namelist_RCMs ##
namelist_RCMs_total <- c(namelist_RCMs, "All_RCMs")

## Variable List
list_variableName <-c(paste("Mean of", c("July Temp.", "July Prec.", "June SMI", "July SMI", "Aug. SMI")),
                      paste("SD of", c("July Temp.", "July Prec.", "June SMI", "July SMI", "Aug. SMI")) , 
                      paste("Mean of", nameList_climate),
                      paste("SD of", nameList_climate) )

list_variableName_export <- c(paste(c("July_Temp", "July_Prec", "June_SMI", "July_SMI", "Aug_SMI"), "Mean", sep="_"), 
                            paste(c("July_Temp", "July_Prec", "June_SMI", "July_SMI", "Aug_SMI"), "SD", sep="_"), 
                            paste( nameList_climate, "Mean", sep="_"), 
                            paste( nameList_climate, "SD", sep="_"))

## Legend List
list_legend_Variables <- c("none", "right")
list_legend_export <- c("noLegend", "legend")
##  
list_titleVariables <- list(element_text(color="white") , element_text(color="black") )
list_title_export <- list("noTitle", "title")

## List of start and end dates of climate periods 
'Those are necessary for the conditioning in filter'
climateyears_list <- list(c(1971, 2021, 2070), c(2000, 2050, 2099))

#######################################
#### Define Function used in Loop ####
#####################################
plot_variables = function (TimePeriod, ExVar, RCMs, Tit, Leg){
  ggplot(TimePeriod) +
    geom_sf(data=vg2500_krs, fill="gray", color="white")  +
    geom_sf(aes(fill = TimePeriod[[5 + ExVar]])) +
    ggtitle(paste(unique(TimePeriod$time),  ": ", list_variableName[[ExVar]], " - ", namelist_RCMs_total[[RCMs]],  sep="") ) +
    list_sc[[ExVar]] +
    theme_bw()  +
    theme(legend.position = list_legend_Variables[Leg]) +
    # theme(legend.title=element_blank()) +
    theme(plot.title =list_titleVariables [[Tit]] )
  
}

# rm(Stat)
# TimePeriod <- MeteoMonth_df_tidy_summaries_diff2021_sf_short
# paste(unique(TimePeriod$time),  ": ", list_variableName[[ ExVar]], " - ",namelist_RCMs[[RCMs]],  sep="")

# plot_variables(MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  2, 2)


######################
#### Start Loops ####
####################

###########################################
#### Loop through all 5 climate models ####
for (RCMs in seq_along(namelist_RCMs_total)){

  #### Filter for particular RCMs ####
  MeteoMonth_df_tidy <- MeteoMonth_df_tidy_total %>% filter(RCM == namelist_RCMs_total[[RCMs]])
  print(unique(MeteoMonth_df_tidy$RCM))
  summary(MeteoMonth_df_tidy$sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean)

  
  ###########################################################################################################################
  #### Loop to create Means and SD of the absolute values for the climate periods 1971 - 2000, 2021 - 2050, 2070 - 2099 ####
  #########################################################################################################################

  #### Generate list with data.frame container for each climate period: Mean ####
  MeteoMonth_df_tidy_summaries_list <- list(MeteoMonth_df_tidy_summaries_1979 = data.frame(), MeteoMonth_df_tidy_summaries_2021= data.frame(),
                                            MeteoMonth_df_tidy_summaries_2070 = data.frame())
  
  #### List to store sf.data.frames ####
  MeteoMonth_df_tidy_summaries_sf_list <- list(MeteoMonth_df_tidy_summaries_sf_1979 = data.frame(), MeteoMonth_df_tidy_summaries_sf_2021= data.frame(),
                                               MeteoMonth_df_tidy_summaries_sf_2070 = data.frame())
  
  #############################################
  #### Start of loop - select for numerics ####  
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
  
  summary(MeteoMonth_df_tidy_summaries_list[[3]])
  summary(MeteoMonth_df_tidy_summaries_sf_list[[3]])
  
  # View(MeteoMonth_df_tidy_summaries_list[[2]])
  
  ##################################################################################################################################
  #### Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) ####
  ################################################################################################################################
  'Zwar werden hier demeaned data benutz mit der Referenzperiod 1971 - 2000. Jedoch ziehe ich nochmals die Werte der Referenzperiode ab. Diese sind für 
  alle außer SMI Werte sehr nahe an Null. Für SMI wird dadurch die Abweichung vom Mean ausgeben, und damit auch diese Daten Null zentriert.
  Auch macht das Wohl für die yield anomalien Sinn, da daduch die Entwicklung dieser angezeigt wird. 
  Grundsätzlich stellt sich die Frage, wie dies auf SD übertragen werden kann. 
  '
  
  
  ## Create Differences ##
  MeteoMonth_df_tidy_summaries_diff2021 <- MeteoMonth_df_tidy_summaries_list[[2]] - MeteoMonth_df_tidy_summaries_list[[1]]
  MeteoMonth_df_tidy_summaries_diff2070 <- MeteoMonth_df_tidy_summaries_list[[3]] - MeteoMonth_df_tidy_summaries_list[[1]] 
  MeteoMonth_df_tidy_summaries_diff2021
  summary(  MeteoMonth_df_tidy_summaries_diff2021)
  
  ## Add new comIds ##
  MeteoMonth_df_tidy_summaries_diff2021$comId <- MeteoMonth_df_tidy_summaries_list[[2]]$comId
  MeteoMonth_df_tidy_summaries_diff2070$comId <- MeteoMonth_df_tidy_summaries_list[[2]]$comId
  
  ## Build new RCM Indicator ##
  MeteoMonth_df_tidy_summaries_diff2021$RCM <- rep(unique(MeteoMonth_df_tidy$RCM), 406)
  MeteoMonth_df_tidy_summaries_diff2070$RCM <- rep(unique(MeteoMonth_df_tidy$RCM), 406)
  
  ## Build Climate Period Indicator ##
  MeteoMonth_df_tidy_summaries_diff2021$time <- rep("(2021 - 2099) - (1971 - 2000)", 406)
  MeteoMonth_df_tidy_summaries_diff2070$time <- rep("(2070 - 2099) - (1971 - 2000)", 406)
  
  summary(MeteoMonth_df_tidy_summaries_list[[1]])
  summary(MeteoMonth_df_tidy_summaries_diff2021)
  summary(MeteoMonth_df_tidy_summaries_diff2070)
  
  
  ##########################################################################
  #### Merge difference data with vg2500_krs to get Spatial Attributes ####
  MeteoMonth_df_tidy_summaries_diff2021_sf <-  inner_join(vg2500_krs, MeteoMonth_df_tidy_summaries_diff2021, by = c("comId"))
  MeteoMonth_df_tidy_summaries_diff2070_sf <-  inner_join(vg2500_krs, MeteoMonth_df_tidy_summaries_diff2070, by = c("comId"))
  

  #### Select Important Variables ####
  MeteoMonth_df_tidy_summaries_diff2021_sf_selected <- MeteoMonth_df_tidy_summaries_diff2021_sf %>% select(SMI_Jun_Mean, SMI_Aug_Mean, P_Jul_demeaned_Mean , T_Jul_demeaned_Mean, 
                                                                                                           sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Mean, 
                                                                                                           SMI_Jun_Sd, SMI_Aug_Sd, P_Jul_demeaned_Sd , T_Jul_demeaned_Sd, 
                                                                                                           sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Sd)
  
  MeteoMonth_df_tidy_summaries_diff2070_sf_selected <- MeteoMonth_df_tidy_summaries_diff2070_sf %>% select(SMI_Jun_Mean, SMI_Aug_Mean, P_Jul_demeaned_Mean , T_Jul_demeaned_Mean, 
                                                                                                           sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Mean, 
                                                                                                           SMI_Jun_Sd, SMI_Aug_Sd, P_Jul_demeaned_Sd , T_Jul_demeaned_Sd, 
                                                                                                           sMA_lm.fit_SMI_6_Jun_Aug_anomaly_demean_Sd)
  
  
  ############################################################################################################################################################################
  ############################################################################################################################################################################
  ######################################################
  #### Export summary statistics of all Variables  ####
  ####################################################
  # 
  # ## Differences ##
  # stargazer(MeteoMonth_df_tidy_summaries_diff2021, type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/","DeskriptiveStats_diff2021_", namelist_RCMs_total[[RCMs]], ".txt", sep=""))
  # stargazer(MeteoMonth_df_tidy_summaries_diff2070, type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_diff2070_",  namelist_RCMs_total[[RCMs]],".txt", sep=""))
  # 
  # ## Absolute Values ##
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[1]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_1970_", namelist_RCMs_total[[RCMs]],".txt", sep=""))
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[2]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2021_", namelist_RCMs_total[[RCMs]],".txt", sep=""))
  # stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_list[[3]]), type = "text", title="Descriptive statistics", digits=3,
  #           out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2070_", namelist_RCMs_total[[RCMs]],".txt", sep=""))


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
  - Yield Output
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
   
 
  # Tit <- 2
  # Leg <- 2
  
  # ###################################
  # #### (2070-2099) - (1971-2000) ####
  #### loop through Variables
  for (ExVar in seq_along(list_variableName)){ 
    
    ##################################
    ### (2071-2099) - (1971-2000) ####
    print("(2070-2099) - (1971-2000)")
    print(list_variableName[[ ExVar]])
    
    Tit=1; Leg=2
    plot_diff2070_list_notitle_legend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=2; Leg=2
    plot_diff2070_list_title_legend[[ExVar]][[RCMs]] <-  plot_variables(MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=1; Leg=1
    plot_diff2070_list_notitle_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=2; Leg=1
    plot_diff2070_list_title_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2070_sf_short, ExVar, RCMs,  Tit, Leg)
    
    # TimePeriod <-   MeteoMonth_df_tidy_summaries_diff2021_sf_short  
    
    ###################################
    #### (2021-2050) - (1971-2000) ####
    print("(2021-2050) - (1971-2000)")
    print(list_variableName[[ ExVar]])
    
    Tit=1; Leg=2
    plot_diff2021_list_notitle_legend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=2; Leg=2
    plot_diff2021_list_title_legend[[ExVar]][[RCMs]] <-  plot_variables(MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=1; Leg=1
    plot_diff2021_list_notitle_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  Tit, Leg)
    Tit=2; Leg=1
    plot_diff2021_list_title_nolegend[[ExVar]][[RCMs]] <-  plot_variables( MeteoMonth_df_tidy_summaries_diff2021_sf_short, ExVar, RCMs,  Tit, Leg)
   
        
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





