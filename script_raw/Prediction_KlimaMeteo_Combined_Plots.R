#### Description ####
'
Make combine plots derived from plots in KlimaMeteo_model_plot.R, KlimaMeteo_total_plot.R and Prediction_Plots.R.
Each specific plot used in the combined plots needs to be created first in those scripts.  

'

#### Dependencies ###
'
KlimaMeteo_model_plot.R, KlimaMeteo_total_plot.R and BasePrediction_Plots_Average and BasePrediction_Plot_Model.R.

'

#### Output ####
'Combined Plots
- TJul, PJul, SMIJun, SMIAug
- TJul, PJul, SMIJun, SMIAug, Yield
- TJul, PJul, SMIJul
- TJul, PJul, SMIJul, Yield
<- "./figures/figures_exploratory/Proj/MeteoVar/"'


################################################################################################################################################################################################
#### Make combined Plots of Means ####
################################################################################################################################################################################################


'Combined Plots
- TJul, PJul, SMIJun, SMIAug
- TJul, PJul, SMIJun, SMIAug, Yield
- TJul, PJul, SMIJul
- TJul, PJul, SMIJul, Yield'

'For the yield plots it is necessary to load plot_sd_diff...list via the BasePrediction_Plots Script. '

## Create List of models to loop trrough##
namelist_RCMs <- c("DMI","ICTP", "KNMI","MPI","SMHI")

#######################################################
#### Mean plots for TJul, PJul, SMIJun, SMIAug ####
plot_diff2070_list[[2]][[2]][[1]][[3]][[l]] 
plot_mean_SMI_6_Jun_Aug <- grid.arrange(plot_mean_1970_TJul , plot_mean_1970_PJul,  plot_mean_1970_SMIJun, plot_mean_1970_SMIAug,
                                        plot_mean_diff2021_TJul, plot_mean_diff2021_PJul, plot_mean_diff2021_SMIJun, plot_mean_diff2021_SMIAug,
                                        plot_mean_diff2070_TJul, plot_mean_diff2070_PJul, plot_mean_diff2070_SMIJun, plot_mean_diff2070_SMIAug,
                                        ncol=4, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=30)))
plot_mean_SMI_6_Jun_Aug
ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jun_Aug_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_mean_SMI_6_Jun_Aug , width=28, height=24)

##############################################################
#### Mean plots for TJul, PJul, SMIJun, SMIAug, Yield ####
plot_mean_yield_SMI_6_Jun_Aug <- grid.arrange(plot_mean_1970_TJul , plot_mean_1970_PJul,  plot_mean_1970_SMIJun, plot_mean_1970_SMIAug,plot_mean_1971_list[[1]][[l]],
                                              plot_mean_diff2021_TJul, plot_mean_diff2021_PJul, plot_mean_diff2021_SMIJun, plot_mean_diff2021_SMIAug, plot_mean_diff2021_list[[1]][[l]],
                                              plot_mean_diff2070_TJul, plot_mean_diff2070_PJul, plot_mean_diff2070_SMIJun, plot_mean_diff2070_SMIAug, plot_mean_diff2070_list[[1]][[l]],
                                              ncol = 5, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=25)))
plot_mean_yield_SMI_6_Jun_Aug

ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jun_Aug_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jun_Aug , width=35, height=24)

###############################################
#### Mean plots for TJul, PJul, SMIJul ####
plot_mean_SMI_6_Jul <- grid.arrange(plot_mean_1970_TJul , plot_mean_1970_PJul,  plot_mean_1970_SMIJul,
                                    plot_mean_diff2021_TJul, plot_mean_diff2021_PJul, plot_mean_diff2021_SMIJul,
                                    plot_mean_diff2070_TJul, plot_mean_diff2070_PJul, plot_mean_diff2070_SMIJul,
                                    ncol=3, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=30)))
plot_mean_SMI_6_Jul
ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jul_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_mean_SMI_6_Jul , width=28, height=24)

######################################################
#### Mean plots for TJul, PJul, SMIJul, Yield ####
plot_mean_yield_SMI_6_Jul <- grid.arrange(plot_mean_1970_TJul , plot_mean_1970_PJul,  plot_mean_1970_SMIJul, plot_mean_1971_list[[2]][[l]],
                                          plot_mean_diff2021_TJul, plot_mean_diff2021_PJul, plot_mean_diff2021_SMIJul,  plot_mean_diff2021_list[[2]][[l]],
                                          plot_mean_diff2070_TJul, plot_mean_diff2070_PJul, plot_mean_diff2070_SMIJul, plot_mean_diff2070_list[[2]][[l]],
                                          ncol = 4, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=20)))
plot_mean_yield_SMI_6_Jul

ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jul_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jul , width=28, height=24)



#########################################################################################################
#### Plot of Means: Comparing Average to the two most extreme models  and the average of all models ####
#######################################################################################################

## _SMI_6_Jun_Aug_ ##
# plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg <- grid.arrange(
#                     arrangeGrob( plot_mean_diff2021_list[[1]][[1]], plot_mean_diff2070_list[[1]][[1]], top="DMI"),
#                     arrangeGrob(plot_mean_diff2021_list[[1]][[5]], plot_mean_diff2070_list[[1]][[5]], top="SMHI"),
#                     arrangeGrob(plot_mean_diff2021_average_list[[1]], plot_mean_diff2070_average_list[[1]], top="Average"),
#                     ncol = 3)
# # str(plot_mean_1971_list,2)
# # plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg 

## Arrage with ggarrrange (with common legend) ##
plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg  <- 
  ggarrange(
    plot_mean_diff2021_list[[1]][[1]], plot_mean_diff2021_list[[1]][[5]], plot_mean_diff2021_average_list[[1]],
    plot_mean_diff2070_list[[1]][[1]], plot_mean_diff2070_list[[1]][[5]], plot_mean_diff2070_average_list[[1]],
    labels = c("a1)", "b1)", "c1)", "a2)", "b2)", "c2)"), font.label = list(size = 10, color = "black", face = "plain", family = NULL), vjust = 1.7, 
          common.legend = TRUE, legend = "right")


ExVar = 11 
print(list_variableName[[ExVar]])

# DMI <-
#   ggarrange(plot_diff2021_list_notitle_nolegend[[ExVar]][[1]], plot_diff2070_list_notitle_nolegend[[ExVar]][[2]], labels = c("a1", "a2"), 
#             ncol=1, nrow=2 )  %>%
#   ggexport(filename = paste("./figures/figures_exploratory/Proj/Combined/","DMI",".png", sep=""), 
#            width=800, height=550)
plot_diff2021_list_notitle_legend[[ExVar]][[1]]
 DMI_annotated <-  annotate_figure(  ggarrange(plot_diff2021_list_notitle_nolegend[[ExVar]][[1]], plot_diff2070_list_notitle_nolegend[[ExVar]][[2]], labels = c("a1)", "a2)"),
                                               ncol=1, nrow=2) , top = text_grob("DMI", color = "black", face = "bold", size = 20, family= " Arial"))
 SMHI_annotated <- annotate_figure(  ggarrange(plot_diff2021_list_notitle_nolegend[[ExVar]][[5]], plot_diff2070_list_notitle_nolegend[[ExVar]][[5]], labels = c("b1)", "b2)"),
                                               ncol=1, nrow=2 ) , top = text_grob("SMHI", color = "black", face = "bold", size = 20, family= " Arial"))
 Average_annotated <- annotate_figure(  ggarrange(plot_diff2021_list_notitle_legend[[ExVar]][[6]], plot_diff2070_list_notitle_legend[[ExVar]][[6]], labels = c("c1)", "c2)"),
                                                  ncol=1, nrow=2 , 
                                                  common.legend = TRUE, legend = "right") , top = text_grob("Avg. of RCMs", color = "black", face = "bold", size = 20, family= " Arial"))
plot_diff2021_list_title_legend[[ExVar]][[5]]
plot_diff2070_list_title_legend[[ExVar]][[5]]
plot_diff2021_list_title_legend[[ExVar]][[6]]
plot_diff2070_list_title_legend[[ExVar]][[6]]

plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg  <- 
  ggarrange( DMI_annotated, SMHI_annotated ,  Average_annotated , ncol=3, nrow = 1, 
    common.legend = TRUE, legend = "right", align ="v")


# plot_diff2021_list_title_legend[[ExVar]][[1]], plot_diff2021_list_title_legend[[ExVar]][[5]], plot_diff2021_list_title_legend[[ExVar]][[6]],
# plot_diff2070_list_title_legend[[ExVar]][[1]], plot_diff2070_list_title_legend[[ExVar]][[5]], plot_diff2070_list_title_legend[[ExVar]][[6]],
# labels = c("a1)", "b1)", "c1)", "a2)", "b2)", "c2)"), font.label = list(size = 10, color = "black", face = "plain", family = NULL), vjust = 1.7,
## Annotate the arranged figure ##
plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg_annoted <- annotate_figure(plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg ,
                                                                        top = text_grob("Yield Anomaly", color = "black", face = "bold", size = 20) )
## Export the arranaged and annotated figure ##
# annotate_figure(plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg ,
#                 top = text_grob("Yield Anomaly", color = "black", face = "bold", size = 20) ) %>% 
plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg  %>%
  ggexport(filename = paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg",".png", sep=""), 
           width=800, height=550)

# ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg ",".pdf", sep=""), 
#        plot = plot_mean_yield_SMI_6_Jun_Aug_ExtremesAndAvg_annoted, width=28, height=24)


# ## _SMI_6_Jul_ ##                                        
# plot_mean_yield_SMI_6_Jul_ICTP_MPI_Av <- grid.arrange(arrangeGrob(plot_mean_1971_list[[2]][[2]], plot_mean_diff2021_list[[2]][[2]], plot_mean_diff2070_list[[2]][[2]], top="ICTP"),
#                                                           arrangeGrob(plot_mean_1971_list[[2]][[4]], plot_mean_diff2021_list[[2]][[4]], plot_mean_diff2070_list[[2]][[1]], top="MPI"),
#                                                           arrangeGrob(plot_mean_1971_average_list[[2]], plot_mean_diff2021_average_list[[2]],
#                                                                       plot_mean_diff2070_average_list[[2]],
#                                                                       top="Average"), ncol = 3)
# str(plot_mean_1971_list,2)
# plot_mean_yield_SMI_6_Jul_ICTP_MPI_Av

# ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jul_ICTP_MPI_Av",".pdf", sep=""), 
#        plot = plot_mean_yield_SMI_6_Jul_ICTP_MPI_Av , width=28, height=24)

################################################################################
#### Combined Plots of the second climate period (2070 - 2099) for each RCM ####
################################################################################

#### Jul P, Jul T, June SMI, Jul SMi, Aug SMI  ####
namelist_RCMs <- c("MPI","DMI","KNMI","ICTP","SMHI")

## Define function to loop through ##
ExVar_arrange_notitle_legend <- function( ExVar ){ggarrange(plot_diff2070_list_notitle_legend[[ExVar]][[1]] ,
                                             plot_diff2070_list_notitle_legend[[ExVar]][[2]] ,
                                             plot_diff2070_list_notitle_legend[[ExVar]][[3]] ,
                                             plot_diff2070_list_notitle_legend[[ExVar]][[4]] ,
                                             plot_diff2070_list_notitle_legend[[ExVar]][[5]] ,
                                             common.legend = TRUE, legend = "bottom",  ncol = 1, nrow=5)}


rm(ExVar)
ExVar_list <- lapply(1:18, ExVar_arrange_notitle_legend)

# plot_diff2070_list_title_legend[[1]][[1]]
JulyTmp_annttd <- annotate_figure(ExVar_list[[1]],
                top = text_grob("July Temperature", color = "black", face = "bold", size = 14))

# plot_diff2070_list_title_legend[[2]][[1]]
JulyPrec_annttd <- annotate_figure(ExVar_list[[2]],
                                  top = text_grob("July Precipitation", color = "black", face = "bold", size = 14))

# plot_diff2070_list_title_legend[[3]][[1]]
JuneSMI_annttd <- annotate_figure(ExVar_list[[3]],
                                   top = text_grob("June SMI", color = "black", face = "bold", size = 14)) 

# plot_diff2070_list_title_legend[[4]][[1]]
JulySMI_annttd <- annotate_figure(ExVar_list[[4]],
                                   top = text_grob("July SMI", color = "black", face = "bold", size = 14))

# plot_diff2070_list_title_legend[[5]][[1]]
AugSMI_annttd <- annotate_figure(ExVar_list[[5]],
                                   top = text_grob("August SMI", color = "black", face = "bold", size = 14))

# plot_diff2070_list_title_legend[[11]][[1]]
Yield_SMIJunAug_MetJul_annttd <- annotate_figure(ExVar_list[[11]],
                                 top = text_grob("Yield Anomaly", color = "black", face = "bold", size = 14))


rm(nested)
nested <- ggarrange(JuneSMI_annttd , JulyTmp_annttd, JulyPrec_annttd , AugSMI_annttd , Yield_SMIJunAug_MetJul_annttd, ncol = 5, nrow=1)
nested  %>% 
  ggexport(filename = paste("./figures/figures_exploratory/Proj/Combined/","structural",".png", sep=""), 
           width=1000, height=1200)

# plot_structure2070 <- 
# grid.arrange(arrangeGrob(plot_mean_diff2070_PJul_list[[1]], plot_mean_diff2070_TJul_list[[1]], plot_mean_diff2070_SMIJun_list[[1]],
#                          plot_mean_diff2070_SMIJul_list[[1]], plot_mean_diff2070_SMIAug_list[[1]], plot_mean_diff2070_list[[1]][[1]], 
#                          plot_mean_diff2070_list[[2]][[1]], left="MPI", nrow = 1),
#              arrangeGrob(plot_mean_diff2070_PJul_list[[2]], plot_mean_diff2070_TJul_list[[2]], plot_mean_diff2070_SMIJun_list[[2]],
#                          plot_mean_diff2070_SMIJul_list[[2]], plot_mean_diff2070_SMIAug_list[[2]], plot_mean_diff2070_list[[1]][[2]], 
#                          plot_mean_diff2070_list[[2]][[2]], left="DMI", nrow = 1),
#              arrangeGrob(plot_mean_diff2070_PJul_list[[3]], plot_mean_diff2070_TJul_list[[3]], plot_mean_diff2070_SMIJun_list[[3]],
#                          plot_mean_diff2070_SMIJul_list[[3]], plot_mean_diff2070_SMIAug_list[[3]], plot_mean_diff2070_list[[1]][[3]], 
#                          plot_mean_diff2070_list[[2]][[3]],left ="KNMI", nrow = 1),
#              arrangeGrob(plot_mean_diff2070_PJul_list[[4]], plot_mean_diff2070_TJul_list[[4]], plot_mean_diff2070_SMIJun_list[[4]],
#                          plot_mean_diff2070_SMIJul_list[[4]], plot_mean_diff2070_SMIAug_list[[4]], plot_mean_diff2070_list[[1]][[4]], 
#                          plot_mean_diff2070_list[[2]][[4]],left ="ICTP", nrow = 1),
#              arrangeGrob(plot_mean_diff2070_PJul_list[[5]], plot_mean_diff2070_TJul_list[[5]], plot_mean_diff2070_SMIJun_list[[5]],
#                          plot_mean_diff2070_SMIJul_list[[5]], plot_mean_diff2070_SMIAug_list[[5]], plot_mean_diff2070_list[[1]][[5]],  
#                          plot_mean_diff2070_list[[2]][[5]], left ="SMHIRCA", nrow = 1),
#                         nrow=5)    
# 
# plot_structure2070

ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_structure2070",".pdf", sep=""), 
       plot = plot_structure2070 , width=48, height=30)

# ################################################################################################################################################################################################
# ################################################################################################################################################################################################

# ################################################################################################################################################################################################
# #### Make combined Plots of SDs ####
# ################################################################################################################################################################################################
# '- Combined Plots
# - TJul, PJul, SMIJun, SMIAug
# - TJul, PJul, SMIJun, SMIAug, Yield
# - TJul, PJul, SMIJul
# - TJul, PJul, SMIJul, Yield'
# 
# 'For the yield plots it is necessary to load plot_sd_diff...list via the BasePdiction_Plots Script. '
# 
# 
# #######################################################
# #### Sd plots for TJul, PJul, SMIJun, SMIAug ####
# 
# plot_sd_SMI_6_Jun_Aug <- grid.arrange(plot_sd_1970_TJul , plot_sd_1970_PJul,  plot_sd_1970_SMIJun, plot_sd_1970_SMIAug,
#                                       plot_sd_diff2021_TJul, plot_sd_diff2021_PJul, plot_sd_diff2021_SMIJun, plot_sd_diff2021_SMIAug,
#                                       plot_sd_diff2070_TJul, plot_sd_diff2070_PJul, plot_sd_diff2070_SMIJun, plot_sd_diff2070_SMIAug,
#                                       ncol=4, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=30)))
# # plot_sd_SMI_6_Jun_Aug
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jun_Aug_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_sd_SMI_6_Jun_Aug , width=28, height=24)
# 
# # ##############################################################
# # #### Sd plots for TJul, PJul, SMIJun, SMIAug, Yield ####
# # plot_sd_yield_SMI_6_Jun_Aug <- grid.arrange(plot_sd_1970_TJul , plot_sd_1970_PJul,  plot_sd_1970_SMIJun, plot_sd_1970_SMIAug,plot_sd_1971_list[[1]][[l]],
# #                                             plot_sd_diff2021_TJul, plot_sd_diff2021_PJul, plot_sd_diff2021_SMIJun, plot_sd_diff2021_SMIAug, plot_sd_diff2021_list[[1]][[l]],
# #                                             plot_sd_diff2070_TJul, plot_sd_diff2070_PJul, plot_sd_diff2070_SMIJun, plot_sd_diff2070_SMIAug, plot_sd_diff2070_list[[1]][[l]],
# #                                             ncol = 5, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=25)))
# # plot_sd_yield_SMI_6_Jun_Aug
# # 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jun_Aug_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jun_Aug , width=35, height=24)
# # 
# ###############################################
# #### Sd plots for TJul, PJul, SMIJul ####
# plot_sd_SMI_6_Jul <- grid.arrange(plot_sd_1970_TJul , plot_sd_1970_PJul,  plot_sd_1970_SMIJul,
#                                   plot_sd_diff2021_TJul, plot_sd_diff2021_PJul, plot_sd_diff2021_SMIJul,
#                                   plot_sd_diff2070_TJul, plot_sd_diff2070_PJul, plot_sd_diff2070_SMIJul,
#                                   ncol=3, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=30)))
# # plot_sd_SMI_6_Jul
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jul_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_sd_SMI_6_Jul , width=28, height=24)
# 
# # ######################################################
# # #### Sd plots for TJul, PJul, SMIJul, Yield ####
# # plot_sd_yield_SMI_6_Jul <- grid.arrange(plot_sd_1970_TJul , plot_sd_1970_PJul,  plot_sd_1970_SMIJul, plot_sd_1971_list[[2]][[l]],
# #                                         plot_sd_diff2021_TJul, plot_sd_diff2021_PJul, plot_sd_diff2021_SMIJul,  plot_sd_diff2021_list[[2]][[l]],
# #                                         plot_sd_diff2070_TJul, plot_sd_diff2070_PJul, plot_sd_diff2070_SMIJul, plot_sd_diff2070_list[[2]][[l]],
# #                                         ncol = 4, top=textGrob(paste(namelist_RCMs[[l]]),gp=gpar(fontsize=20)))
# # plot_sd_yield_SMI_6_Jul
# # 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jul_", namelist_RCMs[[l]],".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jul , width=28, height=24)

