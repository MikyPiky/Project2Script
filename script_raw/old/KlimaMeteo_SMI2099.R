## Descriptiom ##

'
Check SMI Values because of pecularities for the year 2099, in particular SMHI and ICTP

'

## Input ##

'
- "MeteoMonth_df_tidy_*.csv <- from KlimaMeteo_netcdf_to_sf&Tidy (reshaped climate data from wide to tidy)

'

## Output ##

' MeteoMeonth_df_tidy_19712000_demeaned_*.csv -> ./data/data_proj/ '

#### Packages ####
source("./script/script_raw/Packages.R")



#########################################################################################################################################################################################
#########################################################################################################################################################################################
source("./script/script_raw/BaseModel.R")

###########################################################################
#### Loop through climate models to make maps of SMI for the year 2099 ####
for (i in 1:length(namelist_RCMs)){
  
  ####################
  #### Load data ####
  ##################
  Climate <- read_csv(paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_RCMs[[i]],".csv", sep=""))
  Climate$comId <- as.integer(Climate$comId)
  # View(Climate)
  Climate
  
  #############################################################
  ## Filter for comId not represented for in training data ####
  Climate_filter  <- Climate %>% filter(comId != c(2000,11000))
  Climate_filter
  # View(Climate_filter)
  
  ###################################################################################
  #### Filter for comIds for which no data exist because of no agricultural land ####
  noAgri <- c(9780, 9776, 9763, 9180)
  Climate_filter <- Climate_filter %>% filter(!comId %in% noAgri)
  # filter(comId != c(9780)) %>% filter(comId != c(9776)) %>% filter(comId != c(9763)) %>% filter(comId != c(9180))
  
  #########################
  #### Only select SMI ####
  Climate_filter <- Climate_filter  %>% select(comId,year, SMI_Jan:SMI_Dec)

  
  ################################################################
  #### Check the distribution of the zero and one SMI values ####
  ##############################################################
  Climate_filter_one <- Climate_filter  %>% filter_at(vars(SMI_Jan:SMI_Dec), any_vars(. == 1))
  Climate_filter_zero <- Climate_filter  %>% filter_at(vars(SMI_Jan:SMI_Dec), any_vars(. == 0))
  Climate_filter_one_table  <- table(Climate_filter_one$comId, Climate_filter_one$year)
  Climate_filter_zero_table <- table(Climate_filter_zero$comId, Climate_filter_zero$year)
  
  
  write_csv( Climate_filter_one, path=paste("./figures/figures_exploratory/Proj/SMI/one_", namelist_RCMs[[i]],".csv", sep=""  ) )
  write_csv( Climate_filter_zero, path=paste("./figures/figures_exploratory/Proj/SMI/zero_", namelist_RCMs[[i]],".csv", sep=""  ) )
  write.csv( Climate_filter_one_table,  file=paste("./figures/figures_exploratory/Proj/SMI/one_table_", namelist_RCMs[[i]],".txt", sep=""  ))
  write.csv( Climate_filter_zero_table, file=paste("./figures/figures_exploratory/Proj/SMI/zero_table_", namelist_RCMs[[i]],".csv", sep=""  )) 
  
  pdf(paste("./figures/figures_exploratory/Proj/SMI/zeroAndOne_", namelist_RCMs[[i]],".pdf", sep=""  ))
  par(mfrow=c(2,1))
  plot(Climate_filter_one$comId ~ Climate_filter_one$year)
  plot(Climate_filter_zero$comId ~   Climate_filter_zero$year)
  dev.off()
  
#   ########################################################################
#   #### Check values of year 2099 because of pecularities found there ####
#   ######################################################################
#   Climate_filter_2099 <- Climate %>% filter(year==2099) %>% select(comId, SMI_Jan:SMI_Dec)
#   
#   
#   Climate_filter_2099_sf <- inner_join(vg2500_krs, Climate_filter_2099, by="comId") 
#   
#   # Climate_filter_2099_sf_select <-  Climate_filter_2099_sf %>% select(-USE, -GEN, - SHAPE_LENG, -SHAPE_AREA,  year)
#   
#   # Climate_filter_2099_sf_gather <- gather(Climate_filter_2099_sf_select,key=season, value=value, -comId )
#   
#   # View(Climate_filter_2099_sf_gather)
#   
#   myPalette <- colorRampPalette((brewer.pal(11, "RdYlGn")))
#   sc <- scale_fill_gradientn( colours = myPalette(100), limits=c(0.0000001, 0.9999999999999))
#   
#   plot_SMI_2099 <-list()
#   
#   season_list <- list("SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_May", "SMI_Jun", "SMI_Jul", "SMI_Aug", "SMI_Sep", "SMI_Oct", "SMI_Nov", "SMI_Dec")
#   
#   for (s in seq_along(season_list)){
#     plot_SMI_2099[[s]] <-
#       ggplot( Climate_filter_2099_sf) +
#       geom_sf(data=vg2500_krs, fill="gray", color="white")  +
#       geom_sf(aes_string(fill = season_list[[s]] )) +
#       sc +
#       theme_bw() +       theme(plot.title = element_text(hjust = 0.5))
#     
#   }
#   
#   plot_SMI_2099_all <-  grid.arrange(plot_SMI_2099[[1]], plot_SMI_2099[[2]], plot_SMI_2099[[3]], plot_SMI_2099[[4]], plot_SMI_2099[[5]], plot_SMI_2099[[6]], 
#                                      plot_SMI_2099[[7]], plot_SMI_2099[[8]], plot_SMI_2099[[9]], plot_SMI_2099[[10]], plot_SMI_2099[[11]], plot_SMI_2099[[12]], 
#                                      top=textGrob(paste(namelist_RCMs[[i]], sep=" & " ), gp=gpar(fontsize=30)), ncol=4)
#   
#   ggsave(paste("./figures/figures_exploratory/Proj/SMI/plot_SMI_2099_all", namelist_RCMs[[i]],".pdf", sep=""), 
#          plot =  plot_SMI_2099_all, width=20, height=16)
#   
}
