#### File Description ####
'
Get descriptive statistic for the certain periods of the input training data. The data are SMI_July to SMI_Aug.
'
#### Dependencies and Input ####
'
## MeteoMonth_train_tidy.csv <- "./data/data_processed/MeteoMonth_df_tidy" (Meteo_netcdf_to_tidy:r)
'

#### Ouput ####
'
  - data.frame of mean and median of subperiods
  "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],".csv"
  - Export Summary/ Descriptive Statistics of the SMI Data 
  "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.txt"
  "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.tex"
  "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Mean.tex"

'


#### Packages ####
source("./script/script_raw/Packages.R")


#############################################################################################################################################################################################
#############################################################################################################################################################################################



###############################################
#### Load tidy data.frame of training data #### 
MeteoMonth_train_tidy <- read_csv("./data/data_processed/Meteo_train_demean.csv")
MeteoMonth_train_tidy
unique(MeteoMonth_train_tidy$year)

#########################################
#### Select only SMI, Prec, and Tavg ####
MeteoMonth_train_tidy <- MeteoMonth_train_tidy %>% select(comId, year, SMI_Jun:SMI_Aug)

################################################################
#### Retrieve com spedcific means and medians of subperiods ####

namelist_periods <- c("1971_2000", "1999_2015", "1951_2015")
list_periods <- list(c(1971,1999,1951), c(2000,2015,2015))
list_periods[[2]][[1]]
noAgri <- c(9780, 9776, 9763, 9180)
#### Start of loop trough subperiods ####
for (i in 1:3){
  MeteoMonth_train_tidy_MeanMedian <- MeteoMonth_train_tidy %>%
    group_by(comId) %>%
    filter(!comId %in% noAgri ) %>% # filter for coms which have no agricultural area
    filter(year >= list_periods[[1]][[i]] & year <=  list_periods[[2]][[i]]) %>%
    select(comId, SMI_Jun:SMI_Aug) %>%
    summarise_all(funs(mean, median)) 

  
  # MeteoMonth_train_tidy_MeanMedian <- as.data.frame(MeteoMonth_train_tidy_MeanMedian)
  
  #### Write data.frame of meand and median of subperiods ####
  write_csv(MeteoMonth_train_tidy_MeanMedian, paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],".csv", sep=""))
  
  # View(MeteoMonth_train_tidy_MeanMedian)
  # summary(MeteoMonth_train_tidy_MeanMedian)
  
  
  #### Export Summary/ Descriptive Statistics of the SMI Data ####
  capture.output(summary(MeteoMonth_train_tidy_MeanMedian), file =  paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.txt", sep="") ) 
  stargazer(as.data.frame(MeteoMonth_train_tidy_MeanMedian), out =  paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.tex", sep="") )
  stargazer(as.data.frame(MeteoMonth_train_tidy_MeanMedian), out =  paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Mean.tex", sep="") ,
            summary.stat = c("mean"))
  

}
