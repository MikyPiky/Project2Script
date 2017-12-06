## Descriptiom ##

' This Script produces Summary Statistics (mean, median, min, max, sd) of Data (P,T,SMI) and
  yield anomalies (predicted via different prediction models) 
  derived from RCMs 
  for the periods i.e. 1951 - 2099, 1971 - 2000, 1999 - 2015, 2021 -2050, and 2071 - 2099. "

'

## Input ##

'
-  Climate_predicted.csv <- paste("./data/data_proj/output/", namelist_RCMs[[i]],"/Climate_predicted.csv", sep="")

'

## Output ##

'- paste("./figures/figures_exploratory/Proj/", namelist_RCMs[[i]],"/Summary_",namelist_periods[[l]],".txt", sep="")
'


#### Packages ####
source("./script/script_raw/Packages.R")

##################################################################################################################################################################################################
##################################################################################################################################################################################################
rm(list=ls())
source("./script/script_raw/BaseModel.R")
getwd()

######################################################################################
#### Check means of the administrative districts to validate the SMI climate data ####
######################################################################################

#### Preparation of loop ####
i=1

#### Start of loop through RCMs ####
for (i in 1:5){
  Climate_predicted <- read_csv(paste("./data/data_proj/output/", namelist_RCMs[[i]],"/Climate_predicted.csv", sep=""))
  Climate_predicted_summary  <- Climate_predicted %>% select(-(SMI_May6:SMI_Oct6)) %>% summary()
  Climate_predicted_summary 
  
  #### Preparation for loop through climate periods ####
  namelist_periods <- c("1951_2099", "1971_2000", "2021_2050", "2070_2099", "1999_2015")
  list_periods <- list(c(1951, 1971, 2021, 2070, 1999), c(2099, 2000,2050,2099, 2015))
  list_periods[[2]][[1]]
  noAgri <- c(9780, 9776, 9763, 9180)
  
  
  
  #### Start of loop trough climate periods - only SMI_Jun to SMI_Aug ####
  for (l in seq_along(namelist_periods)){
    #### Retriev com specific Mean of the SMI of subperiods ####
    Climate_predicted_subperiod <-  Climate_predicted %>%
      # select(comId, year, SMI_Jun:SMI_Aug) %>%
      # group_by(comId) %>%
      filter(!comId %in% noAgri) %>% # filter for coms which have no agricultural area
      filter(year >= list_periods[[1]][[l]] & year <=  list_periods[[2]][[l]])
    # %>%
      # select(comId, SMI_Jun:SMI_Aug) %>%
      # summarise_all(funs(mean, median))

    # View(Climate_predicted_subperiod)
    
    #### Export Descriptive Statistics of the SMI Data ####
    stargazer(as.data.frame(Climate_predicted_subperiod ),  out = paste("./figures/figures_exploratory/Proj/", namelist_RCMs[[i]],"/Summary_",namelist_periods[[l]],".txt", sep=""), median=T)
    stargazer(as.data.frame(Climate_predicted_subperiod ),  out = paste("./figures/figures_exploratory/Proj/", namelist_RCMs[[i]],"/Summary_",namelist_periods[[l]],".tex", sep=""), median=T)

  } # End of loop trough climate periods
} # End of loop trough RCMs

