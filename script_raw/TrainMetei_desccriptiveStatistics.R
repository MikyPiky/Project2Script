#### File Description ####
'
Get descriptive statistic for the certain periods of the input training data.
'
#### Dependencies and Input ####
'
## MeteoMonth_train_tidy.csv <- "./data/data_processed/MeteoMonth_df_tidy" (Meteo_netcdf_to_tidy:r)
'

#### Ouput ####
'
YieldMeteo (also needed in second Project on climate projections to train the data)
write.csv(YieldMeteo,"../Proj1/data/data_processed/YieldMeteo.csv", row.names= FALSE )
write.csv(YieldMeteo,"../Proj2/data/data_processed/YieldMeteo.csv", row.names= FALSE )

'


#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
# library(reshape)
library(stringr)
# library(eeptools)
library("ggplot2")
library("foreign")
library("plm")
library("car")
library("lmtest")
library("lattice")
library("zoo")
library("scales")
library("nlme")
library("lme4")
library("mgcv")
# library("apsrtable")
# library("texreg")
library("DataCombine")
library("reshape2")
# library("pracma")


#############################################################################################################################################################################################
#############################################################################################################################################################################################



#########################################################################
#### Load tidy data.frame of training data #### 
MeteoMonth_train_tidy <- read.csv(paste("./data/data_processed/MeteoMonth_df_tidy",".csv", sep=""))
MeteoMonth_train_tidy$X <- NULL
str(MeteoMonth_train_tidy)
MeteoMonth_train_tidy$comId <- as.factor(str_pad(MeteoMonth_train_tidy$comId, width=5, side="left", pad = "0"))

#########################################
#### Select only SMI, Prec, and Tavg ####
MeteoMonth_train_tidy <- MeteoMonth_train_tidy %>% select(comId, year, SMI_Jun:SMI_Aug)

################################################################
#### Retrieve com spedcific means and medians of subperiods ####

namelist_periods <- c("1971_2000", "1999_2015", "1951_2015")
list_periods <- list(c(1971,1999,1951), c(2000,2015,2015))
list_periods[[2]][[1]]

for (i in 1:3){
MeteoMonth_train_tidy_MeanMedian <- MeteoMonth_train_tidy %>%
  group_by(comId) %>%
  filter(comId != ("09180") & comId != ("09780") & comId != ("09776") & comId != ("09763")) %>% # filter for coms which have no agricultural area
  filter(year >= list_periods[[1]][[i]] & year <=  list_periods[[2]][[i]]) %>%
  summarise_all(funs(mean, median))

write.csv(MeteoMonth_train_tidy_MeanMedian, paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],".csv", sep=""))

# View(MeteoMonth_df_tidy_MeanMedian_subperiod )

#### Export Summary/ Descriptive Statistics of the SMI Data ####
capture.output( summary(MeteoMonth_train_tidy_MeanMedian), file =  paste("./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.txt", sep="") ) 
}
