#### Description ####
' This script serves to make plots of the soil moisture anomaly distributions dependent on the com. 
The reason is that the fixed effects appear rather large when only using SMI. Further, silage maize is demeaned differently for each comId, since the number of observation diverge. 
Basically, this is interesting to figure out the effect of transferring the fixed effects from 
'

#### Input ####
Maize_meteo <- read.csv( file="./data/data_processed/Maize_meteo.csv")


#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(dplyr)
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
library(stargazer)
library(Hmisc)
library(ggplot2)


##################################################################################################################################################################################################
##################################################################################################################################################################################################


Maize_meteo <- read.csv( file="./data/data_processed/Maize_meteo.csv")
# Maize_meteo$X <- NULL
str(Maize_meteo)


p <- ggplot(Maize_meteo, aes (comId,siloMaizeAnomaly))
p + geom_boxplot(aes(colour = factor(comId))) + theme(legend.position="none")
+ geom_smooth(aes(colour = factor(comId)),se = F,method =  "lm")


