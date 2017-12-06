#### File Description ####
'
# The goal is to merge the SMI and data of the regional statistics (= yield data) -> tidy data
  - Merge: Adjusting the comIds and matching is necessary because there are differences
  - The final record has only 410 spatial units per year: 
    Reasons for this:
      YieldData 525
      Kreis_SMI_spdf_1995: 412
        * Only five-digit ComIDs considered: 55 ComIds less
        * Landkreis Aachen: The Aachen district was dissolved in accordance with the Aachen Act by the end of October 20, 2009, and from the nine municipalities of Aachen and Aachen
                             Of the city of Aachen was formed as a new municipality association with the effect of October 21, 2009, the local authority of the city council Aachen
        * Circular reforms: Saxony (2008: 35 ComIds less) and Saxony-Anhalt (2007: 24 ComIds less)
    
      - Only Kreise und Kreisfreie cities are taken into account, ie every ComID with 2 or 3 digits is not considered.
  - Also produce state specific variables -> state and stateID 

'
#### Dependencies and Input ####
'
## Yield2015.csv #
  # GENESIS-Tabelle: 115-46-4;Hektarerträge ausgewählter landwirtschaftlicher Feldfrüchte- Jahressumme - regionale Tiefe: Kreise und krfr. Städte; Erntestatistik; 
  # Hektarerträge (dt/ha); dezitone/hectar = 100 kg/10000 m2
  # Jahre: 1999 - 2015

  # "year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"
  # names used in original file: Winterweizen  Roggen und Wintermenggetreide	Wintergerste	Sommergerste	Hafer	Triticale	Kartoffeln	Zuckerrüben	Winterraps	Silomais
  
  # Begriffsinhalt Erntertrag:
      Die Ertragsschätzungen erfolgen von fachkundigen und mit den speziellen Verhältnissen ihres Betriebes bzw. ihres
      Berichtsbezirks gut vertrauten Berichterstatterinnen und Berichterstattern. Als Berichterstatterinnen und
      Berichterstatter sind vielfach Leiterinnen oder Leiter landwirtschaftlicher Betriebe tätig. Bei Getreide,
      Kartoffeln und Raps erfolgen zusätzlich objektive Ertragsmessungen im Rahmen der "Besonderen Ernte- und
      Qualitätsermittlung". Der "Besonderen Ernte- und Qualitätsermittlung" liegt ein mathematisches
      Stichprobenverfahren zu Grunde, das auf die sehr genaue Bestimmung des im Landesdurchschnitt erzielten Ertrags
      ausgerichtet ist; die Messungen erfolgen dabei auf Flächeneinheiten, die mit Hilfe des Stichprobenverfahrens
      repräsentativ ausgewählt wurden. 
      Eine Dezitonne (dt) entspricht 100 kg.
  
  # Achtung bei Barley, da ich was die Flächendaten angeht teilweise nur Gesamtbarley habe
  # goal: dependent variabels for production function
  # important variables: Da die Erträge nicht absolut angeben sind, sondern in Ertrag pro Hektar, kann man die einzelen regionalen Einheiten vergleichen. Darüber hinaus 
    kann man sie wohl als Produktivität in diesem Jahr interpretieren.

## MeteoMonth_df_tidy.csv <- "./data/data_processed/MeteoMonth_df_tidy" (Meteo_netcdf_to_tidy:r)
'

#### Ouput ####
'
YieldMeteo (also needed in second Project on climate projections to train the data)
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
library(zoo)
library(foreign)
# library(maps)
library(colorspace)
library(lattice)
library(stringr)
library(DataCombine)
library(reshape2)
library(sf)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(grid)
library(stargazer)
library(stringr)

#############################################################################################################################################################################################
#############################################################################################################################################################################################
rm(list=ls())
########################################
#### Load tidy table of yield Data  ####
# yieldData <- read.table("../Proj1//data/data_raw/Train/Yield2015.csv", sep=";", dec=",", quote="\"", na.strings=(c("-","/", ".")),
#                         col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape",
#                                     "siloMaize"),   nrows=8925, skip=7, colClasses=c("integer","factor", "factor", rep("numeric", 10)))
# 

yieldData <- read_csv("data/data_raw/Yield2016_4.csv", 
                       col_names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape",
                                                                                   "siloMaize"), 
                       skip=1, na = c("-","/", ".", "...", "0")) ## also define 0 as NA
yieldData


#### Drop first row - cumulated datafor Germany (DG) ####
yieldData <- yieldData %>% filter(comId != "DG") 

yieldData$comId <- as.integer(yieldData$comId)

#########################################################################
#### Load SpatialPolygonesDataframe mit meteorological Daten ab 1995 #### 
MeteoMonth_df_tidy <- read_csv("./data/data_processed/Meteo_train_demean.csv", na = c("0")) ## also define 0 as NA, no 0 found in SMI data
names(MeteoMonth_df_tidy)

#######################################
## Mergen der Meteorologischen Daten ##
#######################################
YieldMeteo <- inner_join(yieldData, MeteoMonth_df_tidy, by=c("comId", "year"))
table(YieldMeteo$year) # 410
table(YieldMeteo$comId)

# View(YieldMeteo)

#### Delete merged data.frames ####
rm(yieldData, MeteoMonth_df_tidy)

###################################################################################################################################################################################################
################################################################################# Manipulation of Yield Data ######################################################################################
###################################################################################################################################################################################################


##########################################
#### Delete zero values in yield Data ####
##########################################
## Here I delete all Yield with value 0, as this is not comprehensible
## Furthermore, the variable anomalies are to be explained when the plant is planted.
# YieldMeteo$winterWheat[YieldMeteo$winterWheat==0] <- NA
# YieldMeteo$rye[YieldMeteo$rye==0] <- NA
# YieldMeteo$winterBarley[YieldMeteo$winterBarley==0] <- NA
# YieldMeteo$oats[YieldMeteo$oats==0] <- NA
# YieldMeteo$triticale[YieldMeteo$triticale==0] <- NA
# YieldMeteo$potatoes[YieldMeteo$potatoes==0] <- NA
# YieldMeteo$sugarBeet[YieldMeteo$sugarBeet==0] <- NA
# YieldMeteo$winterRape[YieldMeteo$winterRape==0] <- NA
# YieldMeteo$siloMaize[YieldMeteo$siloMaize==0] <- NA

##############################
#### Make state variables ####
##############################
## stateID _ State IDs ##
# i.e. if comId starts with 1 give it a 01 and when it starts with 1 give it a 16 
YieldMeteo$comId

x <- YieldMeteo$comId
str_sub(x, -3, -1) <- "" ;x
table(x)
YieldMeteo$stateId <- x
YieldMeteo$state <- x # initialize state column for loop
 
unique(YieldMeteo$stateId)

####################################
## make names for state variables ##
names(YieldMeteo)

# make list of comStateIds und Namen
csIds_list <- list("stateId"=unique(YieldMeteo$stateId), 
                 "state"= c("Schleswig-Holstein","Lower Saxony", "Bremen","NRW", "Hesse","Rhineland-Palatinate","Baden-Wurttemberg","Bavaria","Saarland","Brandenburg","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Thuringia" )) ; csIds_list


## Loop durch die Liste um  comStateNames den entsprechende comStateIds zuzuordnen
for (i in 1:length(csIds_list$stateId)){
  YieldMeteo$state[YieldMeteo$stateId==csIds_list[[1]][i]] <- csIds_list[[2]][i]
}

## Check, whether adding names to stateID worked
table(YieldMeteo$state)
table(YieldMeteo$stateId)

# # looks good
# unique(YieldMeteo$comState)
# unique(YieldMeteo$stateID)

# View(YieldMeteo)



#########################
#### Reorder Columns ####
length(YieldMeteo)
names(YieldMeteo)
names(YieldMeteo[, c(1:3,138:139, 4:137 )])
YieldMeteo <- YieldMeteo[, c(1:3,138:139, 4:137 )]
unique(YieldMeteo$year)
head(YieldMeteo, 15)
tail(YieldMeteo, 15)


#####################################
#### Export tidy YieldMeteo Data #### 
write_csv(YieldMeteo,"../Proj2/data/data_processed/YieldMeteo.csv")

rm(list=ls())
