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

########################################
#### Load tidy table of yield Data  ####
yieldData <- read.table("../Proj1//data/data_raw/Train/Yield2015.csv", sep=";", dec=",", quote="\"", na.strings=(c("-","/", ".")),
                        col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape",
                                    "siloMaize"),   nrows=8925, skip=7, colClasses=c("integer","factor", "factor", rep("numeric", 10)))
# View(yieldData)
str(yieldData)
table(yieldData$year)

table(yieldData$comId , is.na(yieldData$siloMaize))



#########################################################################
#### Load SpatialPolygonesDataframe mit meteorological Daten ab 1995 #### 
MeteoMonth_df_tidy <- read.csv(paste("./data/data_processed/MeteoMonth_df_tidy",".csv", sep=""))
MeteoMonth_df_tidy$X <- NULL
str(MeteoMonth_df_tidy)
MeteoMonth_df_tidy$comId <- as.factor(str_pad(MeteoMonth_df_tidy$comId, width=5, side="left", pad = "0"))


#######################################
## Mergen der Meteorologischen Daten ##
#######################################

levels(MeteoMonth_df_tidy$comId)
levels(yieldData$comId)

YieldMeteo <- merge(yieldData, MeteoMonth_df_tidy, by=c("comId", "year"))
table(YieldMeteo$year) # 410
table(YieldMeteo$comId)


###################################################################################################################################################################################################
################################################################################# Manipulation of Yield Data ######################################################################################
###################################################################################################################################################################################################


##########################################
#### Delete zero values in yield Data ####
##########################################
## Here I delete all Yield with value 0, as this is not comprehensible
## Furthermore, the variable anomalies are to be explained when the plant is planted.
YieldMeteo$winterWheat[YieldMeteo$winterWheat==0] <- NA
YieldMeteo$rye[YieldMeteo$rye==0] <- NA
YieldMeteo$winterBarley[YieldMeteo$winterBarley==0] <- NA
YieldMeteo$oats[YieldMeteo$oats==0] <- NA
YieldMeteo$triticale[YieldMeteo$triticale==0] <- NA
YieldMeteo$potatoes[YieldMeteo$potatoes==0] <- NA
YieldMeteo$sugarBeet[YieldMeteo$sugarBeet==0] <- NA
YieldMeteo$winterRape[YieldMeteo$winterRape==0] <- NA
YieldMeteo$siloMaize[YieldMeteo$siloMaize==0] <- NA

##############################
#### Make state variables ####
##############################
## comIdState _ State IDs ##
# i.e. if comId starts with 1 give it a 01 and when it starts with 1 give it a 16 
YieldMeteo$comId

x <- YieldMeteo$comId
str_sub(x, -3, -1) <- "" ;x
table(x)
YieldMeteo$comIdState <- x

unique(YieldMeteo$comIdState)

####################################
## make names for state variables ##
names(YieldMeteo)

# make list of comStateIds und Namen
csIds_list<-list("comStateIds"=unique(YieldMeteo$comIdState), "comStateNames"= c("Schleswig-Holstein","Lower Saxony", "Bremen","NRW", "Hesse","Rhineland-Palatinate","Baden-Wurttemberg","Bavaria","Saarland","Brandenburg","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Thuringia" )) ; csIds_list


## Loop durch die Liste um  comStateNames den entsprechende comStateIds zuzuordnen
for (i in seq_along(csIds_list$comStateIds))
{
  YieldMeteo$comState[YieldMeteo$comIdState==csIds_list[[1]][i]] <- csIds_list[[2]][i]
  
}


## Check, whether adding names to comIdState worked
# table(YieldMeteo$comState=="Lower Saxony")
# table(YieldMeteo$comIdState=="03")
# # looks good
# unique(YieldMeteo$comState)
# unique(YieldMeteo$comIdState)
# head(YieldMeteo, 15)
# tail(YieldMeteo, 15)
# View(yieldData)



#########################
#### Reorder Columns ####
length(YieldMeteo)
names(YieldMeteo[, c(1:3,82,83, 4:81 )])
YieldMeteo <- YieldMeteo[, c(1:3,82,83, 4:81 )]


#####################################
#### Export tidy YieldMeteo Data #### 
write.csv(YieldMeteo,"../Proj1/data/data_processed/YieldMeteo.csv", row.names= FALSE )
write.csv(YieldMeteo,"../Proj2/data/data_processed/YieldMeteo.csv", row.names= FALSE )
