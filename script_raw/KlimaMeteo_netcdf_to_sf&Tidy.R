#### Description of Script ####
'
- Process Data from climate projection runs based on 5 RGM ("DMI","ICTP","KNMI","MPI","SMHI" - the driver is one climate model)
- Data are scaled on administrative districts by a Fortran routine
  - Stephans Fortran Program
    - Data are derived on (4*4 km)
    - Masked for non-irrigated agricultural land  (CLC06, class 12, 100*100 m)  
    - Data are agregated on polygons of administrative districts (according to a weighting matrix
    - Soil moisture is transformed into SMI

  - Loop which 
   a) produces data.frames (csv) for each variables Pre, Tavg, and SMI 
    a1) reads in the netcdf output for Pre, Tavg, and SMI for the three regional climate models used (MPI, DMI, KNMI, ICTP, SMHI)
    a2) Extract to data.frame with the same time length
    a3) Export csv dataframes for each RCM

  - Loop which
     b) Reshape Spatial Data to Tidy Data to allow to fit the models
     b1) Combine Data of each Variable to one large, wide data.frame 
     b2) Combina Data with spatial information (vg2500_krs) to generate sf.data.frame (including spatial information)
     b3) Preperation for Reshaping (Generating list including the years of each Meteo Month combination (list_MeteoMonthYear) 
     and list of new variable names (list_MeteoMonthNames))
     b4) Loop through all 36 Meteo Month Combinations to melt all years considered into one column 
     b5) Add Spatial and Temporal Information to newly created data.frame
     b6) Change strings which are necessary for merging 
     b7) Produce lagged variables of SMI and the meteorological variables for the months after the harvest
     b8) Write newly created MeteoMonth_df_tidy 


'
#### Output ####
## Files
' - Tidy data.frame (nach reshape) <- /data/data_proj/MeteoMonth_df_tidy_*.csv 
  '


#### Dependencies and input ####
'- Meterological and SMI Data derived from the climate models ("DMI","ICTP","KNMI","MPI","SMHI") -> data_proj/input
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
library(xtable)


########################################################################################################################################################################################
########################################################################################################################################################################################
#### Loop which 
#### a) produces data.frames (csv) for each variables Pre, Tavg, and SMI 
#### a1) reads in the netcdf output for Pre, Tavg, and SMI for the three regional climate models used (MPI, DMI, KNMI, ICTP, SMHI)
#### a2) Extract to data.frame with the same time length
#### a3) Appends data.frame on SpatialPolyonDataFrame with Spatial Information (vg2500_krs)
#### a4) Export csv dataframes for each input Variable seperately ( It is not possible to read in one large data.frame combining all information ) (Output Size = 200 MB)

##############################################################################################
#### Load Shape with SpatialInformation for the Polygones of the Administrative Districts ####
vg2500_krs <- read_sf("./data/data_proj/input/CLC/", "vg2500_krs")
str(vg2500_krs, 2)
vg2500_krs$RS

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS


###############################
## Make namelist for loop ####
namelist_var <- c("mSMI", "pre_lk", "tavg_lk")

namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")



#######################
#### Start of Loop ####

for (i in 1:5){
## i relates to each of the five lists incorporating the input data for each modell  
  print("_______________________________________________________________________________________________________________")
  print(namelist_models[i])
##################################################################    
#### Read in NetCDF with data (Tavg, Pre, SMi) from the RCMs ####   
################################################################

###################################  
## Directories to load data from ##
SMI_directory <- paste("./data/data_proj/input/", namelist_var[1],"_", namelist_models[i], ".nc", sep="")
Pre_directory <- paste("./data/data_proj/input/", namelist_var[2],"_", namelist_models[i], ".nc", sep="")
Tav_directory <- paste("./data/data_proj/input/", namelist_var[3],"_", namelist_models[i], ".nc", sep="")

####################
#### Load NETCDF ###
SMI_open <- nc_open(SMI_directory)
Pre_open <- nc_open(Pre_directory)
Tav_open <- nc_open(Tav_directory)

print(SMI_open)
print(Pre_open)
print(Tav_open)
# Hier habe ich 1800 Monate seit Januar 1951

SMI <- ncvar_get(SMI_open, varid="SMI")
Pre <- ncvar_get(Pre_open, varid="pre_lk")
Tav <- ncvar_get(Tav_open, varid="tavg_lk")

nc_close(SMI_open)
nc_close(Pre_open)
nc_close(Tav_open)


##########################################################
#### Extract to data.frame with the same time length ####
########################################################
Pre <- as.data.frame(Pre[1:412, 1:1788])
print(dim(Pre))
# (Pre[1:412, 1:1788])
Tav <- as.data.frame(Tav[1:412, 1:1788])
print(dim(Tav))
# (Tav[1:412, 1:1788])
SMI <- as.data.frame(SMI[1:412,1:1788])

print(dim(SMI))
# (SMI[1:412, 1:1788])
'The start data provided in the header are different for SMI compared to the other variables:
SMI_MPI: 1951, SMI_DMI:1951, SMI_KNMI: 1951, SMI_ICTP: 1951, SMI_SMHI: 1951; Rest 1950
But also the length of the calculation differ:
MPI:1800, DMI:1788, KNMI: 1800, ICTP: 1800, SHMIRCA: 1800

Since it can be assumed that all simulations need to include the year 2099, the only reasonable starting data for all the data is then 1951, because otherwise there
are too few years in this model (149 years needed).

Thus, we cut all the data for 149 years, i.e. 149*12 = 1788 time steps with the assumption that the starting year is 1951. 

'

 # }
summary(SMI$V1) 
summary(Pre$V1)
summary(Tav$V1) 


##########################################
#### Incorporate spatial information ####
########################################

#####################################
#### Make a list of data.frames ####
###################################
List_DataWide <- list(SMI, Pre, Tav)
names(List_DataWide) <- c("SMI", "P", "T")
str(List_DataWide,1)

###################################################################################################
#### Convert data.frame to matrix to be able to define NA values and take care of zero values ####
#################################################################################################
# SMI_m <- as.matrix(SMI)
# Pre_m <- as.matrix(Pre)
# Tav_m <- as.matrix(Tav)
# # dim(SMI_m)
# # head(SMI_m)
# # any(SMI_m == -9999)
# # any(SMI_m == 9999)
# 
# #### 0 Werte in NA ####
# NA_list <- c("SMI_m","Pre_m","Tav_m")
# i=3
# any(NA_list[[i]] == 0)
# u <- SMI_m == 0
# SMI_m[u] <- NA
# levelplot(SMI_m)


#######################
#### Change Names ####
#####################
## 1951 - 2099
idx_SM  <- make.names(c(gsub(" ","", paste("SMI",as.yearmon(seq(from=as.Date('1951-1-1'), to=as.Date('2099-12-1'), 'month')), sep=""))), unique = TRUE)
idx_Pre <- make.names(c(gsub(" ","", paste("P",as.yearmon(seq(from=as.Date('1951-1-1'), to=as.Date('2099-12-1'), 'month')), sep=""))), unique = TRUE)
idx_Tav <- make.names(c(gsub(" ","", paste("T",as.yearmon(seq(from=as.Date('1951-1-1'), to=as.Date('2099-12-1'), 'month')), sep=""))), unique = TRUE)

# ## 1971 - 2000
# idx_SM  <- make.names(c(gsub(" ","", paste("SMI",as.yearmon(seq(from=as.Date('1971-1-1'), to=as.Date('2000-12-1'), 'month')), sep=""))), unique = TRUE)
# idx_Pre <- make.names(c(gsub(" ","", paste("P",as.yearmon(seq(from=as.Date('1971-1-1'), to=as.Date('2000-12-1'), 'month')), sep=""))), unique = TRUE)
# idx_Tav <- make.names(c(gsub(" ","", paste("T",as.yearmon(seq(from=as.Date('1971-1-1'), to=as.Date('2000-12-1'), 'month')), sep=""))), unique = TRUE)

names(List_DataWide[[1]]) <- idx_SM
names(List_DataWide[[2]]) <- idx_Pre
names(List_DataWide[[3]]) <- idx_Tav

##################################################
#### Delete German Expression in Columnnames ####
################################################
for (a in 1:3)
{
names(List_DataWide[[a]]) <- chartr("ä","a",names(List_DataWide[[a]]))
names(List_DataWide[[a]]) <- chartr("z","c",names(List_DataWide[[a]]))
names(List_DataWide[[a]]) <- chartr("Mai","May",names(List_DataWide[[a]]))
names(List_DataWide[[a]]) <- chartr("Okt","Oct",names(List_DataWide[[a]]))
}


###########################################
#### Combine with Spatial Information ####
#########################################

# #########################
# #### Align Rownames #####
# ' Allways necessary when working with spCBind'
# rownames(List_DataWide[[1]]) <-rownames(List_DataWide[[2]]) <- rownames(List_DataWide[[3]]) <- 0:411


####################################################################
#### Make spatialpolygonsdataframe for each meterological input #### 
' Use of cbind to combine the data. Ich can use the sf.data.frame derived from the vg2500 shapefile. The spatial information there are the same 
  to those used in the Python Script employed by Stephan to extract the Spatial Information of SMI, Pre, and T. 
'

#####################################
#### Make one large Data.Frame  ####

# #### Compare data.frames ####
# summary(List_DataWide[[1]][500:520]) # SMI
# summary(List_DataWide[[2]][500:520]) # Pre
# summary(List_DataWide[[3]][500:520]) # Tav

#### Cbind data.frames ####
MeteoSMI_df_wide <-  cbind(List_DataWide[[1]], List_DataWide[[2]])
dim(MeteoSMI_df_wide)
MeteoSMI_df_wide <-   cbind(MeteoSMI_df_wide, List_DataWide[[3]])
dim(MeteoSMI_df_wide)
str(MeteoSMI_df_wide)
names(MeteoSMI_df_wide)

# ##############################################################################################
# #### Make one large SF.Data.Frame necessary to create tidy data.frame used in regression ####
# 
# #### Change RS to five digits #####
# vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
# vg2500_krs$RS
# dim(vg2500_krs)
# str(vg2500_krs)
# 
# #### Combine data with Spatial Information ####
# MeteoSMI_spdf_wide  <- bind_cols(vg2500_krs, MeteoSMI_df_wide)
# dim(MeteoSMI_spdf_wide )
# str(MeteoSMI_spdf_wide,1)

##################################
## Gain some experience with sf ##
# str(MeteoSMI_spdf_wide )
# attr(SMI_spdf , "sf_column")
# class(SMI_spdf)
# 
# print(SMI_spdf[9:15], n = 3)
# 
# plot(st_geometry(SMI_spdf))

## So kann ich mich mir expplizit die einzelnen ComIds anschauen ##
# plot(st_geometry(SMI_spdf)[[100]], col = 'red',add=T)
# 
# class(st_geometry(SMI_spdf))
# 
# st_bbox(SMI_spdf)
# st_crs(SMI_spdf) # st_transform

#########################################################
#### Export Data for each input Variable seperately ####
#######################################################
write.csv(MeteoSMI_df_wide,   paste("./data/data_proj/","Meteo_df_wide_", namelist_models[[i]],".csv", sep=""))

print("End of loop one")
} # Loop dauert nur 30 Sekunden, daher kann ich Daten eigentlich wieder löschen. Aber ich 

remove(MeteoSMI_df_wide, Pre, SMI, Tav)
remove(idx_Pre, idx_SM, idx_Tav)
remove(List_DataWide)
remove(Pre_open, SMI_open, Tav_open)
remove(Pre_directory, SMI_directory, Tav_directory)

######################################################################################################################################################################################
######################################################################################################################################################################################
##############################################################
#### Reshape Spatial Data to Tidy Data to fit the models ####
############################################################

#### Loop which
#### b) Reshape sSpatial Data to Tidy Data to allow to fit the models
#### b1) Combine Data of each Variable to one large, wide data.frame 
#### b2) Combina Data with spatial information (vg2500_krs) to generate sf.data.frame (including spatial information)
#### b3) Preperation for Reshaping (Generating list including the years of each Meteo Month combination (list_MeteoMonthYear) 
#### and list of new variable names (list_MeteoMonthNames))
#### b4) Loop through all 36 Meteo Month Combinations to melt all years considered into one column 
#### b5) Add Spatial and Temporal Information to newly created data.frame
#### b6) Change strings which are necessary for merging 
#### b7) Produce lagged variables of SMI and the meteorological variables for the months after the harvest
#### b8) Write newly created MeteoMonth_df_tidy 


'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt (Jahr). 
Da mein Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsprechend angepasst werden.
Soll heißen aus den SMI Variablen mit Monats und Jahres zuordnung wird nun ein SMI mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den SMI, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen.
Für dieses Vorgehen reshape ich den Kreis_SMI_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den SMI mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die SMI mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der SMI mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der SMI mit Monats Zuordnungen nach den Jahren differenziert.
'


###################################
## Make namelists for loop () ####
#################################
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")


#######################
#### Start of Loop ####

for (i in 1:5){
  
  ####################################################################################################
  #### Read data: first as df, then combine with spatial information derived from Kreis_shape (v) ####
  MeteoSMI_df_wide <- read_csv(paste("./data/data_proj/","Meteo_df_wide_",namelist_models[[i]],".csv", sep=""))
  MeteoSMI_df_wide$X1 <- NULL
  #### Remove wide data.frame from directory ####
  unlink(paste("./data/data_proj/","Meteo_df_wide_",namelist_models[[i]],".csv", sep=""), recursive = FALSE, force = FALSE)
  
  MeteoSMI_df_wide
  
  ##############################
  #### Create SF.data.frame ####
  MeteoSMI_spdf_wide  <-  bind_cols(vg2500_krs, MeteoSMI_df_wide )
  
  ####################################
  #### Preperation for Reshaping ####
  ##################################
  names(MeteoSMI_spdf_wide)
  (dim(MeteoSMI_spdf_wide)[2] - 6)
  
  #########################################################################################################################################
  #### Generate list of starting(year 1951) and ending points (2099) of each Meteo Month combination necessary for list_MeteoMonthYear ####
  
  ## Where is the first entry of a variable 
  StartingValue <- 7
  
  FirstVariableEntry <- c(seq(StartingValue, StartingValue + 11,1), (seq(StartingValue,StartingValue + 11,1) + 1788), (seq(StartingValue,StartingValue + 11,1) + 2*1788)) # 1788
  names(MeteoSMI_spdf_wide )[FirstVariableEntry]
  
  ## Last entry
  LastVariableEntry <- FirstVariableEntry + 148*12 # normalerweise 148*12
  names(MeteoSMI_spdf_wide )[LastVariableEntry]
  
  ## Combine entrys into one list
  list0 <- list(FirstVariableEntry, LastVariableEntry)
  
  
  #########################################################################################################################################################
  #### Loop to create a list (list_MeteoMonthYear) for each Meteo Combination (36) with all the variables names over the years considered (149) ####
  ## Container 
  list_MeteoMonthYear <- list()
  
  
  for (ii in 01:36){
    list_MeteoMonthYear[ii] <- list(c(names(MeteoSMI_spdf_wide )[c(seq(list0[[1]][ii], list0[[2]][ii], 12))]))
  }
  list_MeteoMonthYear
  ' Caveat: keep order in next list: 1:12 SMI, 13:24: Pre, 25:36:Tav'
  #########################################################################
  #### List of names for the new variables - Meteo Month Combination #####
  list_MeteoMonthNames <-list(c("SMI_Jan"), c( "SMI_Feb"), c("SMI_Mar"), c("SMI_Apr"), c("SMI_May"), c("SMI_Jun"),c("SMI_Jul"),c("SMI_Aug"),c("SMI_Sep"), c("SMI_Oct"), 
                       c("SMI_Nov"), c("SMI_Dec"),
                       c("P_Jan"), c( "P_Feb"), c("P_Mar"), c("P_Apr"), c("P_May"), c("P_Jun"),c("P_Jul"),c("P_Aug"),c("P_Sep"), c("P_Oct"), 
                       c("P_Nov"), c("P_Dec"),
                       c("T_Jan"), c( "T_Feb"), c("T_Mar"), c("T_Apr"), c("T_May"), c("T_Jun"),c("T_Jul"),c("T_Aug"),c("T_Sep"), c("T_Oct"), 
                       c("T_Nov"), c("T_Dec"))
  list_MeteoMonthNames
  
  
  ################################################################################################################
  ### Loop over the 36 listMonthYearNames to melt the years into one column for each Meteo Month combination ####
  ##############################################################################################################
  ## Make container for loop #
  MeteoMonth_df_tidy <- data.frame(1:61388) # set container , NORMALLY 412*149
  MeteoMonth <- data.frame() # set container
  # str(MeteoSMI_df_wide)
  
  str(MeteoSMI_spdf_wide )
  
  for(j in 1:36) {
    #### Melt all the observations (year 1951 to 2099) of each Meteo - Month Combination (listMonthYearNames with 36 items) into one column (MonthMeteo)
    MeteoMonth  <- melt(MeteoSMI_spdf_wide , id.vars="RS", measure.vars = list_MeteoMonthYear[[j]])
    ### Change the names 
    names(MeteoMonth)[3] <-  list_MeteoMonthNames[[j]]
    print(names(MeteoMonth))
    
    #### Add to a combined data.frame including all 36 Meteo - Moneth Combination
    MeteoMonth_df_tidy <- cbind(MeteoMonth_df_tidy,MeteoMonth[3]) # here I make a dataframe with the 36 newly created variables
    print(names(MeteoMonth_df_tidy))
  
  }
  
  
  head(MeteoMonth_df_tidy)
  head(MeteoMonth)
  
  ###########################################################################
  #### Add Spatial and Temporal Information to newly created data.frame #### 
  #########################################################################
  
  ## Combine data.frames
  MeteoMonth_df_tidy <- cbind(MeteoMonth[, 1:2], MeteoMonth_df_tidy)
  head(MeteoMonth_df_tidy)
  dim(MeteoMonth_df_tidy)
  MeteoMonth_df_tidy$X1.61388 <- NULL
  
  rownames(MeteoMonth_df_tidy) <- NULL
  
  
  #######################################################
  ## Change strings which are necessary for merging ####
  #####################################################
  colnames(MeteoMonth_df_tidy)[colnames(MeteoMonth_df_tidy)=="variable"] <- "year"
  colnames(MeteoMonth_df_tidy)[colnames(MeteoMonth_df_tidy)=="RS"] <- "comId"
  colnames(MeteoMonth_df_tidy)
  
  ## Reduce comId string to years integers only ##
  MeteoMonth_df_tidy$year <- as.integer(str_sub(MeteoMonth_df_tidy$year,5,8 ))
  unique(MeteoMonth_df_tidy$year)
  
  ## Reduce ID-Strings to proper comId  ##
  MeteoMonth_df_tidy$comId <- as.factor(str_sub(MeteoMonth_df_tidy$comId,1,5))
  unique(MeteoMonth_df_tidy$comId)
  
  
  # #################################################################################
  # ### Abgleichen der neu erstellten Daten mit MeteoSMI_df_wide (orginal Daten) ####
  # head(MeteoSMI_df_wide$SMIJan1951)==head(MeteoMonth_df_tidy$SMI_Jan)
  # 
  # tail(MeteoSMI_df_wide$SMIDec2099)==tail(MeteoMonth_df_tidy$SMI_Dec)
  # 
  # ## Erstellen und überprüfen der beiden Vektoren, die jeweils den SMI des Jahres 19999 im Monat Januar wiedergebeb
  # ## Vector in MeteoMonth_df_tidy des SMI Januar 19999
  # MeteoMonth_df_tidy$SMI_Jan[MeteoMonth_df_tidy$year == 1999]
  # length(MeteoMonth_df_tidy$SMI_Jan[MeteoMonth_df_tidy$year==1999])
  # 
  # ## Vector in MeteoSMI_df_wide des SMI Januar 19999
  # MeteoSMI_df_wide$SMIJan1999
  # length(MeteoSMI_df_wide$SMIJan1999)
  # 
  # match(MeteoSMI_df_wide$SMIJan1999,MeteoMonth_df_tidy$SMI_Jan[MeteoMonth_df_tidy$year==1999]) # check
  # all(MeteoMonth_df_tidy$SMI_Jan[MeteoMonth_df_tidy$year==1999]%in%MeteoSMI_df_wide$SMIJan1999) # check
  # # Daten sehen gut aus

  ############################################################################################################
  #### Produce lagged variabled of SMI and the meteorological variables for the months after the harvest ####
  ##########################################################################################################
  names(MeteoMonth_df_tidy)
  
  ## List of variables to produce lags 
  list_slide <- c("SMI_Aug","SMI_Sep","SMI_Oct","SMI_Nov","SMI_Dec", "T_Aug","T_Sep","T_Oct","T_Nov","T_Dec",
                  "P_Aug","P_Sep","P_Oct","P_Nov","P_Dec")
  
  ## Loop through the list 
  for (k in 1:length(list_slide)){
    MeteoMonth_df_tidy <- slide(MeteoMonth_df_tidy, Var =list_slide[[k]], GroupVar= "comId", slideBy = -1) 
    }
  
  colnames(MeteoMonth_df_tidy) <- sub("-1", "_lag", colnames(MeteoMonth_df_tidy))
  colnames(MeteoMonth_df_tidy) 
  
  # dim(head(MeteoMonth_df_tidy) )
  # head(MeteoMonth_df_tidy)  
  # tail(MeteoMonth_df_tidy)
  
  ## Check for validity ##
  # head(MeteoMonth_df_tidy[MeteoMonth_df_tidy$year==1999,])[10:14] %in% head(MeteoMonth_df_tidy[MeteoMonth_df_tidy$year==2000,])[39:43]  
  ' Da zum Beispiel der August des Jahre 19999 mit dem lag des Jahres 2000 übereinstimmt, scheint alles zu passen'
  
  ############################################
  #### Write newly created MeteoMonth_df_tidy ####
  ##########################################
  write_csv(MeteoMonth_df_tidy, paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_models[[i]],".csv", sep=""))
  
  str(MeteoMonth_df_tidy)

} ## close of reshape loop 

rm(list=ls())


