#############################
#### Data Pre-Processing ####
#############################

#### Description of Script ####
' - Prepare data.frame on yield and meteo vars for exploring models and fitting / train final model
    - Load Maize_meteo -> filter for relevant data (siloMais, etc)
    - Generate Stepwise functions of SMI with six anomalies
    - Delete rows with NAs -> among those are the 4 comId with no agricultural land
    - Remove comIds with less than nine yield observations to avoid leverage issues (comparable to first paper - another cut would also delete the 
      the important regions for silage maize production at Rheinebene)
    - Remove log trend of indepedent variable -> no lnear trend observed, i.e. no deleting, but relevant to detect outliers
    - Issue with Outliers - Correct Typo in Data
    - Change Variable Names: Prec -> P, Tavg -> T 
    - Create yield anonmalies (Yield - comId specific mean )
'    
#### Output ####
## Files
' - avgYield_comId.csv (com Id specific mean of yield) -> /Proj2/data/data_processed/
  - Maize_meteo (data.frame after preparation scheme described above) -> /Proj2/data/data_processed/

'
## Plots
'
- Maps with missing values (different cutoffs) <-/figures_exploratory/Train/
- Map with average yield for each comId: AverageYield.pdf  <-/figures_exploratory/Train/
- Map of sum of yield_anomalies for each comId: Yield_predict_sumsComId.pdf <- /figures_exploratory/Train/", modelListNames[[i]]

'

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
'  -  YieldMeteo.csv-> /Proj2/data/data_processed/ <- (Merge_YieldMeteo_Proj2.R)

'



###################
## Load Packages ##
source("./script/script_raw/Packages.R")

##############################################################################################################################################################################
# rm(list=ls()) ## clear workspace
##############################################################################################################################################################################
#### Prepare data.frame on yield and meteo vars for exploring models and fitting final model #####
##############################################################################################################################################################################

################################################
#### Load shape of administrative districts ####
################################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

##################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
################################################################################################################

##########################################
#### Read in tidy Dataframe for Maize ####
Maize_meteo <- read_csv("./data/data_processed/YieldMeteo.csv")

# Maize_meteo$ <- NULL
# Maize_meteo$comId <- as.integer(Maize_meteo$comId)
# str(Maize_meteo)
# names(Maize_meteo)

##############################################################
##### Remove variabels which are not relevant for Maize #####


#### Unselect dependent Variables but silo Maize ####
Maize_meteo <- Maize_meteo %>% select(-winterWheat,-rye,-winterBarley,-summerBarley, -oats, -triticale, -potatoes, -sugarBeet, -winterRape)
names(Maize_meteo) 

#### Deselct _lag data ###
Maize_meteo <- Maize_meteo %>% select(names(Maize_meteo), -matches("_lag"))
names(Maize_meteo) 


# #### Deselct PET_ data ###
# Maize_meteo <- Maize_meteo %>% select(names(Maize_meteo), -matches("PET_"))
# names(Maize_meteo) 

#### Deselect variables which are off season  #####
deselect <- (c("_Jan|_Feb|_Mar|_Apr|_Nov|_Dec"))
colDeSelect <- grep(deselect, names(Maize_meteo))
Maize_meteo <- Maize_meteo %>% select(- (colDeSelect))
  
names(Maize_meteo)


# Maize_meteo <- as.data.frame(Maize_meteo )
View(Maize_meteo)
# any(Maize_meteo[Maize_meteo == 0])


##########################################
#### Create stepwise function of SMI ####
########################################

#################################
## Stepwise with six anomalies ##

# May
Maize_meteo$SMI_May6 <- relevel(cut(Maize_meteo$SMI_May, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
                                    # labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                    #            "abnrml wt" ,"abndnt wt", "svr wt")),  "nrml") 
table(Maize_meteo$SMI_May6, Maize_meteo$year  )


# June
Maize_meteo$SMI_Jun6 <- relevel(cut(Maize_meteo$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
table(Maize_meteo$SMI_Jun6, Maize_meteo$year  )

# July
Maize_meteo$SMI_Jul6 <- relevel(cut(Maize_meteo$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
table(Maize_meteo$SMI_Jul6, Maize_meteo$year  )
levels((Maize_meteo$SMI_Jun6))

# Aug
Maize_meteo$SMI_Aug6 <- relevel(cut(Maize_meteo$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
table(Maize_meteo$SMI_Aug6,Maize_meteo$year  )

# Sep
Maize_meteo$SMI_Sep6 <- relevel(cut(Maize_meteo$SMI_Sep, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
table(Maize_meteo$SMI_Sep6,Maize_meteo$year  )


# Oct
Maize_meteo$SMI_Oct6 <- relevel(cut(Maize_meteo$SMI_Oct, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                    labels = c("drght_svr","drght_mdrt","dry", "nrml",
                                               "wt" ,"wt_abndnt", "wt_svr")),  "nrml") 
table(Maize_meteo$SMI_Oct,Maize_meteo$year  )

# ###################################
# #### Deselect normal SMI data ####
# #################################
# Maize_meteo <- Maize_meteo %>% select(-c(SMI_May:SMI_Oct))
# names(Maize_meteo)
# 
# unique(Maize_meteo$comId)

# ###########################
# #### Demean siloMaize ####
# #########################
# Maize_meteo
# 
# Maize_meteo_demean <- Maize_meteo %>%
#                       group_by(comId) %>%
#                       mutate_at("siloMaize", funs(SiloMaize_demeaned= . - mean(.)))
# 
# 'For the case that the data do not have full observations (17), no mean is created and the data are transformed into NAs'
# 
# Maize_meteo_demean
# View(Maize_meteo_demean)

#########################################################################################
#### Remove comIds with more than one observations missing to avoid leverage issues #### 
#######################################################################################

############################################
## First delete all observations with NAs ##
is.na(Maize_meteo)

Maize_meteo_naOmit <- na.omit(Maize_meteo)
str(Maize_meteo_naOmit )
 
length(unique(Maize_meteo_naOmit$comId)) # 365 
'45 comIds have no observation in general. Those are for example the 4 com which have no agricultural land. '

unique(as.factor(Maize_meteo_naOmit$comId))

#######################################################################################
#### Check distribution of observations to get an impression about missing values ####
#####################################################################################
missing_distribution <- as.data.frame(table(Maize_meteo_naOmit$comId))


#############################################################
#### Make map of spatial distribution of missing values ####
###########################################################
# missing_distribution_sf <- merge(vg2500_krs, missing_distribution, by.x="RS", by.y="Var1")
# # dim(missing_distribution_sf)
# str(missing_distribution_sf)
# missing_distribution_sf$Freq <- as.numeric(missing_distribution_sf$Freq)
# 
# missing_distribution_sf_plot_09 <-
#   ggplot(missing_distribution_sf) + 
#     # geom_sf(data = vg2500_krs, fill="black") + 
#     geom_sf(aes(fill = cut(Freq, c(0,9,17)) )) + 
#   guides(fill=guide_legend(title="Number of Observations - Cut 9"))
# 
# missing_distribution_sf_plot_10 <-
#   ggplot(missing_distribution_sf) + 
#   # geom_sf(data = vg2500_krs, fill="black") + 
#   geom_sf(aes(fill = cut(Freq, c(0,10,17)) ))+ 
#   guides(fill=guide_legend(title="Number of Observations - Cut 10"))
# 
# 
# missing_distribution_sf_plot_9_10_17 <-
#   ggplot(missing_distribution_sf) + 
#   # geom_sf(data = vg2500_krs, fill="black") + 
#   geom_sf(aes(fill = cut(Freq, c(0,9,10,17)) )) + 
#   guides(fill=guide_legend(title="Number of Observations"))
# 
# missing_distribution_sf_plot_16_17 <-
#   ggplot(missing_distribution_sf) + 
#   # geom_sf(data = vg2500_krs, fill="black") + 
#   geom_sf(aes(fill = cut(Freq, c(0,14,17)) )) + 
#   guides(fill=guide_legend(title="Number of Observations"))
# 
# ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut9.pdf", sep="") , missing_distribution_sf_plot_09, width=16, height=9) 
# ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut10.pdf", sep="") , missing_distribution_sf_plot_10, width=16, height=9) 
# ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_9_10_17.pdf", sep="") , missing_distribution_sf_plot_9_10_17, width=16, height=9) 
# ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_16_17.pdf", sep="") , missing_distribution_sf_plot_16_17, width=16, height=9) 

# 
# ## comIds with at least one observation missing ##
# list_delete <- c( 3101, 3102, 3103, 3153, 3154, 3157, 3158,  
#            3402, 3404, 
#            5111, 5112, 5113, 5114, 5117, 5119, 5124, 5314, 5315, 5316, 5334, 5378,  5512, 5515 ,
#            5711, 5911, 5913, 5914, 5916, 5954,  6411, 6412, 6413, 
#            6438, 6611, 7131, 7132, 7133, 7134, 7135, 7137, 7138, 7140, 7141, 7143, 
#            7233,  7331, 7332, 7333, 7334, 7335, 7337, 7338, 7339, 7340, 8111, 8115, 8117, 8135,  8215, 8216, 
#            8235,  8315, 8316, 8317,  8326, 8327,  8336, 8415, 8416,  8435 ,
#            9461,  
#            12052, 12053, 13051, 13052, 13053, 13054, 13055, 13056, 13057, 13058, 13059, 13060, 13061, 
#            13062, 14511, 14612, 14628, 14713, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052, 16056, 
#            16072)
# length(list_delete)
# list_delete[[1]]

#################################################
#### comIds with less than nine observation ####
###############################################
table(Maize_meteo_naOmit$comId) < 9 
sum(table(Maize_meteo_naOmit$comId) < 9) # 103 comIds have missing independent data when only considering full data sets, 31 with a cutoff of nine

list_delete <-  as.integer(c("03101", "03102", "03402", "05111", "05112", "05113", "05114", "05117", "05124", "05314",
                 "05315", "05334", "05378", "05512", "05911", "05916", "07131", "07133", "07135", "07233", 
                 "07331", "07332", "07334", "07335", "07337", "07338", "07339", "08111", "12052", "14612", "16052"))

length(list_delete) # 31

# #### Make list of comId to keep ####
# list_keep <- unique(Maize_meteo_naOmit$comId)  [!unique(Maize_meteo_naOmit$comId) %in%  list_delete ]
# length(list_keep)

#### Filter for those comId ####
# temp1 <-  Maize_meteo_naOmit %>% filter(comId %in% list_keep)
temp1 <-  Maize_meteo_naOmit %>% filter(!comId %in% list_delete)
unique(temp1$com) # 334 Landkreise

####################################################
#### Look at comIds with missing dependent data ####
k <- NULL
for (k in 1:length(list_delete)){
print(Maize_meteo_naOmit[Maize_meteo_naOmit$comId == list_delete[[k]],1:6])  
}


' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

###################################################
#### Delete comIds which are Kreisfreie Städte ####
temp2 <- temp1 %>% filter(!str_detect(com, 'Stadt|Kreisfreie Stadt'))
temp3 <- temp2 %>% filter(str_detect(com, ', |Kreis|kreis'))

unique(temp2$com)
unique(temp3$com)

summary(temp3$siloMaize)
summary(temp2$siloMaize)
summary(temp1$siloMaize)


'Durch das Herausnehmen der Kreisfreien Städte wird die Varianu nach oben eingeschränkt, dennoch bleibt die Mean und der Median nahezu unverändert.
'

####################################################################################################
#### Delete outliers found with lm-modelling on BaseModel (Jun and Aug SMI, Meteorology of Jul) ####
temp3[c(815,311,1077),]
View(Maize_meteo%>% filter(comId %in% c(5158, 5374,5358 )))
temp3 <- temp3[-c(815,311,1077),]

####################################
#### Take a look at distrbution ####
summary(temp3$siloMaize)
summary(temp2$siloMaize)
summary(temp1$siloMaize)

## Number of deleted rows ##
dim(temp1) - dim(Maize_meteo_naOmit) # -148 
dim(temp2) - dim(Maize_meteo_naOmit) # -509 (ohe Städte/Kreisfreie Städte)
dim(temp3) - dim(Maize_meteo_naOmit) # -897  (ohe Städte/Kreisfreie Städte) +  (-3) ohne outliers


## Further use old name for data.frame
Maize_meteo <- temp3

# View(Maize)

any(sum(table(Maize_meteo$comId) < 9) ) # 103 comIds have missing independent data when only considering full data sets, 31 with a cutoff of nine
any(table(Maize_meteo$comId) < 9 )


length(unique(Maize_meteo$comId)) # 313 (at least nine observations and no city communities) / 334 (at least nine observations) / 262 (only full comIds)

######################################
## Clean up rownames after deleting ##
rownames(Maize_meteo) <- NULL


# ##################################################
# #### Remove log trend of indepedent variable ####
# ################################################
trend <- lm(siloMaize ~ as.integer(year), data = Maize_meteo)
summary(trend)

'Da es keinen Trend gibt, detrenden wir die Daten nicht mehr. Dennoch lass ich weiterhin siloMaize_logtrend im Datensatz '

# # ###############################################
# # #### Compare different silage Maize Versions ##
# summary(log(Maize_meteo$siloMaize))
# summary(Maize_meteo$siloMaize)
# summary(Maize_meteo$siloMaize_logtrend)
# 
# hist(log(Maize_meteo$siloMaize))
# hist(Maize_meteo$siloMaize)
# hist(Maize_meteo$siloMaize_logtrend)
# ' Auch die Verteilung gibt keinen wirklichen Anlass zu detrenden, oder den logaithmus zu benutzen.'

#####################################################
#### Issue with Outliers - Correct Typo in Data ####
###################################################
' based on outlier statistic in regression on trend'
par(mfrow = c(2,2))
plot(trend) 

## Look at Outliers Values ##
Maize_meteo[c(1930),] # comId 6532, 5116, 5319 when taking into accout city communities

## Look at other values of outliers com -  when not taking out city communitites ##
outliers <- c(6532, 5116, 5913, 9376)
Maize_meteo %>% filter(comId %in% outliers)
# View(Maize_meteo %>% filter(comId %in% outliers))

## Correct Typo in Data ##
Maize_meteo[c(Maize_meteo$comId == "6532" & Maize_meteo$year == "2008"), 6] <- 486.4

######################################################################
#### Retrieve anomaly correction: comId specific mean siloMaize) ####
####################################################################
' The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
This is particular important for  the plotting acticities (corrected for average expected silage maize (log). '
dim(Maize_meteo)

avgYield_comId <- 
  Maize_meteo %>% 
    group_by(comId) %>% 
      summarise(avgYield_comId = mean(siloMaize))

str(avgYield_comId)
summary(avgYield_comId$avgYield_comId)

###############################
#### Export avgYield_comId ####
write_csv(avgYield_comId, path="./data/data_processed/avgYield_comId.csv")

avgYield_comId <- read_csv("./data/data_processed/avgYield_comId.csv")
summary(avgYield_comId)

#######################################
#### Make a map of average values ####
#####################################
avgYield_comId_sf <- as.tibble(merge(vg2500_krs, avgYield_comId, by.x="RS", by.y="comId"))
avgYield_comId_sf

## Plot
averageYield_plot <- 
  ggplot(avgYield_comId_sf) + 
  geom_sf(data=vg2500_krs,fill="white") + 
  geom_sf(aes(fill = avgYield_comId)) +  
  guides(fill = guide_legend(title = "Avg. Yield ")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

## Save Plot
ggsave(paste("./figures/figures_exploratory/Train/", "AverageYield.pdf", sep=""), averageYield_plot, device="pdf", width=8, height= 8) 

###########################################################
#### Append averageYield of each comId on Maize_meteo #####
###########################################################

## Make data.frame with values of average values of each comId ##
dim(avgYield_comId)
avgYield_comId_df <- avgYield_comId[rep(seq_len(17*dim(avgYield_comId)[1])),]
dim(avgYield_comId_df )
avgYield_comId_df$average_yield <- as.factor(avgYield_comId_df$avgYield_comId)
dim(avgYield_comId_df )

avgYield_comId_df <- do.call("rbind", replicate(17, avgYield_comId, simplify = FALSE))
dim(avgYield_comId_df )
avgYield_comId_df$year <- sort(rep(seq(1999,2015),dim(avgYield_comId)[1]))
# str(avgYield_comId_df)
# View(avgYield_comId_df)

#### Merge with Maize_meteo #####
Maize_meteo_avYield <- inner_join(Maize_meteo, avgYield_comId_df, by=c("comId","year"))
Maize_meteo_avYield
# str(Maize_meteo)

## Further use standard name ##
Maize_meteo <- Maize_meteo_avYield
rm(Maize_meteo_avYield)

############################################
#### Transform yield in yield anomalies ####
############################################
Maize_meteo <- 
  Maize_meteo %>%
  mutate(siloMaizeAnomaly = siloMaize - avgYield_comId)
names(Maize_meteo)
##################################
#### Change order of columns ####
################################
Maize_meteo <- Maize_meteo[, c(1:6, 55:56, 7:54)]

############################################################
##### Save newly created data.frame Maize_meteo extern ####
##########################################################
Maize_meteo
write_csv(Maize_meteo, path="./data/data_processed/Maize_meteo.csv")

###########################
#### Clear Environment ####
# rm(list=ls())
   # [! ls() %in% c("Maize_meteo", "vg2500_krs", "avgYield_comId")])

