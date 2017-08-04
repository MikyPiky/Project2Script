#######################################################################
## Base Model for Predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################
#### Description of Script ####
' - Prepare data.frame on yield and meteo vars for exploring models and fitting / train final model
    - Load Maize_meteo -> filter for relevant data (siloMais, etc)
    - Generate Stepwise functions of SMI with six anomalies
    - Remove comIds with less than nine observations to avoid leverage issues (comparable to first paper - another cut would also delete the 
      the important regions for silage maize production at Rheinebene)
    - Remove log trend of indepedent variable -> no trend observed, i.e. no deleting, but relevant to detect outliers
    - Issue with Outliers - Correct Typo in Data
    - Change Variable Names: Prec -> P, Tavg -> T 
    - Create yield anonmalies (Yield - comId specific mean )
  - Explore Models
      - many results, please see specific section
  -  Explore differences on estimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities 
      - many results, please go to section
  - Use of Caret Package: I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
    account one fold for each year. The implementation can be found in BasePrediction_long
  - Retrieve anomaly correction: comId specific mean siloMaize 
    The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
    This is particular important for  the plotting acticities (corrected for average expected silage maize). 
  - Predict on data which were used to train the model (one prediction for each year) to allow comparision 
    - Fit combined Model and best model from Paper 1
    - Loop through those models to make prediction for each year




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
'  -  YieldMeteo.csv-> /Proj2/data/data_processed/ <- derived from Project 1 ( Merge_YieldMeteo.R)

'



###################
## Load Packages ##
library(plm)
library(boot)
library(gtools)
library(lme4)
library(lmtest)
library(car)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(stringr)
library(classInt)
library(RColorBrewer)
library(stargazer)
library(ggthemes)
library(caret)   
library(plyr)
library(sf)
library(tidyverse)
library(ggplot2)  
library(grDevices)
##############################################################################################################################################################################


##############################################################################################################################################################################
#### Prepare data.frame on yield and meteo vars for exploring models and fitting final model #####
##############################################################################################################################################################################

##################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
################################################################################################################

## Read in tidy Dataframe for Maize ##
Maize_meteo <- read.csv("./data/data_processed/YieldMeteo.csv")
Maize_meteo$X <- NULL
str(Maize_meteo)

#############<#######################################
#### Unselect dependent Variables but silo Maize ####
Maize_meteo <- Maize_meteo %>% select(-winterWheat,-rye,-winterBarley,-summerBarley, -oats, -triticale, -potatoes, -sugarBeet, -winterRape)

################################################
#### Load shape of administrative districts ####
################################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

##########################################
#### Create stepwise function of SMI ####
########################################

#################################
## Stepwise with six anomalies ##

# June
Maize_meteo$SMI_Jun6 <- relevel(cut(Maize_meteo$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Jun6, Maize_meteo$year  )

# July
Maize_meteo$SMI_Jul6 <- relevel(cut(Maize_meteo$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                         labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                    "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Jul6, Maize_meteo$year  )


# Aug
Maize_meteo$SMI_Aug6 <- relevel(cut(Maize_meteo$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Aug6,Maize_meteo$year  )


#########################################################################################
#### Remove comIds with more than one observations missing to avoid leverage issues #### 
#######################################################################################

############################################
## First delete all observations with NAs ##
Maize_meteo <- na.omit(Maize_meteo)
length(unique(Maize_meteo$comId)) # 365 / 373
'45 comIds have no observation in general '

######################################################
## Delete all comIds with less than 9 observations ##
missing_distribution <- as.data.frame(table(Maize_meteo$comId))
# str(missing_distribution)
sum(table(Maize_meteo$comId) < 9) # 103 comIds have missing independent data when only considering full data sets, 31 with a cutoff of nine
table(Maize_meteo$comId) < 9 

####################################################
#### Make map of distribution of missing values ####
missing_distribution_sf <- merge(vg2500_krs, missing_distribution, by.x="RS", by.y="Var1")
# dim(missing_distribution_sf)
str(missing_distribution_sf)
missing_distribution_sf$Freq <- as.numeric(missing_distribution_sf$Freq)

missing_distribution_sf_plot_09 <-
  ggplot(missing_distribution_sf) + 
    # geom_sf(data = vg2500_krs, fill="black") + 
    geom_sf(aes(fill = cut(Freq, c(0,9,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations - Cut 9"))

missing_distribution_sf_plot_10 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,10,17)) ))+ 
  guides(fill=guide_legend(title="Number of Observations - Cut 10"))


missing_distribution_sf_plot_9_10_17 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,9,10,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations"))

missing_distribution_sf_plot_16_17 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,14,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations"))

ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut9.pdf", sep="") , missing_distribution_sf_plot_09, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut10.pdf", sep="") , missing_distribution_sf_plot_10, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_9_10_17.pdf", sep="") , missing_distribution_sf_plot_9_10_17, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_16_17.pdf", sep="") , missing_distribution_sf_plot_16_17, width=16, height=9) 

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

## comIds with less than nine observation: ##
list_delete <- c(3101, 3102, 3402, 5111, 5112, 5113, 5114, 5117, 5124, 5314,
                 5315, 5334, 5378, 5512, 5911, 5916, 7131, 7133, 7135, 7233, 
                 7331, 7332, 7334, 7335, 7337, 7338, 7339, 8111,12052, 14612, 16052)

length(list_delete) # 31
list_delete[[1]]


## Look at comIds with missing dependent data ##
k <- NULL
for (k in 1:length(list_delete)){
print(Maize_meteo[Maize_meteo$comId == list_delete[[k]],1:6])  
}
' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

## Delete comIds with at least one observation missing ##
temp <- Maize_meteo
for (k in 1:length(list_delete))
{
  print(Maize_meteo[Maize_meteo$comId==list_delete[k],])
  temp <- (temp[!temp$comId==list_delete[k],])
}


## Number of deleted rows ##
dim(temp) - dim(Maize_meteo) # -148

## Further use old name for data.frame
Maize_meteo <- temp

length(unique(Maize_meteo$comId)) # 334 (at least nine observations) / 262 (only full comIds)

##################################
## Clean up data after deleting ##
rownames(Maize_meteo) <- NULL
str(Maize_meteo)
# Maize_meteo <- plm.data(Maize_meteo, index=c("comId", "year")) 
Maize_meteo[,c("comId","year")] <- lapply(Maize_meteo[,c("comId","year")], factor )


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
Maize_meteo[c(1785, 934, 1383),] # comId 6532, 5116, 5319

## Look at other values of outliers com #
Maize_meteo[Maize_meteo$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen korrigiere ich 86.4 <- 486.4
Maize_meteo[Maize_meteo$comId == "5116",] # 2006 verändere ich nicht 
Maize_meteo[Maize_meteo$comId == "5913",] # 2003 verändere ich nicht 

## Correct Typo in Data ##
Maize_meteo[c(Maize_meteo$comId == "6532" & Maize_meteo$year == "2008"), 6] <- 486.4

##########################################################
#### Change Variable Names: Prec -> P, Tavg -> T ####
########################################################
names(Maize_meteo) <- gsub("Pre", "P", names(Maize_meteo))
names(Maize_meteo) <- gsub("Tav", "T", names(Maize_meteo))


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
write.csv(avgYield_comId, file="./data/data_processed/avgYield_comId.csv")

#######################################
#### Make a map of average values ####
#####################################
avgYield_comId_sf <- merge(vg2500_krs, avgYield_comId, by.x="RS", by.y="comId")
dim(avgYield_comId)
dim(avgYield_comId_sf)
str(avgYield_comId_sf)

## Plot
averageYield_plot <- 
  ggplot(avgYield_comId_sf) + 
  geom_sf(data=vg2500_krs,fill="white") + 
  geom_sf(aes(fill = avgYield_comId)) +  
  guides(fill = guide_legend(title = "Av. Yield ")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

## Save Plot
ggsave(paste("./figures/figures_exploratory/Train/", "AverageYield.pdf", sep=""), averageYield_plot, device="pdf", width=8, height= 8) 

###########################################################
#### Append averageYield of each comId on Maize_meteo #####
###########################################################

## Make data.frame with values of average values of each comId ##
dim(avgYield_comId)
avgYield_comId_df <- avgYield_comId[rep(seq_len(17*334)),]
dim(avgYield_comId_df )
avgYield_comId_df$average_yield <- as.factor(avgYield_comId_df$avgYield_comId)
dim(avgYield_comId_df )

avgYield_comId_df <- do.call("rbind", replicate(17, avgYield_comId, simplify = FALSE))
dim(avgYield_comId_df )
avgYield_comId_df$year <- as.factor(sort(rep(seq(1999,2015),334)))
# str(avgYield_comId_df)
# View(avgYield_comId_df)

#### Merge with Maize_meteo #####
Maize_meteo_avYield <- merge(Maize_meteo, avgYield_comId_df, by=c("comId","year"))
str(Maize_meteo_avYield)
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

############################################################
##### Save newly created data.frame Maize_meteo extern ####
##########################################################
str(Maize_meteo)
write.csv(Maize_meteo, file="./data/data_processed/Maize_meteo.csv")

###########################
#### Clear Environment ####
rm(list=ls()[! ls() %in% c("Maize_meteo", "vg2500_krs", "avgYield_comId")])

# ##################################################################################################################################################################
# ################################### Explore Models ###############################################################################################################
# ##################################################################################################################################################################
 
# ' 
# Caveat: Die Explorative Analyse war für den Datensatz welcher nur comIds mit vollständigen Report berücksichtigt hat
# Ich habe hier eine explorative Analyse verschiedener Modelle gemacht. Der Code zu der Analyse befindet sich in BasePrediction_long.R .
#  Vor allem habe ich mit stepwise in 4 Stufen und Polynomen von SMI gespielt.
# Ergebnisse sind
# - Interessant ist, dass wenn man für die Meteorologie kontrolliert (P, T im July), die SMI wet im August an Significanz verlieren 
#   (Adj. R-Squared: 0.34778 fpr diese Modell (SMIJun6, Pol3TPJun, SMIAug6)
# - Wenn mann eine stepwise function mit nur 4 steps nimmt, dann sind die Ergebnisse vergleichbar (Adj. R-Squared: 0.34353)
# - Interactionsterme zwischen SMIJun4 und SMIJun4 liefern schlechtere Ergebnisse als mit Meteoroligie
# (Adj. R-Squared: 0.23835 ohne T und P July): Es scheint nicht möglich zu sein für
# die Meteorologie in July via SMI-Intaction Jun und Ug zu kontrollieren. 
# - Wenn ich neben Interaction SMI auch für Meteorologie im July reagieren habe ich ein Adj. R-Squared: 0.34989 für Polynome 3. Grades 
#   und 0.35036 für Polynome 4. Grades in P and T 
# - Polynime statt stepwise functions für den SMI verbessern die Ergebnnise auch nicht :
# # # Adj. R-Squared: 0.34989, Polynome 3. Grades in P and T and SMI
# # # Adj. R-Squared: 0.35036, Polynome 4. Grades in P and T , SMi 3. Grad
# # # Adj. R-Squared: 0.35595, Polynome 4. Grades in P and T and SMI
# -  Meteorologie im July verbessert die vorhersagekraft im sample sehr. 
# '
# #############################################################################################################################################################################
# #############################################################################################################################################################################
# 
# ################################################################################################################################
# #### Explore differences on extimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities #####
# ##############################################################################################################################
# 
# ##############################
# #### (Time) - Demean Data ####
# str(Maize_meteo)
# ## Scale all the data within a comId ##
# Maize_meteo_demean1  <- ddply(Maize_meteo,  ~ comId,  numcolwise(scale, scale=FALSE))
# 
# summary(Maize_meteo_demean1)#
# str(Maize_meteo_demean1)
# names(Maize_meteo_demean1)
# 
# #########################################################################
# #### Compare the data demeaned by ddplyr with manually demeand Data ####
# 
# ## ddplyr
# head(Maize_meteo_demean1,18)[5]
# ## manually
# # head(Maize_meteo)[8]
# Maize_meteo[1:18,8] - mean(Maize_meteo[1:17,8]) 
# ' Da der 18. Wert nicht mehr übereinstimmt scheint dass demeanen für die einzelnen comIds zu funktionieren.'
# 
# #######################################################################################################
# #### Comprise full demeaned data.frame  also including temporal information and stepwise functions ####
# ## Append Year ##
# year <- Maize_meteo$year
# Maize_meteo_demean  <- cbind(year, Maize_meteo_demean1)
# summary(Maize_meteo_demean)
# names(Maize_meteo_demean)
# 
# ## Change Order that comId is first column ##
# Maize_meteo_demean  <- Maize_meteo_demean[,c(2,1,3:length(names(Maize_meteo_demean)))]
# names(Maize_meteo_demean )
# 
# ## Append stepwise functions ##
# names(Maize_meteo)[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))]
# names(cbind(Maize_meteo_demean, Maize_meteo[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))]))
# 
# Maize_meteo_demean <- cbind(Maize_meteo_demean, Maize_meteo[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))])
# str(Maize_meteo_demean )
# 
# ############################################################
# #### Compare results of plm, demean, and lm with dummy ####
# ##########################################################
# 
# #### linear ####
# ## plm
# plm.fit_lin <- plm(siloMaize ~ SMI_Jun6 + SMI_Aug6 + I(T_Jul)  ,
#                data = Maize_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_lin)
# 
# ## demean
# lm.fit_demean_lin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul) , data = Maize_meteo_demean)
# summary(lm.fit_demean_lin)  
# 
# ## LSDV 
# lm.fit_dummy_lin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul) + comId, data = Maize_meteo)
# summary(lm.fit_dummy_lin) 
# 
# ## Comparision of Coefficients ##
# coefficients(lm.fit_dummy_lin)[2:14] # I(T_Jul): -0.019542 (siloMaize_logtrend); -0.02019190 log(siloMaize);  -8.423623 (silomaize);
# coefficients(plm.fit_lin)[1:13] # I(T_Jul):  -0.0195418, (siloMaize_logtrend);-0.02019190 log(siloMaize); -8.423623 (silomaize);
# coefficients(lm.fit_demean_lin)[2:14] # I(T_Jul): -0.020220 (siloMaize_logtrend); -0.02019190 log(siloMaize); -8.714845 (silomaize);
# ' Die Coefficienten ändern sich kaum wenn man mit cutoff 9 statt mit cuttoff 17 arbeitet'
# 
# #### nonlinear ####
# ## plm
# plm.fit_nonlin <- plm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul)  + I(T_Jul^2) ,
#                    data = Maize_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_nonlin) # Adj.  R-sq  0.33496 (siloMaize), 0.31927 (siloMaize_logtrend)
# 
# ## demean
# lm.fit_demean_nonlin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul)  + I(T_Jul^2) , data = Maize_meteo_demean)
# summary(lm.fit_demean_nonlin) # Adj.  R-sq  0.3611  (siloMaize),  0.3454  (siloMaize_logtrend)
# 
# ## LSDV 
# lm.fit_dummy_nonlin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul)  + I(T_Jul^2) + factor(comId), data = Maize_meteo)
# summary(lm.fit_dummy_nonlin) # Adj.  R-sq  0.6801 (siloMaize),  0.6779  (siloMaize_logtrend)
# 
# ## Comparision of Coefficients ##
# coefficients(lm.fit_dummy_nonlin)[2:15 ] # I(T_Jul) 0.146906158, I(T_Jul^2) -0.004449528 (siloMaize_logtrend);  
# # I(T_Jul)55.492156  , I(T_Jul^2) -1.708612 (siloMaize)
# coefficients(plm.fit_nonlin)[1:14]  # I(T_Jul) 0.146906158, I(T_Jul^2) -0.004449528 (siloMaize_logtrend)
# # I(T_Jul)55.49215 , I(T_Jul^2) -1.708612 (siloMaize)
# coefficients(lm.fit_demean_nonlin)[2:15] # I(T_Jul) -0.017895938 , I(T_Jul^2) -0.004448825(siloMaize_logtrend)
# # I(T_Jul)-7.806 , I(T_Jul^2) -1.740 (siloMaize)
# 
# 
# ' Die Coefficienten ändern sich kaum wenn man mit cutoff 9 statt mit cuttoff 17 arbeitet
# Bei linearer Konfiguration: Für die Coefficienten bekomme ich die gleichen Werte. 
# Das adjusted R-square unterscheidet sich aber zwischen demean und plm.
# Dummy liefert natürlich ein größeres R2, da dort die Fixed Effects explicit mit eingehen. 
# Bei linearen Modellen kann man also Problemlos demeanen. Des weiteren ist der SMI 
# wohl so wie er definiert ist nicht durch demeanen betroffen, da das demeaning framework nicht für die SMIs angewendet wurde 
# (numcolwise berücksichtige alle Faktoren nicht), 
# die Ergebnisse bei linearen Konfigurationen aber gleich sind.
# Bei nichtlinearitäten: lm.fit_demean liefert andere cofficienten für die Polynome als die drei anderen Modelle. 
# D.h. durch das demeanen werden die Poylnomberechnungen entscheidend verändert.
# The function poly does not work with demeanded data, even not with degree 1 '
# 
# '
# Ich glaube, es wäre schlecht, wenn die Daten gefitted werden, ohne dass jede räumliche Einheit betrachtet werden. In so einem Fall würde es nämlich keinen Fixed Effect
# für die Daten in einem Ort geben. Das heißt, dort würde kein demeaning stattfinden. Daher sollte ich darauf achten, dass ich stratified samples für cross-validation nehme ()
# '
#############################################################################################################################################################################
###############################################################################################################################################################################
# ############################
# #### use caret package ####
# ##########################
###############################################################################################################################################################################
' I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
account one fold for each year. The implementation can be found in BasePrediction_long'

#############################################################################################################################################################################
###############################################################################################################################################################################
###########################################################################################################################################################
################ Predict on data which were used to train the model (one prediction for each year) to allow comparision ##################################
#########################################################################################################################################################

#########################################
#### Fit model used for Predictions ####
#######################################

########################################################
#### Load data.frame including all prepocessed data ####
Maize_meteo <- read.csv(file="./data/data_processed/Maize_meteo.csv")
Maize_meteo$X <- NULL
str(Maize_meteo)
Maize_meteo$comId <-as.factor(Maize_meteo$comId)

avgYield_comId  <- read.csv(file="./data/data_processed/avgYield_comId.csv")
avgYield_comId$X <- NULL

#####################################################
#### Compare SD of different dependent variables ####
sd(Maize_meteo$siloMaize)
sd(Maize_meteo$siloMaizeAnomaly)
' SD for silage maize is larger for the absolute levels compared to the anomalies. Thus, the adjusted R2 is smaller. 
 R2 = 1 - SS_res/SS_tot. When the Varianz in the dependent variable is larger, SS_tot is accordingly larger and thus R2 is smaller. '

####################################
#### Fit combined model - yield ####
lm.fit_SMI_6_Jun_Aug_comId <- lm(siloMaize ~ 
                                 + T_Jul 
                                 + I(T_Jul^2) 
                                 + I(T_Jul^3) 
                                 + P_Jul 
                                 + I(P_Jul^2) 
                                 + I(P_Jul^3) 
                                 + SMI_Jun6 
                                 + SMI_Aug6 
                                 + comId
                                 ,
                                       data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId )  # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
                                      # Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)


# #### Compare to plm.fit ####
plm.fit_SMI_6_Jun_Aug_comId <- plm(siloMaize ~ 
                                   + T_Jul 
                                   + I(T_Jul^2) 
                                   + I(T_Jul^3) 
                                   + P_Jul  
                                   + I(P_Jul^2) 
                                   + I(P_Jul^3) 
                                   + SMI_Jun6 
                                   + SMI_Aug6
                                   ,
                                 data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId ) # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
#                                       # Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)
# 

#############################################
#### Fit best model from paper 1 - yield ####
lm.fit_SMI_6_Jul_comId <- lm(siloMaize ~ 
                             + T_Jul 
                             + I(T_Jul^2) 
                             + I(T_Jul^3) 
                             + P_Jul  
                             + I(P_Jul^2) 
                             + I(P_Jul^3) 
                             + SMI_Jul6
                             + comId
                             , data = Maize_meteo)
# summary(lm.fit_SMI_6_Jul_comId )  # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
                                  # Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)


# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jul_comId <- plm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
#                                    data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jul_comId ) # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
#                                   # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)
# 
# 
# #  
# #########################################
# #### Fit combined model - log(yield) ####
# lm.fit_SMI_6_Jun_Aug_comId_log <- lm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId,
#                                  data = Maize_meteo)
# summary(lm.fit_SMI_6_Jun_Aug_comId_log ) 
# # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
# # Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:  0.6869  (log(silomaize) - keep comIds with at least nine observations)
# 
# 
# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jun_Aug_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6,
#                                    data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug_comId_log ) 
# # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
# # Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:  0.33104  (log(silomaize) - keep comIds with at least nine observations)
# 

# ##################################################
# #### Fit best model from paper 1 - log(yield) ####
# lm.fit_SMI_6_Jul_comId_log <- lm(log(siloMaize)~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId,
#                              data = Maize_meteo)
# summary(lm.fit_SMI_6_Jul_comId_log )  
# # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
# # Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:   0.6859  (log(silomaize) - keep comIds with at least nine observations)
# 
# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jul_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
#                                data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jul_comId_log )
# # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
# # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared: 0.28289,   (log(silomaize) - keep comIds with at least nine observations)

############################################
#### Fit combined model - yield Anomaly ####
#### siloMaizeAnomaly - lm 
lm.fit_SMI_6_Jun_Aug_comId_anomaly <- lm(siloMaizeAnomaly ~ 
                                         + T_Jul 
                                         + P_Jul  
                                         + I(T_Jul^2) 
                                         + I(T_Jul^3) 
                                         + I(P_Jul^2) 
                                         + I(P_Jul^3) 
                                         + SMI_Jun6 
                                         + SMI_Aug6 
                                         + comId,
                                         data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly )  # Adjusted R-squared:   0.3362 (silomaizeAnomaly)

# #### siloMaizeAnomaly - plm 
plm.fit_SMI_6_Jun_Aug_comId_anomaly <- plm(siloMaizeAnomaly ~ 
                                            + T_Jul
                                            + I(T_Jul^2)
                                            + I(T_Jul^3)
                                            + P_Jul
                                            + I(P_Jul^2)
                                            + I(P_Jul^3)
                                            + SMI_Jun6
                                            + SMI_Aug6
                                            , data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly ) # Adjusted R-squared:  0.33616 (silomaizeAnomaly)

#### Check average of fixed effects ####
mean(fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly))

########################################################
#### Fit best model from paper 1 - siloMaizeAnomaly ####
lm.fit_SMI_6_Jul_comId_anomaly <- lm(siloMaizeAnomaly ~
                                      + T_Jul
                                      + I(T_Jul^2)
                                      + I(T_Jul^3)
                                      + P_Jul
                                      + I(P_Jul^2)
                                      + I(P_Jul^3)
                                      + SMI_Jun6
                                      + SMI_Aug6
                                     + comId
                                     , data = Maize_meteo)

summary(lm.fit_SMI_6_Jul_comId_anomaly)  # Adjusted R-squared:  0.2841  (silomaize - keep comIds with at least nine observations)
 # plot(lm.fit_SMI_6_Jul_comId_anomaly)


# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jul_comId_anomaly <- plm(siloMaizeAnomaly ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
#                                        data =  Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jul_comId_anomaly ) # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)




'
## Combined Model ##
## lsdv
# Adjusted R-squared:  0.6939  (silomaize_logtrend  - only full observations) 
# Adjusted R-squared:  0.6964  (silomaize - only full observations),
# Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.6869  (log(silomaize) - keep comIds with at least nine observations)
# Adjusted R-squared:  0.3362  (silomaizeAnomaly - keep comIds with at least nine observations)

## plm
# Adjusted R-squared:  0.35306 (silomaize_logtrend   - only full observations) 
# Adjusted R-squared:  0.36876 (silomaize - only full observations)
# Adjusted R-squared:  0.33616 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.33104 (log(silomaize) - keep comIds with at least nine observations)
# Adjusted R-squared:  0.33616 (silomaizeAnomaly - keep comIds with at least nine observations)

## plm / lsdv
# Adjusted R-squared:  0.5088053 (silomaize_logtrend   - only full observations) 
# Adjusted R-squared:  0.5295233(silomaize - only full observations)
# Adjusted R-squared:  0.4902435 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.4819333 (log(silomaize) - keep comIds with at least nine observations)


## Model Paper 1 ##

## lsdv
# Adjusted R-squared:  0.6694 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.669  (silomaize- only full observations),
# Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.6859 (log(silomaize) - keep comIds with at least nine observations)
# Adjusted R-squared:  0.2841 (silomaizeAnomaly - keep comIds with at least nine observations)

## plm
# Adjusted R-squared:  0.30122 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.31181 (silomaize- only full observations),
# Adjusted R-squared:  0.28411 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.28289 (log(silomaize) - keep comIds with at least nine observations)
# Adjusted R-squared:  0.28411 (silomaizeAnomaly - keep comIds with at least nine observations)

## plm /lsdv
# Adjusted R-squared:  0.4499851 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.4660837 (silomaize- only full observations),
# Adjusted R-squared:  0.4298185 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.4124362 (log(silomaize) - keep comIds with at least nine observations)




#################################################################
### Comparision of coefficients - yield Anomaly vs pure yield ####

coef(lm.fit_SMI_6_Jun_Aug_comId)[1:19]
coef(lm.fit_SMI_6_Jun_Aug_comId_Anomaly)[1:19]
coef(plm.fit_SMI_6_Jun_Aug_comId)
coef(plm.fit_SMI_6_Jun_Aug_comId_anomaly)

# Die Koeffizienten sind alle gleich, egal ob man Anomalien oder Absolute Werte schätzt. Das macht auch sind, da durch das demeanen der abhängigen Variable 
 nur die FE verändert werden. Diese werden größer im Anomaly Setting, da das demeanen auf der rechten Seite des Terms wegfällt und somit ein positiver term auf 
der linken Seite - wo der FE term gebildet wird - wegfällt. Des weiteren müssen FE in einen solchen Anomaly Setting immer negativ sein, da nur auf der linken Seite
in der Optimierung demeaned wird. 
Grundsätzlich sollte ich die Funktionalitäten der einzelnen Faktoren nochmals überprüfen. 
Besonder bei Temperaturen erscheinen die Koeffizienten sehr groß. 
Siehe Koeffizienten für Temp:
3.858508e+02*x  + -1.985713e+01*x² + 3.313989e-01*x³; Adjusted R-squared:  0.6857
3.785850e+01*x  + -1.169963e+00*x² ; Adjusted R-squared:  0.6832 
-5.749814e+00*x ; Adjusted R-squared:  0.6788 
no temp ; Adjusted R-squared: 0.6673

Demeaned man aber die Effekte dann sieht man zumindest bei Temperature einen ähnlichen Sensitivitätsverlauf bei im ersten Paper. Zum Beispiel habe ich eine Regressionsfunktion geschätzt,
welche nur aus einem Polynom dritten Grades für Temperature besteht und den durchschnittlichen FE abgezogen. 

Die Veränderungen in den Skalen werden dann durch FE absorbiert. 
Daraus ergeben sich für mich mehrere Fragen: Was messen die FE generell. Warum verändern sich die Skalen so sehr, wenn man Polynome höheren Grades benutzt. Das erste 
Polynom wird jeweils um den Faktor zehn größer wenn man ein weiteres Grad hinzufügt. 
Grundsätzlich sind es (a) die durchschnittlich zu erwartenden Erträge des Zeitraumes 1999 - 2015 + (b) durchschnittlich zu erwartende Erträge assoziert mit den Variablen, 
für die man Kontrolliert, also den "nicht-variablen" - zeitkonstanten Teil der Variablen + (c) Intercept der optimierten Gleichung. 
Das ist zumindest bei linearen Termen die Interpretation. 

Dabei ist (a) immer ein postiver term in den FE in unseren Setting, da Ertrage positiv definiert sind. Dieser positive term fällt in einem Anomaly setting immer raus.
Die anderen Terme hängen davon ab, ob die Variablen einen durchschnittlich positiven oder negativen Einfluss haben. So ist der FE bei reiner SMI_Aug Konfig positiv 
(13.09219), während er bei reiner SMI_Jun negativ ist (-12.89967). 

Die Fixed Effecte sind aber auch der Intercept, also der offset of der Y-Achse wenn x = 0. Dieser offset ist bei Panel Modellen für jede Gruppe unterschiedlich.
Schaut man sich die lm Konfiguration an, dann markiert der Intercept den Referenzwert und die Koeffizienten der Gruppenspezifischen Dummies die Abweichung davon. 
Dieser Referenzwert wird in der Regel der ersten Gruppe in den Daten zugeordnet. Die Summe aus Intercept und dieser Abweichung sind die FE in plm. Entsprechend ist
der FE der ersten Gruppe der gleich dem Intercept aus lm.
Dieser Intercept wird -  gemeinsam mit den Koeffizienten - so gewählt, dass die Funktion für den Datenbereich ab besten fitted. Entsprechend können die Werte im 
Intercept und den Koeffizienten sehr groß werden, obwohl die Sensitivität im Datenbereich selbt nicht so stark ausgeprägt ist. 

## Conclusions ##
# Detrendenden has basically no effect - makes sense since no trend was found
# Combined models always have a better adjusted R-square (smalles difference for log(silomaize) - keep comIds with at least nine observations))
# plm /lsdv of adjusted R-square always larger for combined models -> more variation in combined model is explained by meteorology and smi comapred to the fixed effects
# when dropping all comIds with missing observation, the R-square is larger compared to when using a cut of nine. 
# no model improvement when using log yield instead of yield 
# The coefficients do not change when using anomalies instead of absolute yield values. However, the R2 gets smaller accordingly and is the same in lm as in plm. The
reason is the smaller SS_tot in R2. The difference in the FE is equal to the group specific mean of yield. Thus, calculating first the anomalies and the predicting
should give the same results compared to predicting the absolute values and then demeaning it. 
# Polynomials can cause large FE and have large coefficients. The reason is that the FE also represents the intercept, which might be large to allow the best fit for
the data. Accordingly, the coefficients get large. 
'

################################
#### Explore Fixed Effects ####
##############################

#######################################################
#### Comparision of fixed effects Yield vs Anomaly ####
all(fixef(plm.fit_SMI_6_Jun_Aug_comId)== fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly))
' Die Fixed Effecte ändern sich. Die FE_abs größer als die FE_anomaly (beide im negativen Bereich). Der Grund ist, dass jede Variable entsprechend ihres
ortspezifischen Durschnitts demeaned wird. Das heißt bei absoluten Werten würde auf der linken Seite der Gleichung ein Term abgezogen werden. 
 Entsprechend wird dieser term den FE hinzugefügt. Dieser positive term fällt bei Anomalien weg, da dort nichts mehr demeanded werden muss. Entsprechend sind 
dort die FE kleiner, da nur noch alle anderen Variablen auf der linken Seite demeanend werden (negative terme)'


## Retrieve averaeg comId yield mean ##
Maize_meteo_year <- Maize_meteo %>% filter(year == 1999 ) %>% select(comId, avgYield_comId )

#### Compare difference between absolute FE and anomaly FE ####
head(fixef(plm.fit_SMI_6_Jun_Aug_comId) - fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly))
head(Maize_meteo_year)

' The difference between the Fixed Effects is exactly the comId specific average of crop yield. This is the amount the
anomaly FE are smaller. That means the rest of the FE can be associated entirely to the mean effects of meteorological and SMI Variables - i.e. the 
demeaning of those. That means, with no demeaning in the '

###############################
#### Retrieve Fixed Effects####
Fixed_Effects_combined <- as.data.frame(fixef(plm.fit_SMI_6_Jun_Aug_comId))
Fixed_Effects_combined$RS <-(rownames(Fixed_Effects_combined))
names(Fixed_Effects_combined)[1] <- "FixedEffects"
Fixed_Effects_combined$model <- as.factor(rep("Absolute Yield-Setting - plm", 334))
str(Fixed_Effects_combined)


Fixed_Effects_combined_anomaly <- as.data.frame(fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly))
Fixed_Effects_combined_anomaly$RS <-(rownames(Fixed_Effects_combined_anomaly))
names(Fixed_Effects_combined_anomaly)[1] <- "FixedEffects"
Fixed_Effects_combined_anomaly$model <- as.factor(rep("Yield Anomaly-Setting- plm", 334))
str(Fixed_Effects_combined_anomaly)

Fixed_Effects <- rbind(Fixed_Effects_combined_anomaly, Fixed_Effects_combined)
str(Fixed_Effects)

#####################################################
#### Check summaries of the two FE Distributions ####
summary(as.vector(Fixed_Effects_combined$FixedEffects))
summary(as.vector(Fixed_Effects_combined_anomaly$FixedEffects))

sd(as.vector(Fixed_Effects_combined$FixedEffects))
sd(as.vector(Fixed_Effects_combined_anomaly$FixedEffects))
' Offensichtlich sind die FE von anomaly zum einen kleiner als die absoluten (Durchschnitt), zum anderen haben diese auch eine wesentlich kleinere Varianz.
Die FE sind grundsätzlich auch alle negativ auf der rechten Seite. Das heißt, wenn sie auf der Linken Seite stehen dann fügen diese etwas hinzu. Das heißt, 
bei Anomaly wird mehr hinzugefügt als bei '



########################################################
#### Compare FE of comID and intercept of lm Modell ####
Fixed_Effects_combined$FixedEffects[1] 
 coef(lm.fit_SMI_6_Jun_Aug_comId)[[1]]

###############################################################################################################
#### Retrieve Fixed Effect from LSDV framework - Intercept, i.e the euqivalent to the comId dummies in lsdv ####
Fixed_Effects_combined_lsdv <- 
  Fixed_Effects_combined %>%
  mutate(FixedEffects = c(FixedEffects) - coef(lm.fit_SMI_6_Jun_Aug_comId)[1])
Fixed_Effects_combined$model <- as.factor(rep("Absolute Yield-Setting - lm", 334))
 
str(Fixed_Effects_combined_lsdv) 

Fixed_Effects <- rbind(Fixed_Effects , Fixed_Effects_combined_lsdv)
str(Fixed_Effects)
#####################################################################################
#### Compare (Fixed Effect - Intercept) with estimated coefficients of lm Modell ####
Fixed_Effects_combined$FixedEffectsSubIntercept[2:334]
coef(lm.fit_SMI_6_Jun_Aug_comId)[20:352]
' This is the same, i.e. the intercept of the model lm is the fixed effect of comId 1001, the dummies for the other comIds are the difference. 
'

######################################################################
#### Add spatial information to Fixed_Effects_combined data.frame ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

Fixed_Effects_combined_sf <- merge(vg2500_krs, Fixed_Effects, by="RS", all.y=T)
str(Fixed_Effects_combined_sf)

#####################################
#### Make a map of Fixed Effects ####
Fixed_Effects_combined_sf_plot <- 
  ggplot(Fixed_Effects_combined_sf) + 
  geom_sf(data = vg2500_krs,fill="white") + 
  geom_sf(aes(fill = FixedEffects)) +  
  guides(fill = guide_legend(title = "Fixed Effects \n1999 - 2015 ")) + 
  facet_wrap(~model)

ggsave(paste("./figures/figures_exploratory/Train/", "FixedEffects_combined.pdf", sep=""), Fixed_Effects_combined_sf_plot , device="pdf", width=8, height= 8) 

##########################################
#### Map of Fixed Effects - Intercept ####
summary(Fixed_Effects_combined_sf$FixedEffectsSubIntercept)

Fixed_Effects_Intercept_combined_sf_plot <- 
  ggplot(Fixed_Effects_combined_sf) + 
  geom_sf(data = vg2500_krs,fill="white") + 
  geom_sf(aes(fill = FixedEffectsSubIntercept)) +  
  guides(fill = guide_legend(title = "FE (plm) - Intercept (lm), \ni.e. comId Dummies (lm)  \n(1999 - 2015)")) 


ggsave(paste("./figures/figures_exploratory/Train/", "FixedEffects_Intercept_combined.pdf", sep=""), Fixed_Effects_combined_sf_plot , device="pdf", width=8, height= 8) 


#################################################################################################################################################################
#### Predictions on observational data - annual junks ####
#################################################################################################################################################################

#######################################################################
#### Loop through those models to make predictions for each year #####
#####################################################################

######################
#### Prepare loop ####
modelList <- list(lm.fit_SMI_6_Jun_Aug_comId, lm.fit_SMI_6_Jul_comId)
modelListAnomaly <- list(lm.fit_SMI_6_Jun_Aug_comId_anomaly, lm.fit_SMI_6_Jul_comId_anomaly)
modelListNames <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jul_comId")


str(modelList,1 )
str(lm.fit_SMI_6_Jun_Aug_comId)

################################################
#### Load shape of administrative districts ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"

##############################################
#### Start loop thorugh predictive models ####
for (i in 1:2){
    dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)

  ######################################################################################################################################
  #### Loop thorugh 1999 to 2015 to make predictions for each year (stored in predictData_train and predictData_train_anomaly) ####
  
  ######################
  #### Prepare loop ####
  ## Set years to loop through ##
  listyear <- seq(1999, 2015)
  
  ## Create container to store predicted data of each annual chunk ##
  predictData_train_anomaly_allyears  <- data.frame()

  #### Choose color Setting for plotting ####
  ## Absolute
  # summary(predictData_train_sf)
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_abs <- scale_fill_gradientn("Yield Deviation", colours = myPalette(100), limits=c(200,700))
  
  ## Anomaly
  myPalette_anomaly <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_anomaly <- scale_fill_gradientn("Yield Deviation", colours = myPalette_anomaly(100), limits=c(-150, 150))
  
  
  ## Start loop trough 17 years in training data ##
    for(m in 1:17){
      ## Create data.frame for the year m ##
      Maize_meteo_year <- Maize_meteo %>% filter(year == listyear[[m]] )
      str(Maize_meteo_year)
      Maize_meteo_year$comId
      
      ################################################
      #### Predict YIELD model on that data.frame ####
      predict_year <- as.data.frame(predict(modelList[[i]], newdata = Maize_meteo_year ))
      names(predict_year) <- "Yield_abs"
      
      ## Cbind time and Spatial Information ##
      predict_year <- cbind(Maize_meteo_year[,c(1:2)], predict_year)
      str(predict_year)
      
      ###############################################################
      #### Predict YIELD ANOMALY (POST) model on that data.frame ####
      predict_year_anomaly_post  <- as.data.frame(as.numeric(predict_year$Yield_abs) - as.numeric(Maize_meteo_year$avgYield_comId))
      names(predict_year_anomaly_post) <- "Yield_anomaly_expost"
      str(predict_year_anomaly_post)
      
      ## Cbind Spatial Information ##
      predict_year_anomaly_post <- cbind(Maize_meteo_year[,c(1:2)], predict_year_anomaly_post)
      str(predict_year_anomaly_post)

      ##############################################################
      #### Predict YIELD ANOMALY (PRE) model on that data.frame ####
      predict_year_anomaly_pre  <- as.data.frame(predict(modelListAnomaly[[i]], newdata = Maize_meteo_year))
      names(predict_year_anomaly_pre) <- c("Yield_anomaly_exante")
      
      
      ## Cbind Spatial Information ##
      predict_year_anomaly_pre <- cbind(Maize_meteo_year[,c(1:2)], predict_year_anomaly_pre)
      str(predict_year_anomaly_pre)

      ############################################
      #### Compare expost and exant anomalies ####
      head( predict_year_anomaly_post)
      head( predict_year_anomaly_pre)
      'Both have exactly the same results -> only work with ex-ante yield anomalies !!!!!!!!!!!!!!!!!!!!!!!!!!'

      ####################################################
      #### Define PRE setting as standard for anomaly ####
      predict_year_anomaly  <- predict_year_anomaly_pre
      str(predict_year_anomaly)
      names( predict_year_anomaly)[3] <-"Yield_anomaly"
      
      # ################################
      # #### Change names to year m ####
      # names(predict_year) <-  c("comId", "year", "com", listyear[[m]])
      # names(predict_year_anomaly ) <- c("comId", "year", "com",  paste(listyear[[m]],"anomaly",sep="_"))
      # 
      #########################################################################################################
      #### Append to create a large data.frame including all comIds (334) and both yield and yield anomaly ####
      ## Create containers with comIds from external source including all 334 comId used ##
      predictData_train <- as.data.frame(avgYield_comId$comId)
      colnames(predictData_train) <- "comId"
      str(predictData_train)
      
      #### Merge predicted data with those containers representing 334 comIds ####
      predictData_train <- merge(predictData_train, predict_year, by="comId", all.x=T)
      str(predictData_train)
      
      #### Merge this data.frame with anomaly data ####
      # str(predictData_train)
      # str( predict_year_anomaly)
      predictData_train_anomaly <- merge( predictData_train , predict_year_anomaly , by=c("comId","year"), all.x=T)
      str(predictData_train_anomaly)
      
      ##############################
      #### Plot predicted data ####
      ############################
      
      ####################################
      #### Create spatial data.frame #####
      predictData_train_sf <- NULL
      predictData_train_sf <- merge(vg2500_krs, predictData_train_anomaly, by="comId")
      str(predictData_train_sf)
      names(predictData_train_sf) <- gsub("X","", names(predictData_train_sf))
      # names(predictData_train_sf) <- gsub(".y","", names(predictData_train_sf))

      # predictData_train_sf$year.x <- NULL
      
      str(predictData_train_sf )

      ## Anomaly
      predictData_train_sf_anomaly_plot  <- 
        ggplot(   predictData_train_sf) + 
        geom_sf(data=vg2500_krs, fill="gray", color="white")  + 
        geom_sf(aes(fill =  predictData_train_sf$Yield_anomaly ))  +  
        guides(fill = guide_legend(title = "Predicted Yield Anomaly")) + 
        sc_anomaly +
        ggtitle(paste(listyear[m])) + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) 
      
      ## Absolut
      predictData_train_sf_absolut_plot  <- 
        ggplot(   predictData_train_sf) + 
        geom_sf(data=vg2500_krs,fill="gray", color="white") + 
        geom_sf(aes(fill =  predictData_train_sf$Yield_abs ))  +  
        guides(fill = guide_legend(title = "Predicted Yield")) + 
        sc_abs + 
        ggtitle(paste( listyear[m] ))  + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) 
  
      
      #### Save the plots ####
      ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf", sep=""),predictData_train_sf_anomaly_plot , device="pdf", width=8, height= 8) 
      ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_", listyear[m],".pdf", sep=""),     predictData_train_sf_absolut_plot, device="pdf", width=8, height= 8) 
   
      ########################################################
      #### Create on large data frame including all years ####
      names(predictData_train_anomaly) <- c( "comId", "year", "yield_predicted", "yield_anomaly_predicted")
      predictData_train_anomaly_allyears <- rbind(predictData_train_anomaly_allyears ,  predictData_train_anomaly  )
      dim(predictData_train_anomaly_allyears)
      dim(predictData_train_anomaly)
    } ## End of loop through 17 years in training data

  
  ###########################################################################################################################
  #### Export the data.frame including the maize yield and maize yield deviations (anomalies) of the period 1999 - 2015 ####
  #########################################################################################################################
  str(predictData_train_anomaly_allyears)
  write.csv(predictData_train_anomaly_allyears, paste("./data/data_processed/Train/", modelListNames[[i]], "/Yield_predict_allYears.pdf", sep="" ))
  
} # Close loop through to predictive models
rm(list=ls())

###############################################################################################
#### Make Plots of sums the anomalies within each comID and the time series of each comID ####
#############################################################################################

################################################
#### Prepare loop thorugh predictive models ####
modelListNames <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jul_comId")

################################################
#### Load shape of administrative districts ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"

#### Read in avgYield_comId to extract 334 coms ####
avgYield_comId <- read.csv( file="./data/data_processed/avgYield_comId.csv")
avgYield_comId$X <- NULL
str(avgYield_comId)


#### Make comId list and comIdName list ####
comList <- merge(avgYield_comId, vg2500_krs, by="comId")
str(comList)

comId_list <- comList$comId
str(comId_list)
comIdName_list <- comList$GEN
str(comIdName_list)

##############################################
#### Start loop thorugh predictive models ####
for (i in 1:2){
  dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)
  
  
  predictData_train_anomaly_allyears <- read.csv(paste("./data/data_processed/Train/", modelListNames[[i]], "/Yield_predict_allYears.pdf", sep="" ))
  predictData_train_anomaly_allyears$X <- NULL
  str(predictData_train_anomaly_allyears)
  
  ############################################################################
  #### Add spatial information - make sf data.frame to allow map plotting ####
  predictData_train_anomaly_allyears_sf <- merge(vg2500_krs, predictData_train_anomaly_allyears, by = "comId") 
  str(predictData_train_anomaly_allyears_sf)
  
  ##########################################################
  #### Calculate sums of the predictions for each comId ####
  predictData_train_sums <- 
    predictData_train_anomaly_allyears_sf %>% 
    group_by(comId)  %>% 
    summarise(comIdsums = sum(yield_anomaly_predicted))
  
  summary(predictData_train_sums )
  str(predictData_train_sums )
  
  ##########################################################################
  #### Plot sums of Predictions over years of each comId in train Data ####
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn(colours = colorRampPalette((brewer.pal(11, "BrBG")))(100))
  
  predictData_train_sums_plot <- 
      ggplot(predictData_train_sums) + 
      geom_sf(aes(fill = comIdsums)) + 
      ggtitle("Sums of annual predictions")  + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
   sc
    
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf", sep=""),  predictData_train_sums_plot , device="pdf", width=8, height= 8) 

  
  ###########################################################################
  #### Loop to plot time series of yield and yield anomaly of each year ####
  #########################################################################
  for (r in 1:length(comId_list)){
    
    ## Filter for each comID
    predictData_train_anomaly_allyears_year   <-
      predictData_train_anomaly_allyears  %>%
      filter(comId == comId_list[[r]])
    
    #### Plot yield ####
    timeseries <- ggplot(predictData_train_anomaly_allyears_year , aes(year, yield_predicted  )) +
      # ylim(250, 650) +
      geom_point(size=0.5, color="grey")    +
      # # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
      geom_hline(aes(yintercept = mean(yield_predicted ))) +
      geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5)    +
      geom_smooth(color="green", se = FALSE,fill="red", size=1.5)      +
      # # geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
      ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
      theme_minimal()   +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Silage Maize Yield")
    
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yield_", comId_list[r], ".pdf", sep=""),
           plot = timeseries , width=14, height=8)
    
    #### Plot yield anomalies ####
    timeseries_anomaly <- ggplot(predictData_train_anomaly_allyears_year , aes(year, yield_anomaly_predicted  )) +
      ylim(-200, 200) +
      geom_point(size=0.5, color="grey") + 
      # stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)     +  
      geom_hline(aes(yintercept = mean(yield_anomaly_predicted))) +
      geom_smooth(method = "lm", se = FALSE, color="orange", size=1.5) +
      geom_smooth(color="green", se = FALSE, fill="red", size=1.5) +
      # geom_quantile(quantiles = c(0.1, 0.9), method = "rqss", lambda = 80, size=1.5) +
      ggtitle(paste(comId_list [[r]], "-" , comIdName_list[[r]])) +
      theme_minimal() +  
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Silage Maize Yield Anomaly")
    
    ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_", comId_list[r], ".pdf", sep=""),
           plot = timeseries_anomaly , width=14, height=8)
  } ## End of Loop to produce time series  
  

} ## END OF LOOP WHICH LOOPS THROUGH THE DIFFERENT PREDICTIVE MODELS 


rm(list=ls())


