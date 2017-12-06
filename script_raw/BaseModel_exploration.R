#############################
#### Data Pre-Processing ####
#############################

#### Description of Script ####
' 
- Explore Models
- many results, please see specific section
-  Explore differences on estimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities 
- many results, please go to section
- Use of Caret Package: I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
account one fold for each year. The implementation can be found in BasePrediction_long
- Retrieve anomaly correction: comId specific mean siloMaize 
The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
This is particular important for  the plotting acticities (corrected for average expected silage maize). 

'

#### Output ####
## Files
' 

'
## Plots
'
- 
- Map of sum of yield_anomalies for each comId: Yield_predict_sumsComId.pdf <- /figures_exploratory/Train/", modelListNames[[i]]

'

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
'  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)

'



###################
## Load Packages ##
source("./script/script_raw/Packages.R")

##############################################################################################################################################################################




# ##################################################################################################################################################################
# ################################### Results####### ###############################################################################################################
# ##################################################################################################################################################################

'
Caveat: Die Explorative Analyse war für den Datensatz welcher nur comIds mit vollständigen Report berücksichtigt hat
Ich habe hier eine explorative Analyse verschiedener Modelle gemacht. Der Code zu der Analyse befindet sich in BasePrediction_long.R .
 Vor allem habe ich mit stepwise in 4 Stufen und Polynomen von SMI gespielt.
Ergebnisse sind
- Interessant ist, dass wenn man für die Meteorologie kontrolliert (P, T im July), die SMI wet im August an Significanz verlieren
  (Adj. R-Squared: 0.34778 fpr diese Modell (SMIJun6, Pol3TPJun, SMIAug6)
- Wenn mann eine stepwise function mit nur 4 steps nimmt, dann sind die Ergebnisse vergleichbar (Adj. R-Squared: 0.34353)
- Interactionsterme zwischen SMIJun4 und SMIJun4 liefern schlechtere Ergebnisse als mit Meteoroligie
(Adj. R-Squared: 0.23835 ohne T und P July): Es scheint nicht möglich zu sein für
die Meteorologie in July via SMI-Intaction Jun und Ug zu kontrollieren.
- Wenn ich neben Interaction SMI auch für Meteorologie im July reagieren habe ich ein Adj. R-Squared: 0.34989 für Polynome 3. Grades
  und 0.35036 für Polynome 4. Grades in P and T
- Polynime statt stepwise functions für den SMI verbessern die Ergebnnise auch nicht :
# # Adj. R-Squared: 0.34989, Polynome 3. Grades in P and T and SMI
# # Adj. R-Squared: 0.35036, Polynome 4. Grades in P and T , SMi 3. Grad
# # Adj. R-Squared: 0.35595, Polynome 4. Grades in P and T and SMI
-  Meteorologie im July verbessert die vorhersagekraft im sample sehr.
'

#############################################################################################################################################################################
#############################################################################################################################################################################
rm(list=ls())
getwd()

##################################
#### Read in Maize_meteo Data ####
Maize_meteo <- read_csv( file="./data/data_processed/Maize_meteo.csv")
Maize_meteo
# Maize_meteo$X1 <- NULL
# str(Maize_meteo)
# names(Maize_meteo)

#############################################################
#### Make factors necessary for statistical applications ####
Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")] <- 
  lapply(Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")], factor )

levels(Maize_meteo$SMI_Aug6)

##########################
#### Relevel SMI data ####

## set normal as reference ##
Maize_meteo$SMI_May6 <- relevel(Maize_meteo$SMI_May6, ref= "nrml") 
Maize_meteo$SMI_Jun6 <- relevel(Maize_meteo$SMI_Jun6, ref= "nrml") 
Maize_meteo$SMI_Jul6 <- relevel(Maize_meteo$SMI_Jul6, ref= "nrml") 
Maize_meteo$SMI_Aug6 <- relevel(Maize_meteo$SMI_Aug6, ref= "nrml") 
Maize_meteo$SMI_Sep6 <- relevel(Maize_meteo$SMI_Sep6, ref= "nrml") 
Maize_meteo$SMI_Oct6 <- relevel(Maize_meteo$SMI_Oct6, ref= "nrml") 

## order drght categories ##
Maize_meteo$SMI_May6 <- fct_relevel(Maize_meteo$SMI_May6, "drght_svr", after =1)
Maize_meteo$SMI_Jun6 <- fct_relevel(Maize_meteo$SMI_Jun6, "drght_svr", after =1)
Maize_meteo$SMI_Jul6 <- fct_relevel(Maize_meteo$SMI_Jul6, "drght_svr", after =1)
Maize_meteo$SMI_Aug6 <- fct_relevel(Maize_meteo$SMI_Aug6, "drght_svr", after =1)
Maize_meteo$SMI_Sep6 <- fct_relevel(Maize_meteo$SMI_Sep6, "drght_svr", after =1)
Maize_meteo$SMI_Oct6 <- fct_relevel(Maize_meteo$SMI_Oct6, "drght_svr", after =1)
# 
levels(Maize_meteo$SMI_May6)
levels(Maize_meteo$SMI_Jun6)
levels(Maize_meteo$SMI_Jul6)
levels(Maize_meteo$SMI_Aug6)
levels(Maize_meteo$SMI_Sep6)
levels(Maize_meteo$SMI_Oct6)

############################################################################################################################################################################
############################
#### Model Exploration ####
##########################
#############################################################################################################################################################################

################################################################################################################################
#### Explore differences on extimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities #####
##############################################################################################################################
' Diese Sektion ist überholt und kann nun gegebenfalls in /old/BaseModel_exploration_Sep2017 eingesehen werden. '

###############################################################################################################################################################################
# ############################
# #### use caret package ####
# ##########################
###############################################################################################################################################################################
' I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
account one fold for each year. The implementation can be found in BasePrediction_long'


###################################################################################################################################
#### Further Explore various variable selections and modelling approaches (LSDV vs plm vs pure anomaly and demeaning setting) ####
#################################################################################################################################


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
summary(lm.fit_SMI_6_Jun_Aug_comId )
coef(lm.fit_SMI_6_Jun_Aug_comId)[1:19 ]
plot(lm.fit_SMI_6_Jun_Aug_comId)

Maize_meteo[c(815,311,1077),]
View(Maize_meteo%>% filter(comId %in% c(5158, 5374,5358 )))
' There are some outliers when looking at the data -> delete in BaseData_precossing '


#########################################
#### Fit combined model - log(yield) ####
lm.fit_SMI_6_Jun_Aug_comId_log <- lm(log(siloMaize) ~ 
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

# summary(lm.fit_SMI_6_Jun_Aug_comId_log )
par(mfrow=c(2,3))
plot(lm.fit_SMI_6_Jun_Aug_comId_log, main="log-siloMaize", which=c(1:6))
plot(lm.fit_SMI_6_Jun_Aug_comId, main="level-siloMaize", which=c(1:6))

#### Compare residuals of log and level ####
hist(resid(lm.fit_SMI_6_Jun_Aug_comId ), n=1000)
hist(resid(lm.fit_SMI_6_Jun_Aug_comId_log ), n=1000)

'In der log config werden die niedrigen siloMaize Werte schlechter erklärt als in der level. Aber es gibt weniger Probleme mit outliern im positiven Bereich. '


###############################################################
#### Fit combined model - yield - meteorological anomalies ####
lm.fit_SMI_6_Jun_Aug_comId_demean <- lm(siloMaize ~ 
                                 + T_Jul_demeaned 
                                 + I(T_Jul_demeaned^2) 
                                 + I(T_Jul_demeaned^3) 
                                 + P_Jul_demeaned 
                                 + I(P_Jul_demeaned^2) 
                                 + I(P_Jul_demeaned^3) 
                                 + SMI_Jun6 
                                 + SMI_Aug6 
                                 + comId
                                 ,
                                 data = Maize_meteo)

# summary(lm.fit_SMI_6_Jun_Aug_comId_demean)  
# plot(lm.fit_SMI_6_Jun_Aug_comId_demean)  

############################################
#### Fit combined model - yield anomaly ####
lm.fit_SMI_6_Jun_Aug_comId_anomaly <- lm(siloMaizeAnomaly ~ 
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
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly)
coef(lm.fit_SMI_6_Jun_Aug_comId_anomaly)[1:19 ]

# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly)

' When considering siloMaizeAnomalies the comId, ie the fixed effects, get insignificant. The coefficients change not significantly.  '

#############################################################
#### Compare to pure anomaly and demeaning settting - lm ####
lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                          + T_Jul_demeaned 
                                          + I(T_Jul_demeaned^2)
                                          + I(T_Jul_demeaned^3)
                                          + P_Jul_demeaned  
                                          + I(P_Jul_demeaned^2)
                                          + I(P_Jul_demeaned^3)
                                          + SMI_Jun6 
                                          + SMI_Aug6 
                                          + comId
                                          ,
                                          data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean) # Adjusted R-squared:  0.3262 

' In all the settings evaluated before it is not possible to explain the very extremes in the silage Maize distribution (both ends). 
  Durch nutzen der Anomalien sind die comIds nicht mehr relevant. Vergleicht man Modelle mit comId und ohne, dann ändert sich quasi nichts an den Modellen. '

par(mfrow=c(2,3))
# plot( lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean  , main = "comId", which = c(1:6))

#### Check out distribution of residuals and standardized residuals ####
# hist(resid(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean), nclass= 1000)
# hist(stdres(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean), nclass= 1000)
' Looks good - almost normally distributed. '

## Summary statistic of the FE ##
coef(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)[20:length(coef(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean))]
intercept = 1.479415e+01 
summary(coef(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)[20:length(coef(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean))] + intercept)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 8.133  15.418  18.136  18.779  21.876  31.391 
'

#########################################################
#### Compare to pure anomaly settting - lm - noMeteo ####
lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean  <- lm(siloMaizeAnomaly ~ 
                                          # + T_Jul_demeaned 
                                        # + I(T_Jul_demeaned^2) 
                                        # + I(T_Jul_demeaned^3) 
                                        # + P_Jul_demeaned  
                                        # + I(P_Jul_demeaned^2) 
                                        # + I(P_Jul_demeaned^3) 
                                        + SMI_Jun6 
                                        + SMI_Aug6 
                                        + comId
                                        ,
                                        data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean )
# plot(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean)
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

' Die Erklärungskraft sinkt auf R2= 0.2136. Vor allem im extremem Bereich erreicht man dadurch mehr Erklärungskraft. Die fitted values sind 
weiter gestreut, währendd die Verteilung der Residuals kleiner ist in der Standard Config mit July Meteorologie. Es scheint also der Fall zu sein,
dass vor allem der July die Extreme erklärt. Hier wäre nun wirklich interessant - aber wohl nicht machbar -, wie es sich mit T_max verhalten würde.

Die Koeffizienten ändern sich auch, wenn mann Meteorologie nicht beachtet. Für Juni SMI werden alle Koeffizienten größer. Die August dry koeffizienten werden auch
größer. Die wet koeffizienten bekommen ein größere Signifikanzniveau, und werden positiv. 
'
## Summary statistic of the FE ##
coef(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean)[14:length(coef(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean))]
intercept =  3.26823 
summary(coef(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean)[14:length(coef(lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean))] + intercept)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -5.057   2.271   5.570   6.490   9.440  22.837 
'

#########################################################
#### Compare to pure anomaly settting - lm - no Prec ####
lm.fit_SMI_6_Jun_Aug_noPrec_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                                   + T_Jul_demeaned
                                                 + I(T_Jul_demeaned^2)
                                                 + I(T_Jul_demeaned^3)
                                                 # + P_Jul_demean
                                                 # + I(P_Jul_demean^2)
                                                 # + I(P_Jul_demean^3)
                                                 + SMI_Jun6
                                                 + SMI_Aug6
                                                 + comId
                                                 ,
                                                 data = Maize_meteo)
# summary(lm.fit_SMI_6_Jun_Aug_noPrec_anomaly_demean) #
# plot(lm.fit_SMI_6_Jun_Aug_noPrec_anomaly_demean) 
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

' Explanatory losses not too much. Extremes might be a littles less explained. Here, the coefficients might be very interesting.  '

#########################################################
#### Compare to pure anomaly settting - lm - no Temp ####
lm.fit_SMI_6_Jun_Aug_noTemp_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                                 #   + T_Jul_demeaned
                                                 # + I(T_Jul_demeaned^2)
                                                 # + I(T_Jul_demeaned^3)
                                                 + P_Jul_demeaned
                                                 + I(P_Jul_demeaned^2)
                                                 + I(P_Jul_demeaned^3)
                                                 + SMI_Jun6
                                                 + SMI_Aug6
                                                 + comId
                                                 ,
                                                 data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_noTemp_anomaly_demean) #
# plot(lm.fit_SMI_6_Jun_Aug_noTemp_anomaly_demean) 
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

'Explanatory loss is rather high: adjusted R-square is 0.2693. Extremes might be a littles less explained. Here, the coefficients might be very interesting.  '


#######################################################
#### Compare to pure anomaly settting - lm - noSMI ####
lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                            + T_Jul_demeaned
                                            + I(T_Jul_demeaned^2)
                                            + I(T_Jul_demeaned^3)
                                            + P_Jul_demeaned
                                            + I(P_Jul_demeaned^2)
                                            + I(P_Jul_demeaned^3)
                                            # + SMI_Jun6
                                          # + SMI_Aug6 
                                          + comId
                                          ,
                                          data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean)

## Summary statistic of the FE ##
intercept = 1.217e+01
summary(coef(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean)[8:length(coef(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean))] + intercept)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
5.649  12.441  14.271  14.647  16.611  29.380
Intercept: 1.217e+01'

## Compare Plots ##
# plot(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean )
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)


' Explanatory Power is lower: adj. r2 0.2599. Extremes might be explained less.   '

# hist(stdres(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean), nclass= 1000)
# hist(stdres(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean), nclass= 1000)

##################################################################
#### Compare to pure anomaly settting - lm - noSMI onlyLinear ####
lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear <- lm(siloMaizeAnomaly ~ 
                                          + T_Jul_demeaned
                                        # + I(T_Jul_demean^2)
                                        # + I(T_Jul_demean^3)
                                        + P_Jul_demeaned
                                        # + I(P_Jul_demean^2)
                                        # + I(P_Jul_demean^3)
                                        # + SMI_Jun6
                                        # + SMI_Aug6 
                                        + comId
                                        ,
                                        data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear)
# plot(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear)
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)
' Explantory Power is less: adj r2 is 0.2146. In particular the spread on the negative silage maize anomalies is lower. Lower extremes are less
explained in the model. '

## Summary statistic of the FE ##
coef(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear)
intercept = 2.212063
summary(coef(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear)[4:length(coef(lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear))] + intercept)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-2.968   3.029   4.601   5.009   6.759  17.484  
'


#######################################################################
#### Compare to pure anomaly settting - lm - September/October SMI ####
lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                            + T_Jul_demeaned
                                          + I(T_Jul_demeaned^2)
                                          + I(T_Jul_demeaned^3)
                                          + P_Jul_demeaned
                                          + I(P_Jul_demeaned^2)
                                          + I(P_Jul_demeaned^3)
                                          + SMI_Jun6
                                          + SMI_Aug6
                                          + SMI_Sep6
                                          + comId
                                          ,
                                          data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean)

# plot(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean)
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

# ## Compare histograms of studentized residuals ##
# p1 <- hist(stdres(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean), nclass= 50)
# p2 <- hist(stdres(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean), nclass= 50)
# plot( p2, col="red", xlim=c(-10,10))  # first histogram
# plot( p1, col="green", xlim=c(-10,10), add=T)
# ' When plotting the studentized residualas there appear to be not too many differences. '
# 
# par(mfrow=c(2,1))
# hist(fitted(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean), n=100, xlim=c(-150, 50), ylim=c(0,200))
# hist(fitted(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean), n=100, xlim=c(-150, 50), ylim=c(0,200))
# 
# #### Compare by Anova ####
# anova(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean, lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean)
# anova(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean, lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean )
# ' Both models do not fit the data equally well. Since the adj. R2 in the model with October is better, this is evidence to include it.'
# 
# #### Perform stepAIC on this model ####
# step <- stepAIC(lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean, direction="both")
# step$anova
' Durch das Hinzufügen von September oder Oktober SMI wird die Erklärungskraft nur unwesentich erhöht. Des Weiteren sind die Koeffizienten eher schwer zu 
erklären. Daher würde ich SMI für diesen Monat rausnehmen. '

#######################################################################
#### Compare to pure anomaly settting - lm - September/October Temp ####
lm.fit_SMI_6_Jun_Aug_TSep_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                                    + T_Jul_demeaned
                                                  + I(T_Jul_demeaned^2)
                                                  + I(T_Jul_demeaned^3)
                                                  + P_Jul_demeaned
                                                  + I(P_Jul_demeaned^2)
                                                  + I(P_Jul_demeaned^3)
                                                  + SMI_Jun6
                                                  + SMI_Aug6
                                                  + T_Sep_demeaned
                                                  + I(T_Sep_demeaned^2)
                                                  + I(T_Sep_demeaned^3)
                                                  # + comId
                                                  ,
                                                  data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_TSep_anomaly_demean)
summary(lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

par(mfrow=c(2,2))
plot(lm.fit_SMI_6_Jun_Aug_TSep_anomaly_demean, main="Sep T")
plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean, main="Std. Anomaly Demean")
plot(lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean, main="Std. Anomaly Demean - noComId")

' Es könnte sein, dass durch das hinzufügen von T_Sep die positiven Anomalien ein wenig besser erklärt werden. Aber es ergibt sich auch kein rießen Unterschied. '




+ P_Jun_demeaned
+ I(P_Jun_demeaned^2)
+ I(P_Jun_demeaned^3)


##########################################################
#### Compare to pure anomaly settting - lm - no ComId ####
lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                           + T_Jul_demeaned
                                         + I(T_Jul_demeaned^2)
                                         + I(T_Jul_demeaned^3)
                                         + P_Jul_demeaned
                                         + I(P_Jul_demeaned^2)
                                         + I(P_Jul_demeaned^3)
                                         + SMI_Jun6
                                         + SMI_Aug6 
                                         # + poly(T_May, 1, raw=T)
                                         # + poly(T_Sep, 3, raw=T)
                                         # + poly(P_May, 1, raw=T)
                                         # + poly(P_Sep, 3, raw=T)
                                         
                                         # + SMI_May6
                                         ,
                                         data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean )
par(mfrow=c(2,3))
plot( lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean , main = "noComId", which = c(1:6))


# par(mfrow=c(2,2))C
# plot(lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean)
# plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)

' Explanatory power is slightly better: adj, r2 is 0.3608. As soon as there are siloMaize anomalies involved, there is no need for comId anymore. The 
coefficients change not significantly. '


###########################################################
#### Compare to pure anomaly settting - lm - July only ####
lm.fit_SMI_6_JulyOnly_anomaly_demean <- lm(siloMaizeAnomaly ~ 
                                          + T_Jul_demeaned
                                          + I(T_Jul_demeaned^2)
                                          + I(T_Jul_demeaned^3)
                                          + P_Jul_demeaned
                                          + I(P_Jul_demeaned^2)
                                          + I(P_Jul_demeaned^3)
                                          + SMI_Jul6
                                          
                                          ,
                                          data = Maize_meteo)
summary(lm.fit_SMI_6_JulyOnly_anomaly_demean)
plot(lm.fit_SMI_6_JulyOnly_anomaly_demean, main="Std. anomaly demean - noComId")

############################
#### Compare to plm.fit ####
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
summary(plm.fit_SMI_6_Jun_Aug_comId )
fixef(plm.fit_SMI_6_Jun_Aug_comId)


#### Change plot output format ####
par(mfrow=c(2,2))


############################
#### Compare to plm.fit ####
plm.fit_SMI_6_Jun_Aug_comId_anomaly <- plm(siloMaizeAnomaly ~
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
summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly )
fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly )
' Die Erklärungskraft ist diesselbe - egal ob siloMaizeAnomaly oder keine. Die Koeffizienten sind auch unverändert.
  Fixed Effects ändern sich inder Größe der durchschnittlichen gruppenspezifischen Yields. '

# #### Explanation ####
# sd(Maize_meteo$siloMaize)
# sd(Maize_meteo$siloMaizeAnomaly)
# ' SD for silage maize is larger for the absolute levels compared to the anomalies. Thus, the adjusted R2 is smaller. 
# R2 = 1 - SS_res/SS_tot. When the Varianz in the dependent variable is larger, SS_tot is accordingly larger and thus R2 is smaller. '
# 

# 
# ##########################################################
# #### Compare plm fit with anomalies and demeaned data ####
# plm.fit_SMI_6_Jun_Aug_comId_anomaly_demean <- plm(siloMaizeAnomaly ~ 
#                                                     + T_Jul_demeaned 
#                                                   + I(T_Jul_demeaned^2) 
#                                                   + I(T_Jul_demeaned^3) 
#                                                   + P_Jul_demeaned  
#                                                   + I(P_Jul_demeaned^2) 
#                                                   + I(P_Jul_demeaned^3) 
#                                                   + SMI_Jun6 
#                                                   + SMI_Aug6
#                                                   ,
#                                                   data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly_demean)
# fixef(plm.fit_SMI_6_Jun_Aug_comId_demean)
# ' Das R-square wird nun ein wenig kleiner wenn man die meteorologischen Variablen demeaned für die Periode 1951 - 2015. 
#   Auch die Koeffizienten ändern sich leicht. 
# '

######################################################
#### Export all Regression Results via stargazer ####
####################################################
stargazer( 
  lm.fit_SMI_6_Jun_Aug_comId,
  lm.fit_SMI_6_Jun_Aug_comId_demean,
           lm.fit_SMI_6_Jun_Aug_comId_anomaly,
           lm.fit_SMI_6_Jun_Aug_comId_anomaly_demean,
           lm.fit_SMI_6_Jun_Aug_noMeteo_anomaly_demean,
           lm.fit_SMI_6_Jun_Aug_noPrec_anomaly_demean,
           lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean,
           lm.fit_SMI_6_Jun_Aug_noSMI_anomaly_demean_linear,
           lm.fit_SMI_6_Jun_Aug_SMIOct6_anomaly_demean, 
           lm.fit_SMI_6_Jun_Aug_noComId_anomaly_demean,
          type="text",
          out= "./figures/figures_exploratory/Train/RegressionResults/lm.fit_noComId.txt"
  # , 
          # column.labels = c("level","level_demean","anomaly", "anomaly_demean", "noMeteo_anomaly_demean", 
          #                   "noPrec_anomaly_demean","noSMI_anomaly_demean", "noSMI_anomaly_demean_linear", 
          #                   "SMIOct6_anomaly_demean", "oComId_anomaly_demean")
          )


####################################################
#### Draw functions for different coefficients ####
##################################################

#### Compare demeand and non demeaned meteorological data ####
summary(Maize_meteo$T_Jul)
min(Maize_meteo$T_Jul_demean)

## TavJuly
coefFunc_demean     <- function(x)  {-11.507*x - 2.064*x^2 + 0.693*x^3}
coefFunc_absolute   <- function(x)  {379.327*x - 19.523*x^2 +  0.326*x^3}
intervall_demean    <- c(summary(Maize_meteo$T_Jul_demean)[[1]], summary(Maize_meteo$T_Jul_demean)[[6]])
intervall_absolute  <- c(summary(Maize_meteo$T_Jul)[[1]], summary(Maize_meteo$T_Jul)[[6]])

#### Export plots ####
T_Jul_demean   <- ggplot(data.frame(x=intervall_demean), aes(x)) + stat_function(fun= coefFunc_demean ) + ggtitle("Temperature July - demeaned data")
T_Jul_absolute <- ggplot(data.frame(x=intervall_absolute), aes(x)) + stat_function(fun= coefFunc_absolute )+ ggtitle("Temperature July - not demeaned data")

ggsave(paste("./figures/figures_exploratory/Train/FunctionCurves/", "T_Jul_demean_functionality.pdf", sep=""), T_Jul_demean , device="pdf", width=8, height= 8) 
ggsave(paste("./figures/figures_exploratory/Train/FunctionCurves/", "T_Jul_absolute_functionality.pdf", sep=""), T_Jul_absolute , device="pdf", width=8, height= 8) 



## PreJuly 
coefFunc_demean     <- function(x)  {0.276*x -0.001*x^2 -0.00000*x^3}
coefFunc_absolute   <- function(x)  { 1.272*x - 0.008*x^2 +  0.00001*x^3}
intervall_demean    <- c(summary(Maize_meteo$P_Jul_demean)[[1]], summary(Maize_meteo$P_Jul_demean)[[6]])
intervall_absolute  <- c(summary(Maize_meteo$P_Jul)[[1]], summary(Maize_meteo$P_Jul)[[6]])

#### Export plots ####
P_Jul_demean <- ggplot(data.frame(x=intervall_demean), aes(x)) + stat_function(fun= coefFunc_demean )+ ggtitle("Precipitation July - demeaned data")
P_Jul_absolute <- ggplot(data.frame(x=intervall_absolute), aes(x)) + stat_function(fun= coefFunc_absolute )+ ggtitle("Precipitation July - not demeaned data")


ggsave(paste("./figures/figures_exploratory/Train/FunctionCurves/", "P_Jul_demean_functionality.pdf", sep=""), P_Jul_demean , device="pdf", width=8, height= 8) 
ggsave(paste("./figures/figures_exploratory/Train/FunctionCurves/", "P_Jul_absolute_functionality.pdf", sep=""), P_Jul_absolute , device="pdf", width=8, height= 8) 


################################################################################################################################################
#####################################
#### Model selection Approaches ####
###################################
################################################################################################################################################

#############################################################
#### Set global model for all model selection approaches ####
fit_step_nonlinear_demean_anomaly <-  lm(siloMaizeAnomaly ~ comId + 
                            poly(P_May_demeaned, degree=3, raw=T) +
                            poly(P_Jun_demeaned, degree=3, raw=T) +
                            poly(P_Jul_demeaned, degree=3, raw=T) +
                            poly(P_Aug_demeaned, degree=3, raw=T) +
                            poly(P_Sep_demeaned, degree=3, raw=T) +
                            poly(P_Oct_demeaned, degree=3, raw=T) +
                            poly(T_May_demeaned, degree=3, raw=T) +
                            poly(T_Jun_demeaned, degree=3, raw=T) +
                            poly(T_Jul_demeaned, degree=3, raw=T) +
                            poly(T_Aug_demeaned, degree=3, raw=T) +
                            poly(T_Sep_demeaned, degree=3, raw=T) +
                            poly(T_Oct_demeaned, degree=3, raw=T) + 
                            SMI_May6 + SMI_Jun6 + SMI_Jul6 +
                            SMI_Aug6 + SMI_Sep6 + SMI_Oct6, data=Maize_meteo)
summary(fit_step_nonlinear_demean_anomaly)

fit_step_linear_demean_anomaly <-  lm(siloMaizeAnomaly ~ comId + 
                                        P_May_demeaned +
                                        P_Jun_demeaned +
                                        P_Jul_demeaned +
                                        P_Aug_demeaned +
                                        P_Sep_demeaned +
                                        P_Oct_demeaned +
                                        T_May_demeaned +
                                        T_Jun_demeaned +
                                        T_Jul_demeaned +
                                        T_Aug_demeaned +
                                        T_Sep_demeaned +
                                        T_Oct_demeaned + 
                                        SMI_May6 + SMI_Jun6 + SMI_Jul6 +
                                        SMI_Aug6 + SMI_Sep6 + SMI_Oct6, data=Maize_meteo)
summary(fit_step_linear_demean_anomaly) # Adjusted R-squared:  0.3738 


#################################################################
#### Model selection for nonlinear anomalies using Step AIC ####
###############################################################
' Hier sehe ich das grundsätzliche Problem, dass Variablen stark miteinander korrelieren. Das macht model selection eigentlich sinnlos. 
  Des Weiteren haben die Modelle unterschiedliche Skalen und auch funktionelle Formen, was sich auch auf die relative Importance der Variablen auswirken kann.
  Die persistence in SMI (saisonale Autocorrelation), die wir also also Vorteil für Klimaprojektionen sehen, kann für model selection ein Nachteil sein.'

###############################################
#### Starting model for variable selection ####

#### Stepwise Model Selection - both directions ####
step_nonlinear <- stepAIC(fit_step_nonlinear_demean_anomaly, direction="both")

step_nonlinear$anova
'comId, SMI_Sep6, and SMI_Jul6 dropped'

#### Best model ####
fit_step_nonlinear_best_demean_anomaly <- lm(siloMaizeAnomaly ~ comId + 
                                poly(P_May_demeaned, degree = 3, raw = T) + 
                                poly(P_Jun_demeaned, degree = 3, raw = T) + 
                                poly(P_Jul_demeaned, degree = 3, raw = T) + 
                                poly(P_Aug_demeaned, degree = 3, raw = T) + 
                                poly(P_Sep_demeaned, degree = 3, raw = T) + 
                                poly(P_Oct_demeaned,degree = 3, raw = T) + 
                                poly(T_May_demeaned, degree = 3, raw = T) + 
                                poly(T_Jun_demeaned, degree = 3, raw = T) + 
                                poly(T_Jul_demeaned, degree = 3, raw = T) + 
                                poly(T_Aug_demeaned, degree = 3, raw = T) + 
                                poly(T_Oct_demeaned, degree = 3, raw = T) + 
                                SMI_May6 + SMI_Jun6 + SMI_Aug6 + SMI_Oct6, data = Maize_meteo)

summary(fit_step_nonlinear_best_demean_anomaly) # Adjusted R-squared:  0.4357


##############################################################
#### Model selection for linear anomalies using Step AIC ####
############################################################

#### Stepwise Model Selection - both directions ####
step_linear <- stepAIC(fit_step_linear_demean_anomaly, direction="both")
'P_May, T_Oct, SMI_Sep, comId are dropped'

step_linear$anova

#### Best model ####
fit_step_linear_best_demeaned_anomaly <- lm(siloMaizeAnomaly ~ 
                                    P_Jun_demeaned + 
                                    P_Jul_demeaned + 
                                    P_Aug_demeaned + 
                                    P_Sep_demeaned + 
                                    P_Oct_demeaned + 
                                    T_May_demeaned + 
                                    T_Jun_demeaned + 
                                    T_Jul_demeaned + 
                                    T_Aug_demeaned + 
                                    T_Sep_demeaned + 
                                    SMI_May6 + SMI_Jun6 + 
                                    SMI_Jul6 + SMI_Aug6 + SMI_Oct6,
                                  data = Maize_meteo)

summary(fit_step_linear_best_demeaned_anomaly) # Adjusted R-squared:  0.4044 

################################################################
#### Model Selection via dredge: Automated model selection ####
##############################################################

## set global model
options(na.action = "na.fail") # necessary for dredge
fit_step_nonlinear_noComId <-  lm(siloMaizeAnomaly ~
                            poly(P_May_demeaned, degree=3, raw=T) +
                            poly(P_Jun_demeaned, degree=3, raw=T) +
                            poly(P_Jul_demeaned, degree=3, raw=T) +
                            poly(P_Aug_demeaned, degree=3, raw=T) +
                            poly(P_Sep_demeaned, degree=3, raw=T) +
                            poly(P_Oct_demeaned, degree=3, raw=T) +
                            poly(T_May_demeaned, degree=3, raw=T) +
                            poly(T_Jun_demeaned, degree=3, raw=T) +
                            poly(T_Jul_demeaned, degree=3, raw=T) +
                            poly(T_Aug_demeaned, degree=3, raw=T) +
                            poly(T_Sep_demeaned, degree=3, raw=T) +
                            poly(T_Oct_demeaned, degree=3, raw=T) + 
                            SMI_May6 + SMI_Jun6 + SMI_Jul6 +
                            SMI_Aug6 + SMI_Sep6 + SMI_Oct6, data=Maize_meteo)
summary(fit_step_nonlinear)

## Dredge 
fit_step_nonlinear_dredge_bic <- dredge(fit_step_nonlinear, rank = "BIC")


get.models(fit_step_nonlinear_dredge_bic, subset= 1)

bestModelDredgeBIC <- lm(formula = siloMaizeAnomaly ~ 
     poly(P_Jul_demeaned, degree = 3, raw = T) + 
     poly(P_Jun_demeaned, degree = 3, raw = T) +
     poly(P_May_demeaned, degree = 3, raw = T) +
     poly(P_Oct_demeaned, degree = 3, raw = T) +
     poly(T_Aug_demeaned, degree = 3, raw = T) +
     poly(T_Jul_demeaned, degree = 3, raw = T) + 
     poly(T_Jun_demeaned, degree = 3, raw = T) +
     poly(T_May_demeaned, degree = 3, raw = T) +
     poly(T_Oct_demeaned, degree = 3, raw = T) +
     poly(T_Sep_demeaned, degree = 3, raw = T) +
     SMI_May6 + SMI_Jun6 + 1
     , data = Maize_meteo)
summary(bestModelDredgeBIC)
plot(bestModelDredgeBIC , main="bestModelDredgeBIC")

' Es erscheint mir, dass hier die negativen Anomalien besser erklärt werden können. Die range der fitted negative values ist zumindest höher.

  Sehr interessant ist hier im jedem Fale, dass SMI August rausfällt, wenn für die Meterologie kontrolliert. Es könnte als bedeuten, dass es starke Abhängigkeiten über 
die Monate hinweg mit der Meterologie gibt. Insgesamt mach diese Modell Sinn, da durch den SMI im Mai und Juni die Bodenfeuchte sozusagen auf den Mittelwert gesetzt wird und
von da an die Veränderungen wichtig sind. Insgesamt kann dieses Modell aber auch nicht die extremem Abweichungen im siloMaize erklären. 
'

# importance(get.models(fit_step_nonlinear_dredge_bic, subset=))

###########################################################################
#### Retrieve Best Model which includes SMI_Jun, SMI_Aug, P_Jul, T_Jul ####
options(na.action = "na.fail")
fit_step_nonlinear_dredge_bic_fixed  <- dredge(fit_step_nonlinear_noComId, fixed=c("SMI_Jun6", "SMI_Aug6", "poly(P_Jul_demeaned, degree = 3, raw = T)", 
                                                   "poly(P_Jul_demeaned, degree = 3, raw = T)", "poly(T_Jul_demeaned, degree = 3, raw = T)", "1") , rank = "BIC", trace = 2)

get.models(fit_step_nonlinear_dredge_bic_fixed, subset= 1)

bestModelDredgeBIC_fixed <-lm(formula = siloMaizeAnomaly ~ 
                                poly(P_Jun_demeaned, degree = 3, raw = T) +
                                poly(P_May_demeaned, degree = 3, raw = T) +
                                poly(P_Oct_demeaned, degree = 3, raw = T) +
                                poly(T_Aug_demeaned, degree = 3, raw = T)  +
                                poly(T_Jun_demeaned, degree = 3, raw = T) +
                                poly(T_May_demeaned, degree = 3, raw = T) +
                                poly(T_Oct_demeaned, degree = 3, raw = T) +
                                poly(T_Sep_demeaned, degree = 3, raw = T) +
                                SMI_May6 +
                                1 +
                                                                poly(P_Jul_demeaned, degree = 3, raw = T) +
                                poly(T_Jul_demeaned, degree = 3, raw = T) +

                                SMI_Jun6 +
                                SMI_Aug6 + 
                                ,
                                data = Maize_meteo)
summary(bestModelDredgeBIC_fixed)
plot(bestModelDredgeBIC_fixed, main="bestModelDredgeBIC_fixed")

#### Hier sollte ich die Interaktion meiner Standard Terme nochmals untersuchen. 


'
Ausgehend vom Model Output habe ich nun verschiedene Sensitivitätsanalysen gemacht - insbesondere wie extreme feuchte SMI in Jun und extreme trockene SMI in August reagieren. 


Niederschlag im Jun scheint nicht so wichtig, genauso wie im Oct. 
SMI May scheint auch nicht so wichtig, da scheint Niederschlag wichtiger zu sein. 
Es sollte, auch aus theoretischer Sicht, Sinn machen, die Temperatur von May zu berücksichtigen, da es eine mindest Temperatur gibt für den Boden, damit Mais 
anfängt sich zu entwickeln. Generell sollt ich versuchen, mehr Theorie basiert zu arbeiten. Dies könnten aussehen wie folgt. Temp in May, SMI in Jun, um die Boden-
feuchte Effect am Anfang der Season zu kontrollieren, dann die Meteorologie in July, welche sehr stark ist und auch einen übergang zu SMI im August bildet, wodurch für die
Effekte in der zweiten Hälfte kontrolliert wird. 

Wenn ich die Nierderschlagseffeckte mit berücksichtige, dann ändert sich sehr wenig  - sowohl die wichtigen Koeffizienten noch die Erklärungskraft im sample wird stark 
beeinflusst -  daher lasse ich diese raus. 

Anders verhält sich mit den Temperatur Effekten. Wenn man alle Temperatur Effekte berücksichtigt dann verschwindet vor allem der starke SMI August Effekt. 

Effekte auf SMI_Aug6drght_svr:  - T_Oct_demeaned gering
                                - T_Sep_demeaned vernachlässigbar
                                - T_May_demeaned vernachlässigbar
                                - T_Oct_demeaned gering
                                - T_Aug_demeaned sehr stark

Lässt man T_Aug mit SMi_Aug6 interagieren, dann ändert das auch nicht an der Reduction des SMI_Aug6drght_svr Effektes. Auch sämtliche Interktion mit Meteorologie in July
stellt den Wert nicht wieder her. 

Es scheint so, als ob Temperature im August einen sehr starken Einfluss auf Yield hat, jedoch der Effekt stark mit SMI korreliert und somit desses Wirkung
reduziert. Leider hat Temperatur einen stark positiven Effekt auf R². Komischerweise sieht man diesen Effekt nicht im ersten Paper. Es muss also Interaktionszusammenhänge 
über die Monate hinweg geben. 

Grundsätzlich verbessert die Berücksichtigung von T_Aug die Erklärungskraft im Modell wenn man T_August berücksichtigt. 
Adjusted R-squared:  0.3608 -> Adjusted R-squared:  0.3914 

Für extreme Feuchte in July kann man derartige Beobachtungen nicht machen. Dort gibt es weniger Sensitivitäten. 

Interaktion SMI Jun und August: Verbessert das Model kaum und kompensiert auch nicht das weglassen der Bodenfeuchte. 


'

#########################################################################
#### Export all Regression Results for model selection ia stargazer ####
#######################################################################
stargazer(fit_step_linear_best_demean, fit_step_nonlinear_best_demean, 
          fit_step_linear_best_demean_anomaly, fit_step_nonlinear_best_demean_anomaly, 
          type="text",
          out= "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug_comId/RegressionResults/fit_best.txt", 
          column.labels = c("linear", "nonlinear"))


################################
#### All subset Regression ####
##############################

#### nonlinear anomalies ####
leaps_nonlinear <- regsubsets(siloMaize ~ comId + 
                                poly(P_May_demean, degree=3, raw=T) +
                                poly(P_Jun_demean, degree=3, raw=T) +
                                poly(P_Jul_demean, degree=3, raw=T) +
                                poly(P_Aug_demean, degree=3, raw=T) +
                                poly(P_Sep_demean, degree=3, raw=T) +
                                poly(P_Oct_demean, degree=3, raw=T) +
                                poly(T_May_demean, degree=3, raw=T) +
                                poly(T_Jun_demean, degree=3, raw=T) +
                                poly(T_Jul_demean, degree=3, raw=T) +
                                poly(T_Aug_demean, degree=3, raw=T) +
                                poly(T_Sep_demean, degree=3, raw=T) +
                                poly(T_Oct_demean, degree=3, raw=T) + 
                                SMI_May6 + SMI_Jun6 + SMI_Jul6 +
                                SMI_Aug6 + SMI_Sep6 + SMI_Oct6, data=Maize_meteo, nbest=1, really.big=T)

# view results 
summary(leaps_nonlinear)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps_nonlinear,scale="r2")
# plot statistic by subset size 

subsets(leaps_nonlinear, statistic="rsq")


####  linear anomalies ####
leaps_linear <- regsubsets(siloMaize ~ comId + 
                                P_May_demean +
                                P_Jun_demean +
                                P_Jul_demean +
                                P_Aug_demean +
                                P_Sep_demean +
                                P_Oct_demean +
                                T_May_demean +
                                T_Jun_demean +
                                T_Jul_demean +
                                T_Aug_demean +
                                T_Sep_demean +
                                T_Oct_demean +
                                SMI_May6 + SMI_Jun6 + SMI_Jul6 +
                                SMI_Aug6 + SMI_Sep6 + SMI_Oct6, data=Maize_meteo, nbest=1, really.big=T)

# view results 
summary(leaps_linear)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps_linear,scale="r2")
# plot statistic by subset size 

subsets(leaps_linear, statistic="rsq")

#######################################
#### Relative Importance Measures ####
#####################################
' Does not work with polynomials'
relaimp_linear <- calc.relimp(fit_step_linear_best_demean,type=c("lmg","last","first"),
            rela=TRUE)
str(relatimp_linear)






##############################
#### Models from paper 1 ####
############################

# #############################################
# #### Fit best model from paper 1 - yield ####
# lm.fit_SMI_6_Jul_comId <- lm(siloMaize ~ 
#                                + T_Jul 
#                              + I(T_Jul^2) 
#                              + I(T_Jul^3) 
#                              + P_Jul  
#                              + I(P_Jul^2) 
#                              + I(P_Jul^3) 
#                              + SMI_Jul6
#                              + comId
#                              , data = Maize_meteo)
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

# ############################################
# #### Fit combined model - yield Anomaly ####
# #### siloMaizeAnomaly - lm 
# summary(Maize_meteo$siloMaizeAnomaly)
# plot(Maize_meteo$siloMaizeAnomaly)
# lm.fit_SMI_6_Jun_Aug_comId_anomaly <- lm(siloMaizeAnomaly ~ 
#                                            + T_Jul
#                                            + P_Jul
#                                            + I(T_Jul^2)
#                                            + I(T_Jul^3)
#                                            + I(P_Jul^2)
#                                            + I(P_Jul^3)
#                                            + SMI_Jun6
#                                          + SMI_Aug6
#                                          + comId
#                                          ,
#                                          data = Maize_meteo)
# summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly )  # Adjusted R-squared:   0.3362 (silomaizeAnomaly)
# 
# # #### siloMaizeAnomaly - plm 
# plm.fit_SMI_6_Jun_Aug_comId_anomaly <- plm(siloMaizeAnomaly ~ 
#                                              # + T_Jul
#                                              # + I(T_Jul^2)
#                                              # + I(T_Jul^3)
#                                              # + P_Jul
#                                              # + I(P_Jul^2)
#                                              # + I(P_Jul^3)
#                                              + SMI_Jun6
#                                            # + SMI_Aug6
#                                            , data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly ) # Adjusted R-squared:  0.33616 (silomaizeAnomaly)
# 
# #### Check average of fixed effects ####
# mean(fixef(plm.fit_SMI_6_Jun_Aug_comId_anomaly))
# 
# ########################################################
# #### Fit best model from paper 1 - siloMaizeAnomaly ####
# lm.fit_SMI_6_Jul_comId_anomaly <- lm(siloMaizeAnomaly ~
#                                        + T_Jul
#                                      + I(T_Jul^2)
#                                      + I(T_Jul^3)
#                                      + P_Jul
#                                      + I(P_Jul^2)
#                                      + I(P_Jul^3)
#                                      + SMI_Jun6
#                                      + SMI_Aug6
#                                      + comId
#                                      , data = Maize_meteo)
# 
# summary(lm.fit_SMI_6_Jul_comId_anomaly)  # Adjusted R-squared:  0.2841  (silomaize - keep comIds with at least nine observations)
# # plot(lm.fit_SMI_6_Jul_comId_anomaly)
# 

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

## Conclusions ##0
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




