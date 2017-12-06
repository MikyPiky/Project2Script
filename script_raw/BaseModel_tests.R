#### Description ####
'
- Test for individual effects: plm::plmtest and plm::pFtest
- Assess Influence (Leverage*discrepancy)
- Heteroskedasdicity
- Serial Autocorrelation
- Cross Section Correlation
- Spatial Correlation -> neues neighbouring package von Roger Bivand
'

#### Input ####
'  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)

'

#### Output ####
''

#### Results ####
'
  - Test for individual effects: no individual effects found (makes sense since all data are either indiviudally demeaned or indeces)
  - Assess Inlfuence: When calculating cutoff by 4/(n-k) and leaving out those observations, the adj.R2 rises. 
    However, no systematic measurement errors can be detected. 
    It seems, nevertheless, that in particular the postitive extreme of silageMaize Anomalies are better explained (when looking at nomal q-q plot).
    I guess it is best to stick with current data.
  - Heteroskedasdicity is not detected by the means of the Breusch - Pagan Test
  - Serial Autocorrelation is not detected by various test

'
 

###################
## Load Packages ##
# source("./script/script_raw/Packages.R")

#############################################################################################################################################################################
#############################################################################################################################################################################

##### Load BaseModel.R ####
rm(list=ls())
getwd()
source("./script/script_raw/BaseModel.R")

##############################
#### Estimate PLM Models ####
############################

####################
#### Base Model ####

## with individual effects ##
plm.fit_SMI_6_Jun_Aug_anomaly_demean <- plm(siloMaizeAnomaly ~
                                              + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                              + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                              + SMI_Jun6
                                            + SMI_Aug6
                                            ,
                                            data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_anomaly_demean )

## pooled model ##
plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling <- plm(siloMaizeAnomaly ~
                                                      + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                      + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                      + SMI_Jun6
                                                    + SMI_Aug6
                                                    ,
                                                    data = Maize_meteo, model=("pooling"), index = c("comId","year"))

summary(plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling)

###########################################################################
#### Best model derived via dredge model subset selection based on BIC ####

## with individual effects ##
plm.bestModelDredgeBIC_anomaly_demean <- plm(formula = siloMaizeAnomaly ~ 
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
                                             ,
                                             data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.bestModelDredgeBIC_anomaly_demean )

## pooled ##
plm.bestModelDredgeBIC_anomaly_demean_pooling <- plm(formula = siloMaizeAnomaly ~ 
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
                                                     ,
                                                     data = Maize_meteo, model=("pooling"), index = c("comId","year"))
summary(plm.bestModelDredgeBIC_anomaly_demean_pooling )

##########################################################################################################################################
##########################################################################################################################################
#### Tests ####
##########################################################################################################################################
##########################################################################################################################################s

##############################################
#### Test for individual (Fixed) Effects ####
############################################

####################
#### Base Model ####

#### Lagrange Multiplier Test - Honda test for unbalanced data #### 
plmtest(plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling, effect = "individual" ) 
' NULL:  no significant individual Effects
The probability of rejecting the Null given the NULL is true is 1 for indiviudal effects -> no individual effects'

plmtest(plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling, effect = "time" ) 
' NULL:  no significant time Effects
The probability of rejecting the Null given the NULL is true is very low for time effects -> time effects effects'


#### Ftest for Individual Effectks ####
pFtest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling)
' NULL:  no significant individual Effects
The probability of rejecting the Null given the NULL is true is 1 for indiviudal effects -> no individual effects'

###########################################################################
#### Best model derived via dredge model subset selection based on BIC ####

#### Lagrange Multiplier Test - Honda test for unbalanced data #### 
plmtest(plm.bestModelDredgeBIC_anomaly_demean_pooling, effect = "individual" ) 
' NULL:  no significant individual Effects
The probability of rejecting the Null given the NULL is true is 1 for indiviudal effects -> no individual effects'

plmtest(plm.bestModelDredgeBIC_anomaly_demean_pooling, effect = "time" ) 
' NULL:  no significant time Effects
The probability of rejecting the Null given the NULL is true is very low for time effects -> time effects effects'


#### Ftest for Individual Effectks ####
pFtest(plm.bestModelDredgeBIC_anomaly_demean, plm.bestModelDredgeBIC_anomaly_demean_pooling)
' NULL:  no significant individual Effects
The probability of rejecting the Null given the NULL is true is 1 for indiviudal effects -> no individual effects'

######################################
#### Test for Heteroskedasdicity ####
####################################

####################
#### Base Model ####
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean)

## Breusch-Pagan Test ##
bptest(lm.fit_SMI_6_Jun_Aug_anomaly_demean) # Breusch Pagan Test of Heteroskedastie in den Störgrößen:
'Null: Homoskedasdicity.' 
'The probability of rejecting the Null given the NULL is true is very low -> Heteroskedasdicity.'

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. ##
bptest(lm.fit_SMI_6_Jun_Aug_anomaly_demean, studentize = TRUE)
'The probability of rejecting the Null given the NULL is true is very low. Thus, we can reject the NULL. -> Heteroskedasdicity'

###########################################
#### Best subset based on BIC (dredge) ####

## Breusch-Pagan Test ##
bptest(bestModelDredgeBIC_anomaly_demean) # Breusch Pagan Test of Heteroskedastie in den Störgrößen:
'Null: Homoskedasdicity.' 
'The probability of rejecting the Null given the NULL is true is very low. Heteroskedasdicity'

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. ##
bptest(bestModelDredgeBIC_anomaly_demean, studentize = TRUE)
'The probability of rejecting the Null given the NULL is true is very low. Thus, we reject the NULL. --> homoscedasdicity.'


###########################################
#### Tests for serial autocorrelation ####
#########################################

####################
#### Base Model ####
pbgtest(plm.fit_SMI_6_Jun_Aug_anomaly_demean_pooling) 
'
NULL: no serial autocorrelation
The probability of rejecting the Null given the NULL is true is very low. --> Serial autocorrelation.
'
## Durbin Watson test for serial autocorrelation 
dwtest(lm.fit_SMI_6_Jun_Aug_anomaly_demean)
'
NULL: no serial autocorrelation
The probability of rejecting the Null given the NULL is true is very low. --> Serial autocorrelation.
'
## Breush Pagan test
bgtest(lm.fit_SMI_6_Jun_Aug_anomaly_demean)
'
NULL: no serial autocorrelation
The probability of such a test statistic given the NULL is very low. --> Serial autocorrelation.
'

###########################################
#### Best subset based on BIC (dredge) ####
pbgtest(plm.bestModelDredgeBIC_anomaly_demean_pooling) 
'
NULL: no serial autocorrelation
The probability of rejecting the Null given the NULL is true is very low.--> Serial autocorrelation.
'
## Durbin Watson test for serial autocorrelation 
dwtest(bestModelDredgeBIC_anomaly_demean)
'
NULL: no serial autocorrelation
The probability of rejecting the Null given the NULL is true is very low. --> Serial autocorrelation.
'
## Breush Pagan test
bgtest(bestModelDredgeBIC_anomaly_demean)
'
NULL: no serial autocorrelation
The probability of such a test statistic given the NULL is very low. --> Serial autocorrelation.
'



#################################################################################################################################################################################
#####################################################
#### Assessing Influence (Leverage*discrepancy) ####
###################################################
#################################################################################################################################################################################

#########################################
#### Estimate model in lm framework ####
#######################################
lm.fit_SMI_6_Jun_Aug_anomaly_demean <- lm(siloMaizeAnomaly ~
                                             + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                             + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                             + SMI_Jun6
                                           + SMI_Aug6
                                           ,
                                           data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean)
par(mfrow=c(2,2))
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean)

#######################################
#### Take a look at the residuals ####
#####################################

## Histogram of the Residuals ##
hist(lm.fit_SMI_6_Jun_Aug_anomaly_demean$residuals, n=2000) 

# Make a data.frame of the residuals
lm.fit_SMI_6_Jun_Aug_anomaly_demean_Resid <- as_tibble(as.numeric(lm.fit_SMI_6_Jun_Aug_anomaly_demean$residuals))
names(lm.fit_SMI_6_Jun_Aug_anomaly_demean_Resid) <- "Residual"

## Make a dataframe with referential variables
dim(Maize_meteo)

Maize_meteo_ref <- Maize_meteo %>% select(year, comId, com)
lm.fit_SMI_6_Jun_Aug_anomaly_demean_Resid <- bind_cols(Maize_meteo_ref, lm.fit_SMI_6_Jun_Aug_anomaly_demean_Resid)

##W Write data.frame of the Residuals
write_csv(lm.fit_SMI_6_Jun_Aug_anomaly_demean_Resid, path = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/Residuals.csv")

#########################
#### Cooks Distance ####
#######################

## Plot Cooks Distance 
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean, which=4)
Maize_meteo[c(1453, 1458, 1452),] %>% select(year:siloMaizeAnomaly, SMI_Jul6, SMI_Aug6, T_Jul_demeaned, P_Jul_demeaned)
  
## Read out Cooks Distance 
cook <- as.data.frame(cooks.distance(lm.fit_SMI_6_Jun_Aug_anomaly_demean))
colnames(cook) <- "CooksDistance"

## Reference Cooks Distance Values
cook_ref <- bind_cols(Maize_meteo_ref, cook)

## Merge cook_ref with Maize_Meteo ##
Maize_meteo_cooks <- inner_join(Maize_meteo, cook_ref, by=c("comId", "com", "year"))
Maize_meteo_cooks

## Order Cooks Distance ##
cook_ref <- cook_ref[order(cook_ref$CooksDistance, decreasing = T), ]

## Order Maize_meteo_cooks ##
Maize_meteo_cooks <- Maize_meteo_cooks[order(Maize_meteo_cooks$CooksDistance, decreasing = T), ]

## Deselect predictor variables from Maize_meteo_cooks ##
Maize_cooks <- Maize_meteo_cooks %>% select(-c(P_May : SMI_Oct6))


## Write out Cooks Distance
write.csv(Maize_cooks, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/Maize_cooks.csv")

## Define Cutoffs ##
cutoff_small <- 4/((nrow(Maize_meteo) - length(lm.fit_SMI_6_Jun_Aug_anomaly_demean$coefficients) - 1)) 
cutoff_small
cutoff_large <- 1

## Filter for observation with cooksDistance larger than small cutoff ##
Maize_cooks_cutoffSmall <- Maize_cooks %>% filter ( CooksDistance > cutoff_small) %>% mutate_if(is.double, funs(round(.,3)))

## Write out Cooks Distance
write.csv(Maize_cooks_cutoffSmall  , file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/Maize_cooks_cutoffSmall.csv")


## Number of Cooks Distance Values below the cutoffs ##
nrow_cooks_small <- nrow(Maize_meteo[cook > cutoff_small,]) # 302
nrow_cooks_large <- nrow(Maize_meteo[cook > cutoff_large,]) 


## Make a table of numbers of values above small_cutoff conditional on years 
year_cooks_small <- as.data.frame(table(Maize_meteo$year[cook > cutoff_small ]) )
names(year_cooks_small) <- c("year", "Freq")
year_cooks_small
plot(year_cooks_small)

## Make a table of numbers of valued above small_cutoff conditional on comId
comId_cooks_small <- as.data.frame(table(Maize_meteo$comId[cook > cutoff_small ]) )
names(comId_cooks_small) <- c("comId", "Freq")
comId_cooks_small
plot(comId_cooks_small)

## Make ordered table of cutoff above frequancies per comId
comId_cooks_small2 <-cbind(comId_cooks_small, unique(Maize_meteo$com))
comId_cooks_small2[order(comId_cooks_small2$Freq),]
comId_cooks_small_ordered <- comId_cooks_small2[order(comId_cooks_small2$Freq),]

## Write Tables
write.csv(year_cooks_small, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_year.csv")
write.csv(comId_cooks_small2, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_comId.csv")
write.csv(comId_cooks_small_ordered, "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_comId_ordered.csv")

################################################################################
#### Explore the comIds which have cumulations (min 5)  above small cutoff ####
##############################################################################
Maize_meteo_cook <- Maize_meteo %>% filter(comId == 5970 | comId == 5913 | comId == 5374 | comId == 12053 | comId == 6437 | comId == 5711 | comId == 5116 )

Maize_meteo_cook <- Maize_meteo_cook[order(Maize_meteo_cook$comId, Maize_meteo_cook$year),]

# View(Maize_meteo_cook)
hist(Maize_meteo$siloMaizeAnomaly, n=200)

#############################################################################################################################
##### Make data,frame excluding comIds which have more than 5 observations with influence above 5 - use it for estimation ####
Maize_meteo_cookCutoff_5 <- Maize_meteo %>% filter(comId != 5970 | comId != 5913 | comId != 5374 | comId != 12053 | comId != 6437 | comId != 5711 | comId != 5116 )


lm.fit_SMI_6_Jun_Aug_anomaly_demean_cookCutoff_5 <- lm(siloMaizeAnomaly ~
                                           + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                           + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                           + SMI_Jun6
                                         + SMI_Aug6
                                         ,
                                         data = Maize_meteo_cookCutoff_5)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cookCutoff_5)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cookCutoff_5 )
' Dieser Ansatz verändert doe Koeffizienten kaum und trägt auch nicht dazu bei, dass mehr Varianz erklärt wird. '

################################################################################################################
##### Make data,frame excluding comIds which are above small cutof in cooks distance- use it for estimation ####

Maize_meteo_cooksSmall <- anti_join( Maize_meteo, cooks_small,by=c("comId", "year"))

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooksSmall <- lm(siloMaizeAnomaly ~
                                                        + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                        + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                        + SMI_Jun6
                                                      + SMI_Aug6
                                                      ,
                                                      data = Maize_meteo_cooksSmall)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooksSmall)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooksSmall )

'
Die Koeffizienten sind nicht so sehr beeinflusst. Das adj. R-square steigt jedoch sehr stark. 
Schaut man sich den Nomal Q-Q plot an, dann scheinen durch das Löschen vor allem die extrem postitiven Werte besser erklärt zu sein. Hier stellt sich nun die Frage, 
inwiefern dies auf mögliche Messfehler zurückzuführen ist.
Schaut man sich die Daten in Maize_cooks_cooksCutoffSmall an, dann scheint es nicht wirklich systematische Messfehler zu geben. Tendenziell haben Kreisfreie Städte eine höhere
Varianz als landschaftlich geprägt Gegenden. Hier könnte es nochmals Sinn machen, über Gewichtung nachzudenken.
Insgesamt wird hier'

################################################################################################################
##### Make data,frame excluding comIds which are above cutoff 0.005 in cooks distance- use it for estimation ####

Maize_meteo_cooks005 <- Maize_meteo_cooks %>% filter(CooksDistance < 0.005)

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks005 <- lm(siloMaizeAnomaly ~
                                                      + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                      + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                      + SMI_Jun6
                                                    + SMI_Aug6
                                                    ,
                                                    data = Maize_meteo_cooks005)
 summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks005)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks005 )

#################################################################################################################
##### Make data,frame excluding comIds which are above cutoff 0.004 in cooks distance- use it for estimation ####

Maize_meteo_cooks004 <- Maize_meteo_cooks %>% filter(CooksDistance < 0.004)

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks004 <- lm(siloMaizeAnomaly ~
                                                    + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                    + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                    + SMI_Jun6
                                                  + SMI_Aug6
                                                  ,
                                                  data = Maize_meteo_cooks004)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks004)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks004 )

#################################################################################################################
##### Make data,frame excluding comIds which are above cutoff 0.003 in cooks distance- use it for estimation ####
Maize_meteo_cooks003 <- Maize_meteo_cooks %>% filter(CooksDistance < 0.003)

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks003 <- lm(siloMaizeAnomaly ~
                                                    + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                    + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                    + SMI_Jun6
                                                  + SMI_Aug6
                                                  ,
                                                  data = Maize_meteo_cooks003)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks003)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks003 )

#################################################################################################################
##### Make data,frame excluding comIds which are above cutoff 0.002 in cooks distance- use it for estimation ####
Maize_meteo_cooks002 <- Maize_meteo_cooks %>% filter(CooksDistance < 0.002)

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks002 <- lm(siloMaizeAnomaly ~
                                                    + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                    + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                    + SMI_Jun6
                                                  + SMI_Aug6
                                                  ,
                                                  data = Maize_meteo_cooks002)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks002)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks002 )


#################################################################################################################
##### Make data,frame excluding comIds which are above cutoff 0.002 in cooks distance- use it for estimation ####
Maize_meteo_cooks001 <- Maize_meteo_cooks %>% filter(CooksDistance < 0.001)

lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks001 <- lm(siloMaizeAnomaly ~
                                                    + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                                    + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                                    + SMI_Jun6
                                                  + SMI_Aug6
                                                  ,
                                                  data = Maize_meteo_cooks001)
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks001)
plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_cooks001)

Maize_meteo_cooks001_anti <- Maize_meteo_cooks %>% filter(CooksDistance >= 0.001)
' Die Erklärungskraft steigt mir dem absenken der cutoffs für die CooksDistance'


################################################################################################################################################################################ 
################################################################################################################################################################################  



#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean,vcov=vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0")) 
cov0_SM_BEST <- vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST <- sqrt(diag(cov0_SM_BEST))

cov0.1_SM_BEST <- vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST <- sqrt(diag(cov0.1_SM_BEST))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, vcov = function(x) vcovBK(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SMI_6_Jun_Aug_anomaly_demean)
coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean,  vcov=function(x) vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0")) 
cov2_SM_BEST     <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0")
DK.se_SM_BEST    <- sqrt(diag(cov2_SM_BEST))
# 
# cov2.1_SM_BEST   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST <- sqrt(diag(cov2.1_SM_BEST))

# cov2.2_SM_BEST   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST <- sqrt(diag(cov2.2_SM_BEST))
# 
# cov2.3_SM_BEST   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST <- sqrt(diag(cov2.3_SM_BEST))
# 
# cov2.4_SM_BEST   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST <- sqrt(diag(cov2.4_SM_BEST))
# 
cov2.5_SM_BEST   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST <- sqrt(diag(cov2.5_SM_BEST))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, vcovDC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0"))
cov3_SM_BEST <- vcovDC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0")
CT.se_SM_BEST <- sqrt(diag(cov3_SM_BEST))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST, Wh.se_serial_SM_BEST,  DK.se_SM_BEST, DK2.5.se_SM_BEST, CT.se_SM_BEST)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SMI_6_Jun_Aug_anomaly_demean, plm.fit_SMI_6_Jun_Aug_anomaly_demean, plm.fit_SMI_6_Jun_Aug_anomaly_demean, plm.fit_SMI_6_Jun_Aug_anomaly_demean, plm.fit_SMI_6_Jun_Aug_anomaly_demean,plm.fit_SMI_6_Jun_Aug_anomaly_demean,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - July",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Jul/SM_best.txt"
)

