#### Description ####
'
- Test for individual effects: plm::plmtest and plm::pFtest
- Assess Influence (Leverage*discrepancy)
- Heteroskedasdicity
- Serial Autocorrelation
- Correct Standard Errors
  - Driscoll Kraay
  - Bootstrapping
'

#### Input ####
'  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)

'

#### Output ####
''

#### Results ####
'
 - Test for individual effects: no individual effects found (makes sense since all data are either indiviudally demeaned or indeces)

'
 

###################
## Load Packages ##
library(MASS)
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
library(car)
library(dplyr)
library(leaps)
library(relaimpo)
library(MuMIn)
library(forcats)

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
Maize_meteo$SMI_May6 <- relevel(Maize_meteo$SMI_May6, ref= "nrml") 
Maize_meteo$SMI_Jun6 <- relevel(Maize_meteo$SMI_Jun6, ref= "nrml") 
Maize_meteo$SMI_Jul6 <- relevel(Maize_meteo$SMI_Jul6, ref= "nrml") 
Maize_meteo$SMI_Aug6 <- relevel(Maize_meteo$SMI_Aug6, ref= "nrml") 
Maize_meteo$SMI_Sep6 <- relevel(Maize_meteo$SMI_Sep6, ref= "nrml") 
Maize_meteo$SMI_Oct6 <- relevel(Maize_meteo$SMI_Oct6, ref= "nrml") 

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

######################################
#### Test for individual effects ####
####################################

## with individual effects ##
plm.fit_SMI_6_Jun_Aug_comId_anomaly <- plm(siloMaizeAnomaly ~
                                           + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                           + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                           + SMI_Jun6
                                           + SMI_Aug6
                                           ,
                                           data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly )

## pooled model ##
plm.fit_SMI_6_Jun_Aug_comId_anomaly_pooled <- plm(siloMaizeAnomaly ~
                                             + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                             + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                             + SMI_Jun6
                                           + SMI_Aug6
                                           ,
                                           data = Maize_meteo, model=("pooling"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly_pooled)

##################################
#### Lagrange Multiplier Test ####
plmtest(plm.fit_SMI_6_Jun_Aug_comId_anomaly_pooled, effect = "individual" ) 
plmtest(plm.fit_SMI_6_Jun_Aug_comId_anomaly_pooled, effect = "time" ) 
# Honda test for unbalanced data
# H0: no significant individual Effects
'Here, we cannot reject the H0 with very high significance'

#######################################
#### Ftest for Individual Effectks ####
pFtest(plm.fit_SMI_6_Jun_Aug_comId_anomaly, plm.fit_SMI_6_Jun_Aug_comId_anomaly_pooled)
'Here, we cannot reject the H0 with very high significance'

#####################################################
#### Assessing Influence (Leverage*discrepancy) ####
###################################################

########################################
#### Estimate model in lm framework ####
lm.fit_SMI_6_Jun_Aug_comId_anomaly <- lm(siloMaizeAnomaly ~
                                             + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                             + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                             + SMI_Jun6
                                           + SMI_Aug6
                                           ,
                                           data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly)


######################################
#### Take a look at the residuals ####

## Histogram of the Residuals ##
hist(lm.fit_SMI_6_Jun_Aug_comId_anomaly$residuals, n=2000) 

# Make a data.frame of the residuals
lm.fit_SMI_6_Jun_Aug_comId_anomaly_Resid <- as_tibble(as.numeric(lm.fit_SMI_6_Jun_Aug_comId_anomaly$residuals))
names(lm.fit_SMI_6_Jun_Aug_comId_anomaly_Resid) <- "Residual"

## Make a dataframe with referential variables
dim(Maize_meteo)

Maize_meteo_ref <- Maize_meteo %>% select(year, comId, com)
lm.fit_SMI_6_Jun_Aug_comId_anomaly_Resid <- bind_cols(Maize_meteo_ref, lm.fit_SMI_6_Jun_Aug_comId_anomaly_Resid)

##W Write data.frame of the Residuals
write_csv(lm.fit_SMI_6_Jun_Aug_comId_anomaly_Resid, path = "./figures/figures_exploratory/Train/lm .fit_SMI_6_Jun_Aug/Residuals.csv")

####################
## Cooks Distance ##

## Plot Cooks Distance
plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly, which=4)
Maize_meteo[c(1453, 1458, 1452),]

## Read out Cooks Distance ##
cook_Jul <- as.data.frame(cooks.distance(lm.fit_SMI_6_Jun_Aug_comId_anomaly))
colnames(cook_Jul) <- "CooksDistance"

## Reference Cooks Distance Values
cook_Jul_ref <- bind_cols(Maize_meteo_ref, cook_Jul)

## Order Cooks Distance ##
cook_Jul_ref <- cook_Jul_ref[order(cook_Jul_ref$CooksDistance, decreasing = T), ]

## Write out Cooks Distance
write.csv(cook_Jul_ref, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/CookDistance.csv")

## Define Cutoffs ##
cutoff_SM_Jul_small <- 4/((nrow(Maize_meteo) - length(lm.fit_SMI_6_Jun_Aug_comId_anomaly$coefficients) - 1)) 
cutoff_SM_Jul_small
cutoff_SM_Jul_large <- 1

## Number of Cooks Distance Values below the cutoffs ##
nrow_cooks_small <- nrow(Maize_meteo[cook_Jul > cutoff_SM_Jul_small,]) 
nrow_cooks_large <- nrow(Maize_meteo[cook_Jul > cutoff_SM_Jul_large,]) 

# ## Make a output
# capture.output(c("Cutoff_small",cutoff_SM_Jul_small), file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooks.txt")
# capture.output(c("Cutoff_small_number",nrow_cooks_small), file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooks.txt", append=T)
# capture.output(c("Cutoff_large",cutoff_SM_Jul_large), file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooks.txt", append=T)
# capture.output(c("Cutoff_large_number",nrow_cooks_large), file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooks.txt", append=T)

## Make a table of numbers of values above small_cutoff conditional on years 
year_cooks_SM_Jul_small <- as.data.frame(table(Maize_meteo$year[cook_Jul > cutoff_SM_Jul_small ]) )
names(year_cooks_SM_Jul_small) <- c("year", "Freq")
year_cooks_SM_Jul_small
plot(year_cooks_SM_Jul_small)

## Make a table of numbers of valued above small_cutoff conditional on comId
comId_cooks_SM_Jul_small <- as.data.frame(table(Maize_meteo$comId[cook_Jul > cutoff_SM_Jul_small ]) )
names(comId_cooks_SM_Jul_small) <- c("comId", "Freq")
comId_cooks_SM_Jul_small
plot(comId_cooks_SM_Jul_small)

## Make ordered table of cutoff above frequancies per comId
comId_cooks_SM_Jul_small2 <-cbind(comId_cooks_SM_Jul_small, unique(Maize_meteo$com))
comId_cooks_SM_Jul_small2[order(comId_cooks_SM_Jul_small2$Freq),]
comId_cooks_SM_Jul_small_ordered <- comId_cooks_SM_Jul_small2[order(comId_cooks_SM_Jul_small2$Freq),]

## Write Tables
# capture.output(year_cooks_SM_Jul_small, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/CooksDistance.txt", append=T)
# capture.output(comId_cooks_SM_Jul_small, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/CooksDistance.txt", append=T)
# capture.output(c("Ordered table of comIds", comId_cooks_SM_Jul_small_ordered), file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/CooksDistance.txt", append=T)

write.csv(year_cooks_SM_Jul_small, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_year.csv")
write.csv(comId_cooks_SM_Jul_small2, file = "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_comId.csv")
write.csv(comId_cooks_SM_Jul_small_ordered, "./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/cooks_comId_ordered.csv")

###############################################################################
#### Explore the comIds which have cumulations (min 5)  above small cutoff ####
Maize_meteo_cook <- Maize_meteo %>% filter(comId == 5970 | comId == 5913 | comId == 5374 | comId == 12053 | comId == 6437 | comId == 5711 | comId == 5116 )
# | comId == 16070 )

Maize_meteo_cook <- Maize_meteo_cook[order(Maize_meteo_cook$comId, Maize_meteo_cook$year),]

View(Maize_meteo_cook)
hist(Maize_meteo$siloMaizeAnomaly, n=200)

##### Make data,frame excluding comIds which have more than 5 observations with influence above 5 ####
Maize_meteo_cutoff_5 <- Maize_meteo %>% filter(comId != 5970 | comId != 5913 | comId != 5374 | comId != 12053 | comId != 6437 | comId != 5711 | comId != 5116 )


lm.fit_SMI_6_Jun_Aug_comId_anomaly_cutoff_5 <- lm(siloMaizeAnomaly ~
                                           + poly(P_Jul_demeaned, degree = 3, raw = T) +
                                           + poly(T_Jul_demeaned, degree = 3, raw = T) +
                                           + SMI_Jun6
                                         + SMI_Aug6
                                         ,
                                         data = Maize_meteo_cutoff_5)
summary(lm.fit_SMI_6_Jun_Aug_comId_anomaly_cutoff_5)
plot(lm.fit_SMI_6_Jun_Aug_comId_anomaly_cutoff_5 )
' Dieser Ansatz verändert doe Koeffizienten kaum und trägt auch nicht dazu bei, dass mehr Varianz erklärt wird. '

################################################################################################################################################################## 


########################
## Heteroskedasdicity ##
bptest(glm.fit_SMI_6_Jun_Aug_comId_anomaly) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SMI_6_Jun_Aug_comId_anomaly)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SMI_6_Jun_Aug_comId_anomaly, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'


######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SMI_6_Jun_Aug_comId_anomaly)
pbgtest(plm.fit_SMI_6_Jun_Aug_comId_anomaly) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SMI_6_Jun_Aug_comId_anomaly)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SMI_6_Jun_Aug_comId_anomaly,vcov=vcovHC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Jul <- vcovHC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Jul <- sqrt(diag(cov0_SM_BEST_Jul))

cov0.1_SM_BEST_Jul <- vcovHC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Jul <- sqrt(diag(cov0.1_SM_BEST_Jul))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SMI_6_Jun_Aug_comId_anomaly, vcov = function(x) vcovBK(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SMI_6_Jun_Aug_comId_anomaly)
coeftest(plm.fit_SMI_6_Jun_Aug_comId_anomaly,  vcov=function(x) vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Jul     <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0")
DK.se_SM_BEST_Jul    <- sqrt(diag(cov2_SM_BEST_Jul))
# 
# cov2.1_SM_BEST_Jul   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Jul <- sqrt(diag(cov2.1_SM_BEST_Jul))

# cov2.2_SM_BEST_Jul   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Jul <- sqrt(diag(cov2.2_SM_BEST_Jul))
# 
# cov2.3_SM_BEST_Jul   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Jul <- sqrt(diag(cov2.3_SM_BEST_Jul))
# 
# cov2.4_SM_BEST_Jul   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Jul <- sqrt(diag(cov2.4_SM_BEST_Jul))
# 
cov2.5_SM_BEST_Jul   <- vcovSCC(plm.fit_SMI_6_Jun_Aug_comId_anomaly,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Jul <- sqrt(diag(cov2.5_SM_BEST_Jul))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SMI_6_Jun_Aug_comId_anomaly, vcovDC(plm.fit_SMI_6_Jun_Aug_comId_anomaly, method = "arellano", type = "HC0"))
cov3_SM_BEST_Jul <- vcovDC(plm.fit_SMI_6_Jun_Aug_comId_anomaly, method = "arellano", type = "HC0")
CT.se_SM_BEST_Jul <- sqrt(diag(cov3_SM_BEST_Jul))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Jul, Wh.se_serial_SM_BEST_Jul,  DK.se_SM_BEST_Jul, DK2.5.se_SM_BEST_Jul, CT.se_SM_BEST_Jul)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SMI_6_Jun_Aug_comId_anomaly, plm.fit_SMI_6_Jun_Aug_comId_anomaly, plm.fit_SMI_6_Jun_Aug_comId_anomaly, plm.fit_SMI_6_Jun_Aug_comId_anomaly, plm.fit_SMI_6_Jun_Aug_comId_anomaly,plm.fit_SMI_6_Jun_Aug_comId_anomaly,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - July",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Jul/SM_Jul_best.txt"
)

