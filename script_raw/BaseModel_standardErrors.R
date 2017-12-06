#### Description ####
'
- Correct Standard Errors
  - Driscoll Kraay
  - Bootstrapping
- Produce Coefficient Table -> corrected and non corrected
'

#### Input ####
'  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)

'

#### Output ####
''

#### Results ####
'
- Test for individual effects: no individual effects found (makes sense since all data are either indiviudally demeaned or indeces)
- Assess Inlfuence: When calculating cutoff by 4/(n-k) and leaving out those observations, the adj.R2 rises. However, no systematic measurement errors can be detected. 
It seems, nevertheless, that in particular the postitive extreme of silageMaize Anomalies are better explained (when looking at nomal q-q plot).
I guess it is best to stick with current data.
- Heteroskedasdicity is detected by the means of the Breusch - Pagan Test
- Serial Autocorrelation is detected by various test

'

#############################################################################################################################################################################
#############################################################################################################################################################################
# rm(list=ls())
# getwd()

source("./script/script_raw/BaseModel.R")

#####################
#### Models used ####
summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean)

## plm Version
plm.fit_SMI_6_Jun_Aug_anomaly_demean <- plm(siloMaizeAnomaly ~
                                            + poly(P_Jul_demeaned, degree = 3, raw = TRUE)
                                          + poly(T_Jul_demeaned, degree = 3, raw = TRUE)
                                          + SMI_Jun6
                                          + SMI_Aug6
                                          ,
                                          data = Maize_meteo, effect="individual", model="pooling")
summary(plm.fit_SMI_6_Jun_Aug_anomaly_demean )

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, vcov=vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0"))

cov0.fit_SMI_6_Jun_Aug_anomaly_demean  <- vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0", cluster="group")
Wh.se_serial.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov0.fit_SMI_6_Jun_Aug_anomaly_demean ))

cov0.1.fit_SMI_6_Jun_Aug_anomaly_demean  <- vcovHC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov0.1.fit_SMI_6_Jun_Aug_anomaly_demean ))


## Beck Katz:
# # coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, vcov = function(x) vcovBK(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
summary(plm.fit_SMI_6_Jun_Aug_anomaly_demean)
coeftest(pplm.fit_SMI_6_Jun_Aug_anomaly_demean,  vcov=function(x) vcovSCC(pplm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0")) 

cov2.fit_SMI_6_Jun_Aug_anomaly_demean      <- vcovSCC(pplm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano",type = "HC0")
DK.se.fit_SMI_6_Jun_Aug_anomaly_demean     <- sqrt(diag(cov2.fit_SMI_6_Jun_Aug_anomaly_demean ))

# 
# cov2.1.fit_SMI_6_Jun_Aug_anomaly_demean    <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov2.1.fit_SMI_6_Jun_Aug_anomaly_demean ))

# cov2.2.fit_SMI_6_Jun_Aug_anomaly_demean    <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov2.2.fit_SMI_6_Jun_Aug_anomaly_demean ))
# 
# cov2.3.fit_SMI_6_Jun_Aug_anomaly_demean    <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov2.3.fit_SMI_6_Jun_Aug_anomaly_demean ))
# 
# cov2.4.fit_SMI_6_Jun_Aug_anomaly_demean    <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov2.4.fit_SMI_6_Jun_Aug_anomaly_demean ))
# 
cov2.5.fit_SMI_6_Jun_Aug_anomaly_demean    <- vcovSCC(plm.fit_SMI_6_Jun_Aug_anomaly_demean,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov2.5.fit_SMI_6_Jun_Aug_anomaly_demean ))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SMI_6_Jun_Aug_anomaly_demean, vcovDC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0"))
cov3.fit_SMI_6_Jun_Aug_anomaly_demean  <- vcovDC(plm.fit_SMI_6_Jun_Aug_anomaly_demean, method = "arellano", type = "HC0")
CT.se.fit_SMI_6_Jun_Aug_anomaly_demean  <- sqrt(diag(cov3.fit_SMI_6_Jun_Aug_anomaly_demean ))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross.fit_SMI_6_Jun_Aug_anomaly_demean , Wh.se_serial.fit_SMI_6_Jun_Aug_anomaly_demean ,  
           DK.se.fit_SMI_6_Jun_Aug_anomaly_demean , DK2.5.se.fit_SMI_6_Jun_Aug_anomaly_demean , CT.se.fit_SMI_6_Jun_Aug_anomaly_demean )


labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")

stargazer(plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
          plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
          plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
          plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
          plm.fit_SMI_6_Jun_Aug_anomaly_demean,
          plm.fit_SMI_6_Jun_Aug_anomaly_demean,
          se = se,   
          dep.var.caption  = "Base Model",
          dep.var.labels = "silomaize",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1
          , type="text", out="./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/RegressionResults/fit_SMI_6_Jun_Aug.txt"
          )

stargazer(plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
          plm.fit_SMI_6_Jun_Aug_anomaly_demean, 
         
          se = list(NULL, DK.se.fit_SMI_6_Jun_Aug_anomaly_demean),   
          dep.var.caption  = "Base Model",
          dep.var.labels = "silomaize",
          style="default",
          model.numbers = FALSE,
          column.labels = c("Standard", "Driscol - Kraay")
          , type="latex"
          # , out="./figures/figures_exploratory/Train/lm.fit_SMI_6_Jun_Aug/RegressionResults/fit_SMI_6_Jun_Aug_short_TEX.txt"
          )



