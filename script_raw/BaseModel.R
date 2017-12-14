#### Description of Script ####
' 
- Load data.frames needed (Maize_meteo, vgr2)
- Estimate models used for prediction, etc., in other scripts
- It appears to be the case that FE models deliver similiar results to those without fixed effects when applying to demeaned data with anomalies 
  in the dependent variables. Thus, I leave out the model.matrix procedures at the moment (as if 2.11.2017). 
- Define lists used in loops accordingly
'
#### Output ####
## Files
' 

'
## Plots
'

'

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
'
  - Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)
  - vg2500_krs <- ("./../Proj1/data/data_spatial/", "vg2500_krs")
  - 
'




###################
## Load Packages ##
source("./script/script_raw/Packages.R")

##############################################################################################################################################################################
#############################################################################################################################################################################
#############################################################################################################################################################################
# rm(list=ls())
getwd()

#### Read in Maize_meteo Data ####
Maize_meteo <- read_csv( file="./data/data_processed/Maize_meteo.csv")
Maize_meteo
# Maize_meteo$X1 <- NULL
# str(Maize_meteo)
# names(Maize_meteo)

#### Make factors necessary for statistical applications ####
Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")] <- 
  lapply(Maize_meteo[,c("comId","year","SMI_May6","SMI_Jun6","SMI_Jul6","SMI_Aug6","SMI_Sep6","SMI_Oct6")], factor )

levels(Maize_meteo$SMI_Aug6)

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

#################################################
#### Load shape of administrative districts ####
###############################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS
names(vg2500_krs)[2] <- "comId"


##########################################
#### Load com specific average yield ####
########################################

avgYield_comId <- read.csv( file="./data/data_processed/avgYield_comId.csv")
avgYield_comId$X <- NULL
avgYield_comId$comId <- as.factor(avgYield_comId$comId)
str(avgYield_comId)




#################################################################################################################################################################
###############################################
#### Estimate models used for predictions ####
#############################################
#################################################################################################################################################################



# ## Yield Anomaly, demeaned meteo, SMI Jun and Aug, comId ##
# lm.fit_SMI_6_Jun_Aug_anomaly_demean_comId <- lm(siloMaizeAnomaly ~ 
#                                                 + poly(P_Jul_demeaned, degree = 3, raw = T)  
#                                                 + poly(T_Jul_demeaned, degree = 3, raw = T) 
#                                                 + SMI_Jun6 
#                                                 + SMI_Aug6 
#                                                 + comId
#                                                 ,
#                                                 data = Maize_meteo)
# # summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_comId )
# par(mfrow=c(2,2))
# plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean_comId)

## Yield Anomaly, demeaned meteo, SMI Jun and Aug, NO comId ##
lm.fit_SMI_6_Jun_Aug_anomaly_demean <- lm(siloMaizeAnomaly ~
                                     + poly(P_Jul_demeaned, degree = 3, raw = TRUE)
                                   + poly(T_Jul_demeaned, degree = 3, raw = TRUE)
                                   + SMI_Jun6
                                   + SMI_Aug6
                                   ,
                                   data = Maize_meteo)

summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean )
par(mfrow=c(2,2))
# plot(lm.fit_SMI_6_Jun_Aug_anomaly_demean, which=c(1:4))


# ## Cross Validate to derive out of sample adjusted R² ##
# library(DAAG)
# cv <- cv.lm(data=Maize_meteo, lm.fit_SMI_6_Jun_Aug_anomaly_demean, m=3) # 3 fold cross-validation
# 
# ##
# theta.fit <- function(x,y){lsfit(x,y)}
# theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
# 
# # matrix of predictors
# X <- as.matrix(Maize_meteo[c("P_Jul_demeaned","P_Jul_demeaned","SMI_Jun6", "SMI_Aug6")])
# str(X )
# # vector of predicted values
# y <- as.matrix(Maize_meteo[c("siloMaizeAnomaly")])
# str(y )
# 
# results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
# cor(y, fit$fitted.values)**2 # raw R2
# cor(y,results$cv.fit)**2 # cross-validated R2 


## Yield Anomaly, demeaned meteo, SMI JuL  ##
lm.fit_SMI_6_Jul_anomaly_demean <- lm(siloMaizeAnomaly ~
                                         + poly(P_Jul_demeaned, degree = 3, raw = T)
                                       + poly(T_Jul_demeaned, degree = 3, raw = T)
                                        + SMI_Jul6

                                       ,
                                       data = Maize_meteo)
summary(lm.fit_SMI_6_Jul_anomaly_demean)
plot(lm.fit_SMI_6_Jul_anomaly_demean, main="Std. anomaly demean - noComId")

# ## Yield Anomaly, demeaned meteo, SMI JuL, comId  ##
# lm.fit_SMI_6_Jul_anomaly_demean_comId <- lm(siloMaizeAnomaly ~
#                                          + poly(P_Jul_demeaned, degree = 3, raw = T)
#                                        + poly(T_Jul_demeaned, degree = 3, raw = T)
#                                        + SMI_Jul6
#                                        + comId
#                                        ,
#                                        data = Maize_meteo)
# summary(lm.fit_SMI_6_Jul_anomaly_demean_comId)
# plot(lm.fit_SMI_6_Jul_anomaly_demean_comId, main="Std. anomaly demean - noComId")

## Best model derived via dredge model subset selection based on BIC ##
' This model is chosen by MuMln::dredge (in BaseMode_exploration). Since SMI_May and SMI_Jun are very much correlated, I prefer 
  to delete SMI_May (which is the month with the lower explanatory power in paper 1). Overall, this model clearly seems prone to overfitting.
  Here, way more variables are chosen compared to the model chosen by the EARTH package, which uses 
'
bestModelDredgeBIC_anomaly_demean <- lm(formula = siloMaizeAnomaly ~
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
                                          SMI_Jun6 + 1
                                        , data = Maize_meteo)
summary(bestModelDredgeBIC_anomaly_demean)
# plot(bestModelDredgeBIC_anomaly_demean, main="bestModelDredgeBIC")

# ## Best model derived via dredge model subset selection based on BIC, comId ##
# bestModelDredgeBIC_anomaly_demean_comId <- lm(formula = siloMaizeAnomaly ~ 
#                                           poly(P_Jul_demeaned, degree = 3, raw = T) + 
#                                           poly(P_Jun_demeaned, degree = 3, raw = T) +
#                                           poly(P_May_demeaned, degree = 3, raw = T) +
#                                           poly(P_Oct_demeaned, degree = 3, raw = T) +
#                                           poly(T_Aug_demeaned, degree = 3, raw = T) +
#                                           poly(T_Jul_demeaned, degree = 3, raw = T) + 
#                                           poly(T_Jun_demeaned, degree = 3, raw = T) +
#                                           poly(T_May_demeaned, degree = 3, raw = T) +
#                                           poly(T_Oct_demeaned, degree = 3, raw = T) +
#                                           poly(T_Sep_demeaned, degree = 3, raw = T) +
#                                           SMI_May6 + SMI_Jun6 + 1 + comId
#                                         , data = Maize_meteo)
# summary(bestModelDredgeBIC_anomaly_demean_comId)
# plot(bestModelDredgeBIC_anomaly_demean_comId, main="bestModelDredgeBIC")

# ## Best model derived via dredge model subset selection based on BIC - with fixed SMI June and August and July Meteorology ##
# bestModelDredgeBIC_anomaly_demean_fixed <-lm( formula = siloMaizeAnomaly ~ 
#                                 poly(P_Jun_demeaned, degree = 3, raw = T) +
#                                 poly(P_May_demeaned, degree = 3, raw = T) +
#                                 poly(P_Oct_demeaned, degree = 3, raw = T) +
#                                 poly(T_Aug_demeaned, degree = 3, raw = T) +
#                                 poly(T_Jun_demeaned, degree = 3, raw = T) +
#                                 poly(T_May_demeaned, degree = 3, raw = T) +
#                                 poly(T_Oct_demeaned, degree = 3, raw = T) +
#                                 poly(T_Sep_demeaned, degree = 3, raw = T) +
#                                 SMI_May6 +
#                                 1 +
#                                 
#                                 poly(P_Jul_demeaned, degree = 3, raw = T) +
#                                 poly(T_Jul_demeaned, degree = 3, raw = T) +
#                                 
#                                 SMI_Jun6 +
#                                 SMI_Aug6  
#                                 ,
#                               data = Maize_meteo)
# 
# summary(bestModelDredgeBIC_anomaly_demean_fixed )
# plot(bestModelDredgeBIC_fixed, main="bestModelDredgeBIC_fixed")

## Base Model with Jun and August SMI employing splines instead of polynomials ##
model_mgcv_BASE <-  gam(siloMaizeAnomaly ~
                          ti(SMI_Jun) +
                          ti(T_Jul_demeaned)  +
                          ti(P_Jul_demeaned ) +
                          ti(SMI_Aug),
                        data = Maize_meteo)


summary(model_mgcv_BASE)

## Take a look at check plot and plotmo ##
# par(mfrow=c(2,2))
# gam.check(model_mgcv_BASE)
# plotmo(model_mgcv_BASE , level = 0.95, ylim =c(-200, 150))


## best mgcv model without interaction ##
model_mgcv_bestEARTH_noInteraction_T <-  gam(siloMaizeAnomaly ~ 
                                               ti(SMI_Jun)  +  
                                               ti(P_Jun_demeaned) + 
                                               ti(P_Jul_demeaned) + 
                                               ti(P_Aug_demeaned) + 
                                               ti(T_May_demeaned) + 
                                               ti(T_Jun_demeaned) + 
                                               ti(T_Jul_demeaned) + 
                                               ti(T_Aug_demeaned) +
                                               ti(T_Sep_demeaned) + 
                                               ti(T_Oct_demeaned),
                                             data= Maize_meteo)   

summary(model_mgcv_bestEARTH_noInteraction_T)

## Take a look at check plot and plotmo ##
# par(mfrow=c(2,2))
# gam.check(model_mgcv_bestEARTH_noInteraction_T )
# plotmo(model_mgcv_bestEARTH_noInteraction_T  , level = 0.95, ylim =c(-200, 150))


#################################################################################################################
#### Preparations to make predictions employing fixed effects for the data derived from five climate models ####
###############################################################################################################
' Since I am not using the same data on which the model is trained I need to implement some changes. 
In particular I cannot work with factor(comId) but need to derive the model matrix to be able to use predict() for those models with fixed effects.'


##############################################
#### Select variables used for estimation ####
Maize_meteo_short <- Maize_meteo %>% select(year, comId, T_Jul_demeaned, P_Jul_demeaned,  SMI_Jun6:SMI_Aug6,siloMaizeAnomaly)


# #########################################
# #### Append model.matrix explicitly ####
# #######################################
'Here I include the model.matrix for the comID exclusively since the  predict command had issues to deal with those.
Also, only the data used in the models are considered further.#
Based on my analysis I decided not to work with fixed effects any more. Instead, I employ demeneaning of the predictors and use anomalies 
for crop yield. First, the spatial fixed effect are very small when demeaning the data and thus are neglectable (also based on tests)'
# 
# #### Create model.matrix ####
# modelmatrix <- model.matrix(~ Maize_meteo$comId)
# dim(modelmatrix)
# str(modelmatrix)
# 
# #### Convert model.matrix to data.frame ####
# modelmatrix_Df <-as.data.frame((modelmatrix))
# str(modelmatrix_Df)
# modelmatrix_Df$`Maize_meteo$comId1002` # There is a one for each year in the data when comId == 1002 is true
# 
# #### Delete modelmatrix ####
# rm(modelmatrix)
# 
# #### Cbind modelmatrix with short data.frame ####
# Maize_meteo_modelmatrix <- cbind(Maize_meteo_short, modelmatrix_Df)
# Maize_meteo_modelmatrix$`(Intercept)` <- NULL
# 
# #### Clean up names  ####
# x <- make.names(names(Maize_meteo_modelmatrix))
# x
# colnames <- gsub("Maize_meteo.", "", x)
# colnames(Maize_meteo_modelmatrix) <- colnames
# str(Maize_meteo_modelmatrix)
# 
# #############################################################
# #### Fit model using model matrix - used for prediction ####
# ###########################################################
# 
# #### Delete columns year and comId ####
# names(Maize_meteo_modelmatrix)
# Maize_meteo_modelmatrix$year <- NULL
# Maize_meteo_modelmatrix$comId <- NULL
# 
# #####################################################
# #### lm.fit with explicit model.matrix of comIds ####
# 
# #### June to August ####
# 
# ## Yield - Anomaly 
# drops <- c("SMI_Jul6", "siloMaize")
# lm.fit_SMI_6_Jun_Aug_anomaly_demean_modelmatrix <- 
#   lm(siloMaizeAnomaly ~ I(T_Jul_demeaned^2) + I(T_Jul_demeaned^3)  +I(P_Jul_demeaned^2) +  I(P_Jul_demeaned^3) + .   ,
#      data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
# summary(lm.fit_SMI_6_Jun_Aug_anomaly_demean_modelmatrix) # Adjusted R-squared:    0.3362 
# 
# #### July ####
# 
# ## Yield - Anomaly
# drops <- c("SMI_Jun6", "SMI_Aug6", "siloMaize")
# lm.fit_SMI_6_Jul_anomaly_demean_modelmatrix <- 
#   lm(siloMaizeAnomaly ~ I(T_Jul_demeaned^2) + I(T_Jul_demeaned^3)  +I(P_Jul_demeaned^2) +  I(P_Jul_demeaned^3) + .   ,
#      data = Maize_meteo_modelmatrix[ , !(names(Maize_meteo_modelmatrix) %in% drops)])
# summary(lm.fit_SMI_6_Jul_anomaly_demean_modelmatrix) # Adjusted R-squared:    0.2841 
# 

########################################################################################################################################################################
########################
#### Prepare loops ####
######################
########################################################################################################################################################################
'Here, I am also adding models based on cross-validation (BaseModel_caret.R). On the other hand, I am leaving out all models including comIds.
 4.11.2017: We decided to work with the models derived from paper 1, i.e. the base models and to leave the cross-validation models for later. '

## models used on training data ##
modelList <- list(lm.fit_SMI_6_Jun_Aug_anomaly_demean,
                  lm.fit_SMI_6_Jul_anomaly_demean,
                  # bestModelDredgeBIC_anomaly_demean, 
                  # model_earth_noInteraction_T,
                  # model_earth_preselect_T, 
                  model_mgcv_bestEARTH_noInteraction_T,
                  # model_mgcv_bestEARTH_interaction_ti_T_k3,
                  model_mgcv_BASE
                  )


modelListNames <- c("lm.fit_SMI_6_Jun_Aug",
                    "lm.fit_SMI_6_Jul",
                    # "bestModelDredgeBIC",
                    # "earth_noInteraction_T",
                    # "earth_preselect_T",
                    "mgcv_bestEARTH_noInteraction_T",
                    # "mgcv_bestEARTH_interaction_ti_T_k3", 
                    "mgcv_SMI_6_Jun_Aug")
# quote(modelList)
## models used on climate data ##
modelList_climate <- list(lm.fit_SMI_6_Jun_Aug_anomaly_demean,
                          lm.fit_SMI_6_Jul_anomaly_demean,
                          # bestModelDredgeBIC_anomaly_demean, 
                          # model_earth_noInteraction_T,
                          # model_earth_preselect_T, 
                          model_mgcv_bestEARTH_noInteraction_T,
                          # model_mgcv_bestEARTH_interaction_ti_T_k3, 
                          model_mgcv_BASE)

nameList_climate <- c("lm.fit_SMI_6_Jun_Aug_anomaly_demean",
                      "lm.fit_SMI_6_Jul_anomaly_demean",
                      # "bestModelDredgeBIC_anomaly_demean",
                      # "earth_noInteraction_T_anomaly_demean",
                      # "earth_preselect_T_anomaly_demean",
                      "mgcv_bestEARTH_noInteraction_T_anomaly_demean",
                      # "mgcv_bestEARTH_interaction_ti_T_k3_anomaly_demean", 
                      "mgcv_SMI_6_Jun_Aug_anomaly_demean")


nameList_climate_store <- list(
  lm.fit_SMI_6_Jun_Aug_anomaly_demean      =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()), 
  lm.fit_SMI_6_Jul_anomaly_demean          =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()), 
  # bestModelDredgeBIC_anomaly_demean        =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()), 
  # model_earth_noInteraction_T              =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()), 
  model_mgcv_bestEARTH_noInteraction_T     =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()),
  # model_mgcv_bestEARTH_interaction_ti_T_k3 =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list()),
  model_mgcv_BASE                          =  list(DMI=list(),ICTP=list(),KNMI=list(),MPI=list(),SMHIRCA=list())
                            )

nameList_climate_average_store <- list(
  lm.fit_SMI_6_Jun_Aug_anomaly_demean      =  list(), 
  lm.fit_SMI_6_Jul_anomaly_demean          =  list(), 
  # bestModelDredgeBIC_anomaly_demean        =  list(), 
  # model_earth_noInteraction_T              =  list(), 
  model_mgcv_bestEARTH_noInteraction_T     =  list(),
  # model_mgcv_bestEARTH_interaction_ti_T_k3 =  list(),
  model_mgcv_BASE                          =  list()
)



## list of RCMs ##
namelist_RCMs <- c("DMI","ICTP", "KNMI","MPI","SMHI")



