#### Description ####
'
Use caret package to undergo crossvalidation to estimate a MARS model. 
'

#### Input ####
'
- BaseModel.R

'

#### Output ####
''

#### Results ####
'

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
# source("./script/script_raw/Packages.R")

#####################################
#### Use of parallel processing ####
###################################
registerDoMC(cores = 4)




##########################################################
#### Load pure linear data to merge with Maize_meteo  ####
YieldMeteo <- read_csv("./data/data_processed/YieldMeteo.csv")
YieldMeteo$year <- as.factor(YieldMeteo$year)  
YieldMeteo$comId <- as.factor(YieldMeteo$comId)  
YieldMeteo

Maize_meteo
Maize_meteo_select <- Maize_meteo %>% select(year, comId, siloMaizeAnomaly,SMI_May6: SMI_Oct6)

YieldMaizeMeteo <- inner_join(YieldMeteo, Maize_meteo_select, by=c("year","comId"))
names(YieldMaizeMeteo )


#########################################################################################################
#### Preselect Variables use for automatic recursive feature selection and in model building by MARS ####
'Do not use SMI for May, July, and August because of correlation. 
Similar, no use of T and PET at the same time.'

## Predictors: pure linear ##
YieldMaizeMeteo_select <- YieldMaizeMeteo %>% select(matches("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct"))


x <- YieldMaizeMeteo_select %>% select(SMI_Jan, SMI_Mar, SMI_May, SMI_Jul,
                                       P_Jan:P_Jul,
                                       T_Jan:T_Jul
                                       )
  # select(SMI_Jun,  SMI_Aug, SMI_Oct, P_May_demeaned:T_Oct_demeaned)
x
View(x)

x_PET <- YieldMaizeMeteo_select %>% select(SMI_Jun,  SMI_Aug, SMI_Oct, P_May_demeaned:P_Oct_demeaned, PET_May_demeaned:PET_Oct_demeaned)
x_PET

# ## Explained Variables ##
y <- YieldMaizeMeteo %>% select(winterWheat)
  # select(siloMaizeAnomaly)



#######################
#### Combine data  ####
data <- bind_cols(y, x)

data_PET <- bind_cols(y, x_PET)

data <- na.omit(data)


##########################################
#### Load Climate data to predict on ####
########################################

#### Need a loop starting here ####
Climate <-  read_csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_RCMs[[1]],"_19712000_demean.csv", sep=""))



##################################################################################################################################################################
###########################
#### Data Split in R #####
#########################
##################################################################################################################################################################


# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(Maize_meteo$siloMaizeAnomaly, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
# train a naive bayes model
model <- lm.fit_SMI_6_Jun_Aug_anomaly_demean
summary(model)
model$call

# # make predictions
# x_test <- data_test[,1:4]
# y_test <- data_test[,5]
# predictions <- predict(model, x_test)
# # summarize results
# confusionMatrix(predictions$class, y_test)

##################################################################################################################################################################
####################
#### Bootstrap ####
##################
##################################################################################################################################################################

# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model1 <- train(siloMaizeAnomaly ~ 1 + 
                  poly(P_Jul_demeaned,3) + 
                  I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) +
                  SMI_Jun6 + SMI_Aug6, 
                data=Maize_meteo, 
                trControl=train_control, 
                method="lm")

model <- train(model$call, trControl=train_control, 
               method="lm")
model1


model2 <- train(siloMaizeAnomaly ~ 1 + 
                  I(P_Jul_demeaned) +I(P_Jul_demeaned^2) +I(P_Jul_demeaned^3) + 
                  I(P_Jun_demeaned) +I(P_Jun_demeaned^2) +I(P_Jun_demeaned^3) + 
                  I(P_May_demeaned) +I(P_May_demeaned^2) +I(P_May_demeaned^3) + 
                  I(P_Oct_demeaned) +I(P_Oct_demeaned^2) +I(P_Oct_demeaned^3) + 
                  I(T_Aug_demeaned) +I(T_Aug_demeaned^2) +I(T_Aug_demeaned^3) + 
                  I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) + 
                  I(T_Jun_demeaned) +I(T_Jun_demeaned^2) +I(T_Jun_demeaned^3) + 
                  I(T_May_demeaned) +I(T_May_demeaned^2) +I(T_May_demeaned^3) + 
                  I(T_Oct_demeaned) +I(T_Oct_demeaned^2) +I(T_Oct_demeaned^3) + 
                  I(T_Sep_demeaned) +I(T_Sep_demeaned^2) +I(T_Sep_demeaned^3) + 
                  SMI_May6 + SMI_Jun6,
                data=Maize_meteo, 
                trControl=train_control, 
                method="lm")
model2
model2_I

model2_poly <- train(siloMaizeAnomaly ~ 1 + 
                       poly(P_Jul_demeaned,3) +
                       I(P_Jun_demeaned) +I(P_Jun_demeaned^2) +I(P_Jun_demeaned^3) + 
                       I(P_May_demeaned) +I(P_May_demeaned^2) +I(P_May_demeaned^3) + 
                       I(P_Oct_demeaned) +I(P_Oct_demeaned^2) +I(P_Oct_demeaned^3) + 
                       I(T_Aug_demeaned) +I(T_Aug_demeaned^2) +I(T_Aug_demeaned^3) + 
                       I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) + 
                       I(T_Jun_demeaned) +I(T_Jun_demeaned^2) +I(T_Jun_demeaned^3) + 
                       I(T_May_demeaned) +I(T_May_demeaned^2) +I(T_May_demeaned^3) + 
                       I(T_Oct_demeaned) +I(T_Oct_demeaned^2) +I(T_Oct_demeaned^3) + 
                       I(T_Sep_demeaned) +I(T_Sep_demeaned^2) +I(T_Sep_demeaned^3) + 
                       SMI_May6 + SMI_Jun6,
                     data=Maize_meteo, 
                     trControl=train_control, 
                     method="lm")
model2_poly


modelLM <- lm(siloMaizeAnomaly ~ 1 + 
                poly(P_Jul_demeaned,3, raw=T) +
                I(P_Jun_demeaned) +I(P_Jun_demeaned^2) + I(P_Jun_demeaned^3) + 
                I(P_May_demeaned) +I(P_May_demeaned^2) +I(P_May_demeaned^3) + 
                I(P_Oct_demeaned) +I(P_Oct_demeaned^2) +I(P_Oct_demeaned^3) + 
                I(T_Aug_demeaned) +I(T_Aug_demeaned^2) +I(T_Aug_demeaned^3) + 
                I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) + 
                I(T_Jun_demeaned) +I(T_Jun_demeaned^2) +I(T_Jun_demeaned^3) + 
                I(T_May_demeaned) +I(T_May_demeaned^2) +I(T_May_demeaned^3) + 
                I(T_Oct_demeaned) +I(T_Oct_demeaned^2) +I(T_Oct_demeaned^3) + 
                I(T_Sep_demeaned) +I(T_Sep_demeaned^2) +I(T_Sep_demeaned^3) + 
                SMI_May6 + SMI_Jun6,
              data=Maize_meteo)
summary(modelLM)
# summarize results           

print(bestModelDredgeBIC_anomaly_demean)

##################################################################################################################################################################
######################################
#### k - fold Cross - Validation ####
##################################### 
##################################################################################################################################################################
# define training control
train_control <- trainControl(method="cv", number=10)

model1 <- train(siloMaizeAnomaly ~ ., 
                data=data, 
                trControl=train_control, 
                method="earth")
model1


###############################################################################################################################################  
###############################################  
#### Repeated k - fold Cross - Validation ####
#############################################
###############################################################################################################################################

#### Define Train Control, i.e. the fold and repetitions of cross - validation ####
subset <- c(1:15)
train_control <- trainControl(method="repeatedcv", number=10, repeats=5)

#################################
#### Mars Algorithm - EARTH ####
###############################

###############################################################################
#### Define tuning grid with INTERACTION -> very similiar to CART approach ####
gbmGrid <-  expand.grid(nprune=c(1:20), degree=c(2))
'Extensive use of interactions does not improve the model fit. Instead, the best results can still  be found with
 a product degree of 2. '


## Model Mars considering all the data ##
model_earth <-  train(winterWheat ~ ., 
                 data=data, 
                 trControl=train_control, 
                 method="earth" , 
                 preProcess=c("center", "scale"),
                 # sizes = subsets,
                 # verbose=TRUE
                 tuneGrid = gbmGrid)

model_earth
model_earth$results
model_earth$finalModel
summary(model_earth)
predictors(model_earth) 
varImp(model_earth)


plot(model_earth)
plot(model_earth, metric="nprune")
plot(model_earth, metric="degree")
plot.earth.models(model_earth$finalModel)
evimp(model_earth$finalModel)

model_earth_climate <- predict(model_earth, Climate)




## Consider only data from theoretical base model ##
model_earth_base <-  train(siloMaizeAnomaly ~ SMI_Jun  +T_Jul_demeaned + P_Jul_demeaned+ SMI_Aug, 
                                         data=data, 
                                         trControl=train_control, 
                                         method="earth" ,
                                         tuneGrid = gbmGrid)

model_earth_base
model_earth_base$results
model_earth_base$finalModel
summary(model_earth_base)
predictors(model_earth_base )
plot(model_earth__base )
traplot(model_earth_base , metric="nprune")
plot(model_earth_base , metric="degree")

## only considering SMI ##
model_earth_smi <-  train(siloMaizeAnomaly ~ SMI_Jun  + SMI_Aug + SMI_Oct, 
                                        data=data, 
                                        trControl=train_control, 
                                        method="earth" ,
                                        tuneGrid = gbmGrid)

model_earth_smi$results
model_earth_smi$finalModel
summary(model_earth_smi )
predictors(model_earth_smi )
plot(model_earth_smi )

#################################################################################################################
#### Define tuning grid withOUT INTERACTION -> I guess simililar to GAMs approach and thus Linear Regression ####
gbmGrid_noInteraction <-  expand.grid(nprune=c(1:20), degree=c(1))

## Consider all data ##
model_earth_noInteraction <-  train(siloMaizeAnomaly ~ ., 
                 data=data, 
                 trControl=train_control, 
                 method="earth" ,
                 # , 
                 # sizes = subsets,
                 # verbose=TRUE
                 tuneGrid = gbmGrid_noInteraction
                 )

model_earth_noInteraction
model_earth_noInteraction$results
model_earth_noInteraction$finalModel
summary(model_earth_noInteraction)
predictors(model_earth_noInteraction) 
'There are only seven out of 15 predictors selected'
plot(model_earth_noInteraction)
varImp(model_earth_noInteraction )

plot.earth.models(list(model_earth_noInteraction$finalModel, model_earth$finalModel))
# plot(model_earth_noInteraction, metric="nprune")
# plot(model_earth_noInteraction, metric="degree")

## Consider only SMI data ##
model_earth_noInteraction_smi <-  train(siloMaizeAnomaly ~ SMI_Jun  + SMI_Aug + SMI_Oct, 
                 data=data, 
                 trControl=train_control, 
                 method="earth" ,
                 tuneGrid = gbmGrid_noInteraction)

model_earth_noInteraction_smi$results
model_earth_noInteraction_smi$finalModel
summary(model_earth_noInteraction_smi )
predictors(model_earth_noInteraction_smi )
plot(model_earth_noInteraction_smi )
traplot(model_earth_noInteraction_smi , metric="nprune")
plot(model_earth_noInteraction_smi , metric="degree")

## Consider only data from theoretical base model ##
model_earth_noInteraction_base <-  train(siloMaizeAnomaly ~ SMI_Jun  +T_Jul_demeaned + P_Jul_demeaned+ SMI_Aug, 
                                        data=data, 
                                        trControl=train_control, 
                                        method="earth" ,
                                        tuneGrid = gbmGrid_noInteraction)

model_earth_noInteraction_base
model_earth_noInteraction_base$results
model_earth_noInteraction_base$finalModel
summary(model_earth_noInteraction_base)
predictors(model_earth_noInteraction_base )
plot(model_earth_noInteraction_base )
traplot(model_earth_noInteraction_base , metric="nprune")
plot(model_earth_noInteraction_base , metric="degree")


#################################################################################################################
#### Define tuning grid withOUT INTERACTION -> I guess simililar to GAMs approach and thus Linear Regression ####
'Reduce nprune to 10'
gbmGrid_noInteraction_10 <-  expand.grid(nprune=c(1:10), degree=c(1))

## Consider all data ##
model_earth_noInteraction_10 <-  train(siloMaizeAnomaly ~ ., 
                                    data=data, 
                                    trControl=train_control, 
                                    method="earth" ,
                                    # , 
                                    # sizes = subsets,
                                    # verbose=TRUE
                                    tuneGrid = gbmGrid_noInteraction_10)

model_earth_noInteraction_10 <- model_earth_noInteraction
model_earth_noInteraction_10
model_earth_noInteraction_10$results
model_earth_noInteraction_10$finalModel
summary(model_earth_noInteraction_10)
predictors(model_earth_noInteraction_10)
plot(model_earth_noInteraction_10)
varImp(model_earth_noInteraction_10 )

plot.earth.models(list(model_earth_noInteraction$finalModel, model_earth$finalModel,model_earth_noInteraction_10))
# plot(model_earth_noInteraction, metric="nprune")

##################################################################
#### Compare the resampling results of all earth models used ####
resamps <- resamples(list(model_earth_noInteraction_base = model_earth_noInteraction_base, 
                          model_earth_noInteraction_smi  =model_earth_noInteraction_smi ))
summary(resamps)
xyplot(resamps, what = "BlandAltman")
diffs <- diff(resamps)
summary(diffs)




##################################################
#### Generalized Additive Model using SPLINE ####
################################################
gbmGrid_gam <-  expand.grid(select = FALSE, method="GCV.Cp" )

#### GAM without feature selection ####
model_gam <-  train(siloMaizeAnomaly ~ ., 
                      data=data, 
                      trControl=train_control, 
                      # tuneGrid = gbmGrid_gam,
                      method="gam" )

model_gam
model_gam$results
model_gam$finalModel
par(mfrow=c(1,1))
summary(model_gam) ## Shows a plot of the relative Importance of each variables
predictors(model_gam)
plot(model_gam)
summary.gam(model_gam$finalModel)
varImp(model_gam)
par(mfrow=c(3,5))
plot.gam(model_gam$finalModel)
plot(model_gam$finalModel)


ggplot(model_gam) + theme(legend.position = "top")

#### GAM with predictors selected by EARTH ####
model_gam_earth <-  train(siloMaizeAnomaly ~  P_Jul_demeaned + T_Aug_demeaned + SMI_Jun + 
                      T_May_demeaned + T_Jun_demeaned + P_Jun_demeaned + T_Sep_demeaned, 
                    data=data, 
                    trControl=train_control, 
                    tuneGrid = gbmGrid_gam,
                    method="gam" )

model_gam_earth
model_gam_earth$results
model_gam_earth$finalModel
par(mfrow=c(1,1))
summary(model_gam_earth) ## Shows a plot of the relative Importance of each variables
predictors(model_gam_earth)
# plot(model_gam_earth)
summary.gam(model_gam_earth$finalModel)
varImp(model_gam_earth)
par(mfrow=c(2,4))
plot.gam(model_gam_earth$finalModel)

plotGAM(model_gam_earth$finalModel, c("P_Jul_demeaned", "T_Aug_demeaned") )

ggplot(model_gam) + theme(legend.position = "top")

#### GAM with only SMI ####
model_gam_smi <-  train(siloMaizeAnomaly ~  SMI_Jun + SMI_Aug + SMI_Oct, 
                          data=data, 
                          trControl=train_control, 
                          tuneGrid = gbmGrid_gam,
                          method="gam" )

model_gam_smi
model_gam_smi$results
model_gam_smi$finalModel
par(mfrow=c(1,1))
summary(model_gam_smi) ## Shows a plot of the relative Importance of each variables
predictors(model_gam_smi)
# plot(model_gam_earth)
summary.gam(model_gam__smi$finalModel)
varImp(model_gam_smi)
par(mfrow=c(3,1))
plot.gam(model_gam_smi$finalModel)

plotGAM(model_gam_earth$finalModel, c("P_Jul_demeaned", "T_Aug_demeaned") )

ggplot(model_gam) + theme(legend.position = "top")

#### GAM - BASE Model ####
model_gam_base <-  train(siloMaizeAnomaly ~  SMI_Jun + SMI_Aug + T_Jul_demeaned + P_Jul_demeaned, 
                        data=data, 
                        trControl=train_control, 
                        tuneGrid = gbmGrid_gam,
                        method="gam" )

model_gam_base
model_gam_base$results
model_gam_base$finalModel
par(mfrow=c(1,1))
summary(model_gam_base) ## Shows a plot of the relative Importance of each variables
predictors(model_gam_base)
# plot(model_gam_earth)
summary.gam(model_gam_base$finalModel)
varImp(model_gam_base)
par(mfrow=c(2,2))
plot.gam(model_gam_base$finalModel)

plotGAM(model_gam_earth$finalModel, c("P_Jul_demeaned", "T_Aug_demeaned") )

ggplot(model_gam) + theme(legend.position = "top")



#### GAM with feature selection ####
gbmGrid_gam_select <-  expand.grid(select = TRUE, method="GCV.Cp" )

model_gam_select <-  train(siloMaizeAnomaly ~ ., 
                    data=data, 
                    trControl=train_control, 
                    tuneGrid =  gbmGrid_gam_select ,
                    method="gam" )

model_gam_select
model_gam_select$results
model_gam_select$finalModel
summary(model_gam_select) ## Shows a plot of the relative Importance of each variables
predictors(model_gam_select)
varImp(model_gam_select)
par(mfrow=c(3,5))
plot.gam(model_gam_select$finalModel)

### Gam based on model selection of EARTH/MARS ####



#### Make predictions on climate data ####
model_gam_climate <- predict(model_gam, Climate)


model_gam_climate <- predict(model_gam, Climate)

'
Aus "An Introduction to statistical learning with Applications in R":
Regression splines often give superior results to polynomial regression. This is because unlike polynomials, 
which must use a high degree (exponent in the highest monomial term, e.g. X^15) to produce flexible fits, 
splines introduce flexibility by increasing the number of knots but keeping the degree fixed. 
Generally, this approach produces more stable estimates. Splines also allow us to place more knots, 
and hence flexibility, over regions where the function f seems to be changing rapidly, and fewer knots where f appears more stable.
Figure 7.7 compares a natural cubic spline with 15 degrees of freedom to a degree-15 polynomial on the Wage data set. 
The extra flexibility in the polynomial produces undesirable results at the boundaries, 
while the natural cubic spline still provides a reasonable fit to the data.'


###########################
#### LM - Base Model  ####
#########################

subset <- c(1:15)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# gbmGrid <-  expand.grid(shrinkage = c(0.01), interaction.depth  = seq(1,7, by=2), n.minobsinnode =10,  n.trees = seq(100,1000, by=50))

model_lmBase <-  train(siloMaizeAnomaly ~ 1 + 
                poly(P_Jul_demeaned,3, raw=TRUE) + 
                I(T_Jul_demeaned) + I(T_Jul_demeaned^2) + I(T_Jul_demeaned^3) +
                    SMI_Jun6 + SMI_Aug6, 
                  data=Maize_meteo, 
                  trControl=train_control, 
                  method="lm")
         
model_lmBase$results
model_lmBase$finalModel
par(mfrow=c(1,1))
summary(model_lmBase) ## Shows a plot of the relative Importance of each variables
predictors(model_lmBase)
# plot(model_lmBase)


# ggplot(model_lmBase) + theme(legend.position = "top")


model_lmBase_climate <- predict(model_lmBase, Climate)


########################
#### CART - Model  ####
######################
' In think this makes more sense in classification context, at least for interpretation. '
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.001)) 

model_rpart <-  train(winterWheat ~ ., 
                       data=data, 
                       trControl=train_control, 
                       method= "rpart", tuneGrid = cpGrid )
model_rpart
model_rpart$results
model_rpart$bestTune
model_rpart$resample
model_rpart$



summary(model_rpart)
predictors(model_rpart)

best.tree = model_rpart$finalModel
summary(best.tree)

par(mfrow=c(1,1))
prp(best.tree)

# Create a new CART model
LossTreeCV = rpart(loss ~. - siloMaize, data = Train, method="class", cp = 0.1)

# Make predictions
model_rpart_climate <- predict(model_rpart, Climate)

' It looks like CART is performing less good compared to other models, in particular EARTH/MARS'


# ####################################################
# #### Use of adaBoost algrothim - Boosted Trees ####
# ##################################################
# ' Like overall, I cannot see the estimated parameters here. The prediction may be improved, the interpretatbility is not 
# possible. '
# subset <- c(1:15)
# train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# gbmGrid <-  expand.grid(shrinkage = c(0.01), interaction.depth  = seq(1,7, by=2), n.minobsinnode =10,  n.trees = seq(100,1000, by=50))
# 
# model_gbm <-  train(siloMaizeAnomaly ~ ., 
#                     data=data, 
#                     trControl=train_control, 
#                     method="gbm" ,
#                     # sizes = subsets,
#                     # verbose=TRUE
#                     tuneGrid = gbmGrid)
# 
# model_gbm
# model_gbm$results
# model_gbm$finalModel
# par(mfrow=c(1,1))
# summary(model_gbm) ## Shows a plot of the relative Importance of each variables
# predictors(model_gbm)
# plot(model_gbm)
# plot(model_gbm, metric=c("RMSE","shrinkage"))
# plot(model_gbm, metric=c("RMSE","interaction.depth"))
# plot(model_gbm, metric="n.minobsinnode")
# 
# ggplot(model_gbm) + theme(legend.position = "top")
# 
# Climate <-  read_csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_RCMs[[1]],"_19712000_demean.csv", sep=""))
# 
# model_gbm_climate <- predict(model_gbm, Climate)
# 
# 
# 
# 
# #############################################
# #### Boosted Generalized Additive Model ####
# ###########################################
# 
# subset <- c(1:15)
# train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # gbmGrid <-  expand.grid(shrinkage = c(0.01), interaction.depth  = seq(1,7, by=2), n.minobsinnode =10,  n.trees = seq(100,1000, by=50))
# 
# model_gamboost <-  train(siloMaizeAnomaly ~ ., 
#                          data=data, 
#                          trControl=train_control, 
#                          # preProc = c("center", "scale"),
#                          method="gamboost" )
# 
# model_gamboost
# model_gamboost$results
# model_gamboost$finalModel
# par(mfrow=c(1,1))
# summary(model_gamboost) ## Shows a plot of the relative Importance of each variables
# predictors(model_gamboost)
# plot(model_gamboost)
# # plot(model_svm, metric=c("RMSE","shrinkage"))
# # plot(model_svm, metric=c("RMSE","interaction.depth"))
# # plot(model_svm, metric="n.minobsinnode")
# 
# ggplot(model_gamboost) + theme(legend.position = "top")
# 
# 
# model_gamboost_climate <- predict(model_gamboost, Climate)
# 




# ##################################
# #### Support Vector Machines ####
# ################################
# 
# subset <- c(1:15)
# train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # gbmGrid <-  expand.grid(shrinkage = c(0.01), interaction.depth  = seq(1,7, by=2), n.minobsinnode =10,  n.trees = seq(100,1000, by=50))
# 
# model_svm <-  train(siloMaizeAnomaly ~ ., 
#                     data=data, 
#                     trControl=train_control, 
#                     method="svmRadial", 
#                     preProc = c("center", "scale") )
#                     # ,
#                     # sizes = subsets,
#                     # verbose=TRUE
#                     # tuneGrid = gbmGrid
#                     # )
# 
# model_svm
# model_svm$results
# model_svm$finalModel
# par(mfrow=c(1,1))
# summary(model_svm) ## Shows a plot of the relative Importance of each variables
# predictors(model_svm)
# plot(model_svm)
# # plot(model_svm, metric=c("RMSE","shrinkage"))
# # plot(model_svm, metric=c("RMSE","interaction.depth"))
# # plot(model_svm, metric="n.minobsinnode")
# 
# ggplot(model_svm) + theme(legend.position = "top")
# 
# Climate <-  read_csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_RCMs[[1]],"_19712000_demean.csv", sep=""))
# 
# model_gbm_climate <- predict(model_gbm, Climate)

# #######################################################
# #### Shrinkage Methods: Lasso and Ridge by glmnet ####
# #####################################################
# ' I think the model stays linear'
# 
# subset <- c(1:15)
# train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # gbmGrid <-  expand.grid(shrinkage = c(0.01), interaction.depth  = seq(1,7, by=2), n.minobsinnode =10,  n.trees = seq(100,1000, by=50))
# 
# model_ridge <-  train(siloMaizeAnomaly ~ ., 
#                     data=data, 
#                     trControl=train_control, 
#                     preProc = c("center", "scale"),
#                     method="glmnet" )
# 
# 
# # ,
# # sizes = subsets,
# # verbose=TRUE
# # tuneGrid = gbmGrid
# # )
# 
# model_ridge
# model_ridge$results
# model_ridge$finalModel
# par(mfrow=c(1,1))
# summary(model_ridge) ## Shows a plot of the relative Importance of each variables
# predictors(model_ridge)
# plot(model_ridge)
# # plot(model_svm, metric=c("RMSE","shrinkage"))
# # plot(model_svm, metric=c("RMSE","interaction.depth"))
# # plot(model_svm, metric="n.minobsinnode")
# 
# ggplot(model_ridge) + theme(legend.position = "top")
# 
# 
# model_ridge_climate <- predict(model_ridge, Climate)


################################################################################################################################################
################################################
#### Leave out one year cross - validation ####
##############################################
################################################################################################################################################
# Establish indeces for leave out one year crossvalidation
table(data$year)
in_train <- holdout <- vector(mode = "list", length = 17)
str(in_train)
list <-  list("Fold01", "Fold02", "Fold03", "Fold04", "Fold05", "Fold06", "Fold07", "Fold08" ,"Fold09", "Fold10", "Fold11", "Fold12", "Fold13", "Fold14", "Fold15",
              "Fold16", "Fold17")
names(list) <- c("Fold01", "Fold02", "Fold03", "Fold04", "Fold05", "Fold06", "Fold07", "Fold08" ,"Fold09", "Fold10", "Fold11", "Fold12", "Fold13", "Fold14", "Fold15",
                 "Fold16", "Fold17")
dim(Yield_Covariates_demean)

in_train <- holdout <- list

str(in_train)

in_train[[1]] <- as.integer(row.names(subset(Yield_Covariates_demean, year==2000)))

x <- seq(1999,2015)

head(Yield_Covariates_demean,20)
rownames(Yield_Covariates_demean) <- NULL
row_index <- 1:nrow(Yield_Covariates_demean)
i <- 1
year <- as.integer(row.names(subset(Yield_Covariates_demean, year==x[[i]])))
year
length(row_index[Yield_Covariates_demean$year %in% x[1] ])
length(row_index[!(Yield_Covariates_demean$year %in% x[1] )])

for(i in 1:17)
{
  
  in_train[[i]] <- row_index[!(Yield_Covariates_demean$year %in% x[i])]
  holdout[[i]] <- row_index[Yield_Covariates_demean$year %in% x[i] ]
  
  print(length(in_train[[i]]) + length(holdout[[i]]))
  print(length(in_train[[i]]) )
  print(length(holdout[[i]]) )
}



Yield_Covariates_demean_ordered  <- Yield_Covariates_demean[order(Yield_Covariates_demean$year),]
head(Yield_Covariates_demean_ordered)
Yield_Covariates_demean <- Yield_Covariates_demean_ordered
dim(Yield_Covariates_demean)

ctrl <- trainControl(method = "cv",
                     savePredictions = TRUE,
                     index = holdout,
                     indexOut = in_train)

# train_control <-  trainControl(method="repeatedcv", number=3, repeats=3)
#
# stateCvFoldsIN <- createFolds(C(Yield_Covariates_demean$year), k = 3, returnTrain=TRUE)

table(Yield_Covariates_demean$year)

str(  stateCvFoldsIN )
y <- Yield_Covariates_demean$year

sapply(stateCvFoldsIN, length)

sapply(stateCvFoldsIN, function(i) table(y[i]))

head(Yield_Covariates_demean, 34)[,1:5]

names(Yield_Covariates)
model_lm.fit_demean <- train(siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght",  "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                               dummy(Yield_Covariates$SMI_Aug6, c("svr drght", "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                               poly(Yield_Covariates$Prec_Jul,2 , raw = T) + poly(Yield_Covariates$Tavg_Jul, 2, raw = T) + factor(Yield_Covariates$comId) , data = Yield_Covariates,trControl=ctrl,method="lm")
'Hier gibt es Probleme, ich habe aber gerade keine Zeit, mich darum zu kümmern.'

print(model_lm.fit_demean)
warnings()

first_fold <- subset(model_lm.fit_demean$pred, Resample == "Fold17")

table(model_lm.fit_demean$pred$Resample)

## These were used to fit the model
table(Yield_Covariates_demean$year[-first_fold$rowIndex])
## These were heldout:
table(Yield_Covariates_demean$year[first_fold$rowIndex])

model_lm.fit_demean$finalModel

#### Literature reccommends to use a MARS model on my data ####
