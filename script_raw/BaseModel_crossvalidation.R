#### Description ####
'
Use caret package to undergo crossvalidation for comparing the out - of- sample predictability of some models. 
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





#######################
## use caret package ##

###########################
#### Data Split in R #####
#########################

# load the libraries
library(klaR)
# load the iris dataset
# Maize_meteo <- as.tibble(Maize_meteo)
deselect <-  c(comId, siloMaize)
Maize_meteo <- Maize_meteo  %>% select(siloMaizeAnomaly:SMI_Oct6) 
 
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

####################
#### Bootstrap ####
##################
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

######################################
#### k - fold Cross - Validation ####
##################################### 

# define training control
train_control <- trainControl(method="cv", number=10)

model1 <- train(siloMaizeAnomaly ~ 1 + 
                  poly(P_Jul_demeaned,3) + 
                  I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) +
                  SMI_Jun6 + SMI_Aug6, 
                data=Maize_meteo, 
                trControl=train_control, 
                method="lm")
model1

# train the model
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

###############################################  
#### Repeated k - fold Cross - Validation ####
#############################################
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

model1 <- train(siloMaizeAnomaly ~ 1 + 
                  poly(P_Jul_demeaned,3, raw=TRUE) + 
                  I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) +
                  SMI_Jun6 + SMI_Aug6, 
                data=Maize_meteo, 
                trControl=train_control, 
                method="lm")
model1
summary(model1)
summary(lm(siloMaizeAnomaly ~ 1 + 
          poly(P_Jul_demeaned,3, raw=TRUE) + 
          I(T_Jul_demeaned) +I(T_Jul_demeaned^2) +I(T_Jul_demeaned^3) +
          SMI_Jun6 + SMI_Aug6, 
        data=Maize_meteo))

# train the model
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
                data = Maize_meteo, 
                trControl=train_control, 
                method="lm")
model2


## model selection ##
model_cvSelection <- train(siloMaizeAnomaly ~ .,
                data=Maize_meteo, 
                trControl=train_control,  
                method = "lm",
                metric="Rsquared")
                
summary(model_cvSelection)
predictors(model_cvSelection)

################################################
#### Leave out one year cross - validation ####
##############################################
# Establish indeces for leave out one year crossvalidation
table(Yield_Covariates_demean$year)
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

x<- seq(1999,2015)

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
