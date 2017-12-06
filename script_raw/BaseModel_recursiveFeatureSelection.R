#### Description ####
'Script to retrieve relative variable importance and automated model selection based on cv provided by the caret package
'

#### Input ####
' - Maize_meteo.csv <- provided by BaseModel.R
  - YieldMeteo <- read_csv("./data/data_processed/YieldMeteo.csv")


'

### Output ####
'
Variable importance plots

'


################################################################################################################################################
################################################################################################################################################
rm(list=ls())
source("./script/script_raw/BaseModel.R")

#########################################################
#### Recursive Feature Selection: only linear terms ####
#######################################################
' There are problems with nonlinear terms, since rfe applies factor in accordance to the model.matrix used. 
  Thus, one variable of *_SMI6 ist dividied into 6 variables.
  This is similar with polynomials. Theoretically, it would be possible to transform the data directly. 
  Again, this would results in three variables for a polynomial of degree 3. 
  Overall, I think it very tidious to use the caret package for settings with polynomials and stepwise functions. 
  Further: Relative Variable Importance is based on the size of the test - statistic: better alternatives available. 
  Alternatives: 
    Relative Variable Importance: relaimpo with bootstrap option
    Subset Selection: so far subsect selection based on BIC -> MuMln::dredge, leaps allow subset selection based on cv. 

  '

#######################################################
#### Merge Data to allow for linear considerations ####
YieldMeteo <- read_csv("./data/data_processed/YieldMeteo.csv")
# Maize_meteo <-  read_csv("./data/data_processed/Maize_meteo.csv")
YieldMeteo$year <- as.factor(YieldMeteo$year)  
YieldMeteo$comId <- as.factor(YieldMeteo$comId)  

Maize_meteo

YieldMaizeMeteo <- inner_join(YieldMeteo, Maize_meteo, by=c("year","comId"))
names(YieldMaizeMeteo )

(Maize_meteo$SMI_Aug6 )

##############################
#### Preselect Variables  ####
'Do not use SMI for May, July, and August because of correlation. 
Similar, no use of T and PET '

## Predictors: pure linear ##
x_linear <- YieldMaizeMeteo %>% select(matches("May|Jun|Jul|Aug|Sep|Oct"))
x_linear <- x_linear %>% select(SMI_Jun,  SMI_Aug, SMI_Oct, P_May_demeaned.x:T_Oct_demeaned.x)
names(x_linear) <- gsub(".x","", names(x_linear))
x_linear

## Predictors: with stepwise function ##
x_step <- YieldMaizeMeteo %>% select(matches("May|Jun|Jul|Aug|Sep|Oct"))
x_step <- x_step %>% select(SMI_Jun6,  SMI_Aug6, SMI_Oct6, P_May_demeaned.x:T_Oct_demeaned.x)
names(x_step) <- gsub(".x","", names(x_step))
x_step 

# ## Explained Variables ##
y <- YieldMaizeMeteo %>% select(siloMaizeAnomaly)
dim(y)

############################
#### Combine data to yX for variImp ####
data_linear <- bind_cols(y, x_linear)
data_step <- bind_cols(y, x_step)

#####################################################
#### Convert to matrixes to allow the use in rfe ####
x_linear <- as.matrix(x_linear)
x_step <- as.matrix(x_step)
y <- as.matrix(y)

######################################################################################################################
#### Rank Features by Importance in accordance to the absolute value of the t–statistic for each model parameter ####
####################################################################################################################

## Data_Step ##
control <- trainControl(method="repeatedcv", number=10, repeats=3)

par(mfrow=c(1,2))
plot(importance_step <- varImp(train(siloMaizeAnomaly~., data=data_step, method="lm",  trControl=control), scale=FALSE))
plot(importance_linear <- varImp(train(siloMaizeAnomaly~., data=data_linear, method="lm",  trControl=control), scale=FALSE))
' Here it interesting to see that the importance of July meteorology pretty much depends on Soil Moisture in June. '

##################################################################################
#### Recursive Feature Elimination based on repeated k-fold cross-validation ####
################################################################################

## Define possible numbers of parameters allowed ##
subsets <- c(1:30)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose =TRUE)

lmProfile <- rfe(x_linear, y,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile

lmProfile$variables

predictors(lmProfile)
' Top Variables based on repeated CV (10 fold) according their coefficient size(useless since not normalized):
  [1] "SMI_Jun"        "T_Aug_demeaned" "SMI_Oct"        "T_May_demeaned" "T_Jul_demeaned" "T_Jun_demeaned" "SMI_Aug"        "T_Oct_demeaned"
 [9] "T_Sep_demeaned" "P_Jul_demeaned" "P_Oct_demeaned" "P_Jun_demeaned" "P_Aug_demeaned" "P_Sep_demeaned" "'

lmProfile$fit

head(lmProfile$resample)
lmProfile$results
'
 Variables     RMSE  Rsquared      MAE   RMSESD RsquaredSD     MAESD
1         4 42.79453 0.2378850 32.97099 1.754965 0.04882077 1.2237090
2         8 39.65660 0.3459097 30.04069 1.566518 0.03094848 1.0485912
3        15 38.82594 0.3730436 29.29734 1.580317 0.03044754 0.9675727

'



#### Plot RMSE against number of variables ####
trellis.par.set(caretTheme())

plot(lmProfile, type = c("g", "o"))


