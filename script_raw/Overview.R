
#### Overview of project scripts ####

#### Data Preparation ####
'
1) Meteo_netcdf_to_tidy.R (in Project 1)
  - Input: Meteorological and SMI Data derived from Fortran Scripts (netcdf)
  - Output: Reshaped Data -> tidy format: MeteoMonth_df_tidy.csv

2) BaseData_demean.R
  - Input: MeteoMonth_df_tidy.csv
  - Output: Meteo_train_demean.csv

3) Merge_YieldMeteo_Proj2.R 
  - Input:  Yield Data (Yield2015.csv) and Meteorological and SMI Data (Meteo_train_demean.csv <- BaseData_demean.R)
  - Output: Merge Data Frame: YieldMeteo.csv > /Proj2/data/data_processed/

4) BaseData_PreProcessing.R
  - Also includes generating demeaned data for the period 1999 - 2015 (except SMI)
  - Input:  YieldMeteo.csv <- Merge_YieldMeteo_Proj2.R
  - Output: Average Yield of each com: avgYield_comId.csv -> /Proj2/data/data_processed/
            Maize_meteo (data.frame after preparation scheme) -> /Proj2/data/data_processed/

5) KlimaMeteo_netcdf_to_sf&tidy.R
  - Input:  Meterological and SMI Data derived from the climate models 
            ("DMI","ICTP","KNMI","MPI","SMHI") -> data_proj_input
  - Output: Tidy data.frame (nach reshape) <- /data/data_proj/MeteoMonth_df_tidy_*.csv 

6) KlimaMeteo_Demean
  - Demean meteorological data (T and P) for the period 1971 - 2000
    Append state information
  - Input:  Tidy data.frame (nach reshape) <- /data/data_proj/MeteoMonth_df_tidy_*.csv 
            YieldMeteo.csv
  - Output: MeteoMeonth_df_tidy_19712000_demeaned_*.csv <- ./data/data_proj/ 

'

#### Train Data Evaluation ####
'
1) BaseMeteo_SMI_descriptiveStatistics.R: Descriptive Statistics of SMI for subset of period 1951 - 2015
  - Input:    - MeteoMonth_df_tidy.csv
  - Output:   - data.frame of mean and median of subperiods for SMI_Jun to SMI_Aug
                "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],".csv"
              - Export Summary/ Descriptive Statistics of the SMI Data 
                "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.txt"
                "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Summary.tex"
                "./data/data_processed/Train/Meteo/MeteoMonth_train_tidy_MeanMedian_", namelist_periods[[i]],"_Mean.tex"

2) BaseMode_correlationTable.R
  - Input: - MeteoMonth_df_tidy_demean.csv <-  BaseData_demean.R
  - Output - Correlation Plots of 
'
 
#### Model Exploration ####
'
1) BaseModel_exploration.R
  - Input: Maize_Meteo <- BaseData_PreProcessing.R
  - Output: 
  
2) BaseModel_tests.R
  - Input: Maize_meteo.csv-> /Proj2/data/data_processed/ (BaseData_PreProcessing.R)
  - Output:
  - 

3) BaseModel_crossvalidation.R

4) BaseModel_standardErrors.R

5) BaseModel_Influence.R

'

#### Model Estimation ####
'
1) BaseModel.R
  - Input: Maize_Meteo <- BaseData_PreProcessing.R
  - Output: List of estimated models and lists used in loops
'

#### Projection / Prediction ####
'
1) BaseModel_Predicton.R
  - Iput: BaseModel.R
  - Output: ## Files
            - Maize_meteo including the predicted values from the models in BaseModel.R "./data/data_processed/Maize_meteo_predicted.csv" 
  
            ## Plots
             - maps of predicted silage maize anomalies for each year in the training period ->     
              "./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf"
            
            - com specific sum of predicted values -> /figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf"
              - time series for each com -> ./figures/figures_exploratory/Train/", modelListNames[[i]],"/TimeSeries/administrative_districts/timeSeries_yieldAnomaly_", 
                comId_list$comId[r]

2) Prediction_projections.R
  - Input:  - Maize_meteo.csv 
            - MeteoMonth_df_tidy_*.csv
  - Output: - Prediction of silage maize yield anomalies based on different estimation models for each RCM in namelist_model:
              Climate_predicted -> ./data/data_proj/output/", namelist_RCMs[[r]],"/Climate_predicted.csv
'

#### Projection evaluation ####
'
1) KlimaMeteo_descriptiveStatistics.R
  - Input: Climate_predicted <- Prediction_projections.R 
  - Output: Summary_*, * is climate period -> ./figures/figures_exploratory/Proj/namelist_RCMs
              
2) KlimaMeteo_SMiZeroAndOne.R
  - check the distribution of one and zeros for the RCMs
  - check the SMI Data of the year 2099 on maps
  - Input: "./data/data_proj/","MeteoMonth_df_tidy_", namelist_RCMs[[i]],".csv" <- KlimaMeteo_netcdf_to_sf&tidy.R
  - Output: - ./figures/figures_exploratory/Proj/SMI/oneAndZero_table.csv
            - ./figures/figures_exploratory/Proj/SMI/zeroAndOne_namelist*.pdf

'


#### Plots employing projections of explanatory variables and yield ####
'
1) KlimaMeteo_model_plot.R
  - Look at explanatory variables derived from each RCM
  - Input:  - MeteoMonth_df_tidy_*.csv
            - vg2500_krs -> data_proj_Input/CLC 
  - Output:  - Variables
                - T in July
                - P in July 
                - SMI in June
                - SMI in July
                - SMI in August

              - summary statistics of all explanatory variables of the reference (1971 - 2000) and climate periods (2021 - 2050, 2070 - 2099) 
              and the difference in the climate vs. reference period <- /Proj2/figures/figures_exploratory/Proj/MeteoVar/
                <- "./figures/figures_exploratory/Proj/MeteoVar/","DeskriptiveStats_diff2021_", namelist_models[[l]], ".txt"
                *namelist_models[[t]] is RCM

              - Plots <- /Proj2/figures/figures_exploratory/Proj/MeteoVar/
               - Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
               - Plot of absolute values (same for Mean and SD)  for each climate period 

2) KlimaMeteo_total_plot.R
  - Look at explanatory variables derived from each RCM - now the average of the RCMs
  - Input:  - MeteoMonth_df_tidy_*.csv
            - vg2500_krs -> data_proj_Input/CLC 
  - Output: same as in KlimaMeteo:plot.R but employing the average over all RCMs.

3) Prediction_Plots.R
  - Look at yield derived from each prediction models and RCM: maps and descriptive Statistics 
  - Input:  - vg2500_krs -> data_proj_Input/CLC 
            - Climate_predicted.csv
  - Output: - summary statistics of yield for reference (1971 - 2000) and climate periods (2021 - 2050, 2070 - 2099) 
              and the difference in the climate vs. reference period via stargazer
              <- "./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_2070_", namelist_models[[t]],".txt"
            Plots:
            - Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
            - Plots of absolute values (same for Mean and SD)  for each climate period 
            <- "./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_mean_Y_", namelist_models[[t]],".pdf"
              *modelListMatrixNames[[s]] is prediction model
              *namelist_models[[t]] is RCM

4) Prediction_Plots_average.R
  - Look at yield derived from each prediction models and RCM - now the average of the RCMs
  - Input:  - Climate_predicted.csv
            - vg2500_krs -> data_proj_Input/CLC 
  - Output: same as in Prediction_Plots.R but employing the average over all RCMs. Uses additionally "average_all_models" in name. 


5) Prediction_KlimaMeteo_Combined_Plots.R
  - Make combined plots of the absolute values in the reference period and the difference of each climate period
  - Input: KlimaMeteo_model_plot.R, KlimaMeteo_total_plot.R and BasePrediction_Plots_Average.R and BasePrediction_Plot_Model.R.
  - Output: Combined Plots of Mean

6) Prediction_Boxplots.R
  - Make boxplots of the differences between climate periods and the reference period
  -Input
    - source("./script/script_raw/BaseModel.R")
    - PredictData_df_tidy[[s]] <- read_csv(paste("./data/data_proj/output/",  namelist_RCMs[[s]],"/Climate_predicted.csv", sep="") )
  - Output
    - ./figures/figures_exploratory/Proj/Boxplots/", nameList_climate[[i]],"/ViolinPlot_*.pdf,  * = various boxplots
'