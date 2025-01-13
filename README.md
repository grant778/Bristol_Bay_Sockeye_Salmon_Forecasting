Salmon_Forecasting_Clean_ADFG 
 - The original script used to predict salmon runs in Bristol Bay Alaska.
 - This script uses raw length at age data provided directly from Alaska Department of Fish and Game.
 - In it, you can see how the raw data was processed to obtain mean lengths at age for each day in the fishing season for 1980 through 2023
 - However, as the raw length at age data is property of ADFG, we only provided the datasets of mean length at age over the fishing season (From 1980 to 2023) for ocean age 2 and ocean age 3 sockeye developed from the raw ADFG data to CJFS. 
      - The raw data used can be obtained upon reasonable request from ADFG.
      - As such, unless the user obtains the data from ADFG, this script will not run as the pre-requisite raw size at age data from ADFG is not provided here or on Dryad
However, the timeseries datasets of mean length at age for age 2 and age 3 fish are provided here and on Dryad.
    - These summaries are used in Salmon_Forecasting_Clean_CJFS.R to predict salmon runs.
  
 - Helper Scripts
   * lengthatage_Function_clean.R; Calculates mean size at age from raw ADFG data
   * salmon_regression_models_clean.R; Runs models and calculates performance metrics


Salmon_Forecasting_Clean_CJFS

- This script should run fully using the data provided to CJFS and provided here on GIT.
- The principal difference between Salmon_Forecasting_Clean_CJFS and Salmon_Forecasting_Clean_11_03, as alluded to above is Salmon_Forecasting_Clean_CJFS directly uses the processed length at age datasets

Data Inputs:
  - Age specific size at age data for age 2 and age 3. Rows are years (1980-2023) columns are day of year; Direct model input
    * Age 2 length at age.xlsx; Age 3 length at age.xlsx
  - Preseason Forecast. Preseason run prediction generated in Fall each year for the following summer
    * Preaseason.csv
  - Total Bristol Bay sockeye run 1980 to 2023; Direct model input
    * Total_Run_1980_2023.csv 
  - Total Bristol Bay sockeye run 1979 to 2022; Direct model input
    * Total_Run_Previous_Year_1979to2022.csv
  - North Pacific Pink Salmon abundance 1979 to 2022; Direct model input
    * pinksalmon_1979to2022.csv
  - Ocean temperatures
    * SST-BristolBay3.csv
  - Existing In season Bayesian methodology performance
    * perf.summary_pred; Used for graphic comparing new methodology with current methodology

- Helper Scripts:
  - salmon_regression_models_clean.R; Runs models and calculates performance metrics
      
  
  
