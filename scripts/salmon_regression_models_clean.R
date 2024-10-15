#This script takes a variety of covariates as well as a dataframe of average lengths at age and runs a dozen different models.
#The script then predicts the run size from these covariates in year t+1 (also using size at age from year t+1)

#Inputs to the function

#meanLengths - a dataframe where rows represent years and columns represent days of the season. 
#Values represent average length at age through that day in the season. This is an age specific dataframe so should contain fish of a single age

#day - day of year

#Total_Run - The sockeye salmon run size in year t

#Total_Run_Previous_Year - The sockeye salmon run size in year t-1

#pinksalmonPrevious_Year - the north pacific pink salmon abundance in year t-1

#Odd_Even_Year - whether year t is odd or even (used to represent if year t is a major pink salmon spawning year or not)

#PreviousSummerMeanTemp - average SST from June through August the summer prior to year t

#PreviousWinterMeanTemp - average SST from December through February the winter prior to fishing season t

#legendLocation - specifies where to place the legend with ggplot

#startyear - first year t 
#IMPORTANT NOTE: startyear is not including the autoregressive terms years, so if total run starts in 1980 but has an autoregressive term (total_run_previous_year) which starts in 1979, enter 1980 here

#endyear - last year t
#IMPORTANT NOTE: startyear is not including the autoregressive terms years, so if total run ends in 2023 but has an autoregressive term (total_run_previous_year) which ends in 2022, enter 2023 here

#start_test_year - first year user wants to begin predicting run size in

one_step_ahead_regression= function(meanLengths,age,day,Total_Run,Total_Run_Previous_Year,pinksalmonPrevious_Year,Odd_Even_Year,PreviousSummerMeanTemp,PreviousWinterMeanTemp,legendLocation = "bottomright",startyear,endyear,start_test_year)
  
{
  
#Read in lists to hold the results of each model

#This model is total run ~ mean length at age  
regressionoutputsBase = list()

#This model is total run ~ mean length at age + previous years' sockeye return

regressionoutputs = list()

#This model is total run ~ mean length at age + previous year's sockeye return + pink salmon abundance in the previous year

regressionoutputsPinkSalmon = list() 

#This model is total run ~ mean length at age + previous year's sockeye return + odd or even year

regressionoutputsPinkSalmon_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable (variable indicates odd or even year)

#This model is total run ~ mean length at age + previous year's sockeye return + pink salmon abundance in the previous year + previous summer temperature

regressionoutputsPinkSalmonSummerTemp = list() 

#This model is total run ~ mean length at age + previous year's sockeye return + odd or even year + previous summer temperature

regressionoutputsPinkSalmonSummerTemp_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ mean length at age + previous year's sockeye return + previous summer temp

regressionoutputsSummerTemp_No_Pinks = list()

#This model is total run ~ mean length at age + previous year's sockeye return + pink salmon abundance in the previous year + previous winter mean SST

regressionoutputsPinkSalmonWinterTemp = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ mean length at age + previous year's sockeye return + odd or even year + previous winter mean SST

regressionoutputsPinkSalmonWinterTemp_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ mean length at age + previous year's sockeye return + previous winter mean SST 

regressionoutputsWinterTemp_No_Pinks = list()

#This model is total run ~ mean length at age + previous year's sockeye return + pink salmon abundance in the previous year + previous summer mean SST + previous winter mean SST

regressionoutputs_covariates_pink_abundance = list()

#This model is total run ~ mean length at age + previous year's sockeye return + odd or even year + previous summer mean SST + previous winter mean SST

regressionoutputs_covariates_pink_year = list()


#SaA EXCLUDED models
#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + pink salmon abundance in the previous year

regressionoutputs_no_SaA_PinkSalmon = list() 

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + odd or even year

regressionoutputs_no_SaA_PinkSalmon_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable (variable indicates odd or even year)

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + pink salmon abundance in the previous year + previous summer temperature

regressionoutputs_no_SaA_PinkSalmonSummerTemp = list() 

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + odd or even year + previous summer temperature

regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + previous summer temp

regressionoutputs_no_SaA_SummerTemp_No_Pinks = list()

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + pink salmon abundance in the previous year + previous winter mean SST

regressionoutputs_no_SaA_PinkSalmonWinterTemp = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + odd or even year + previous winter mean SST

regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd = list() #Binary cyclic pink salmon abundant year or not added as variable 

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + previous winter mean SST 

regressionoutputs_no_SaA_WinterTemp_No_Pinks = list()

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + pink salmon abundance in the previous year + previous summer mean SST + previous winter mean SST

regressionoutputs_no_SaA_covariates_pink_abundance = list()

#This model is total run ~ SaA EXCLUDED + previous year's sockeye return + odd or even year + previous summer mean SST + previous winter mean SST

regressionoutputs_no_SaA_covariates_pink_year = list()

#This model is total run ~ previous year's sockeye return

regressionoutputs_Salmon_Previous_Year_Only  = list()





#Identify years with no average length at age on the entered day of the season
#Identify years with average length at age with 1, and years missing data with 0

NA_Index = c(rep(1,length(meanLengths[,day-164])))

NA_Index[is.na(meanLengths[,day-164])] = 0 #0 means No data


#Remove na years from data vectors

time=seq(startyear,endyear)[which(NA_Index == 1)]

meanLengths = meanLengths[which(NA_Index == 1),]



Total_Run_Previous_Year = Total_Run_Previous_Year[which(NA_Index == 1)]
pinksalmonPrevious_Year = pinksalmonPrevious_Year[which(NA_Index == 1)]
Odd_Even_Year = Odd_Even_Year[which(NA_Index == 1)]
PreviousSummerMeanTemp = PreviousSummerMeanTemp[which(NA_Index == 1)]
PreviousWinterMeanTemp = PreviousWinterMeanTemp[which(NA_Index == 1)]
Total_Run = Total_Run[which(NA_Index == 1)]


length(time[which(time >= start_test_year)])

#Read in outputs for data summaries of models

ANOVA_Info_All_Years = list()

Summary_1 = list()
ANOVA_1 = list()

Summary_2 = list()
ANOVA_2 = list()

Summary_3 = list()
ANOVA_3 = list()

Summary_4 = list()
ANOVA_4 = list()

Summary_5 = list()
ANOVA_5 = list()

Summary_6 = list()
ANOVA_6 = list()

Summary_7 = list()
ANOVA_7 = list()

Summary_8 = list()
ANOVA_8 = list()

Summary_9 = list()
ANOVA_9 = list()

Summary_10 = list()
ANOVA_10 = list()

Summary_11 = list()
ANOVA_11 = list()

Summary_12 = list()
ANOVA_12 = list()

Summary_13 = list()
ANOVA_13 = list()

Summary_14 = list()
ANOVA_14 = list()

Summary_15 = list()
ANOVA_15 = list()

Summary_16 = list()
ANOVA_16 = list()

Summary_17 = list()
ANOVA_17 = list()

Summary_18 = list()
ANOVA_18 = list()

Summary_19 = list()
ANOVA_19 = list()

Summary_20 = list()
ANOVA_20 = list()

Summary_21 = list()
ANOVA_21 = list()

Summary_22 = list()
ANOVA_22 = list()

Summary_23 = list()
ANOVA_23 = list()
#Run the models looping through each year

iter = 1

for(i in (nrow(meanLengths)-(length(time[which(time >= start_test_year)]))):(nrow(meanLengths)-1))
{
  
  regressionoutputsBase[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)])
  #acf(regressionoutputsBase[[i]][["residuals"]])
  
  
  
  regressionoutputs[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i])
  #acf(regressionoutputs[[i]][["residuals"]])

  regressionoutputsPinkSalmon[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i])
  
  regressionoutputsPinkSalmon_Even_Odd[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i])
  #acf(regressionoutputsPinkSalmon[[i]][["residuals"]])
  

  
  regressionoutputsPinkSalmonSummerTemp[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousSummerMeanTemp[1:i])
  
  # cat("\n\nModel 5 regression")
  # print(summary(regressionoutputsPinkSalmonSummerTemp[[iter]]))
  # 
  regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousSummerMeanTemp[1:i])
  #acf(regressionoutputsPinkSalmonSummerTemp[[i]][["residuals"]])
  
  regressionoutputsSummerTemp_No_Pinks[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+PreviousSummerMeanTemp[1:i])
  
  #cat("\n\nModel 7 regression")
  #print(summary(regressionoutputsSummerTemp_No_Pinks[[iter]]))
  
  regressionoutputsPinkSalmonWinterTemp[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousWinterMeanTemp[1:i])
  #acf(regressionoutputsPinkSalmonWinterTemp[[i]][["residuals"]])
  
  regressionoutputsWinterTemp_No_Pinks[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+PreviousWinterMeanTemp[1:i])
  
  
  regressionoutputs_covariates_pink_abundance[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputs_covariates_pink_year[[iter]] = lm(Total_Run[1:i]~meanLengths[1:i,ncol(meanLengths)]+Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  # SaA EXCLUDED
  regressionoutputs_Salmon_Previous_Year_Only[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i])
  
  
  regressionoutputs_no_SaA_PinkSalmon[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i])
  
  regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i])
  #acf(regressionoutputs_no_SaA_PinkSalmon[[i]][["residuals"]])
  
  
  
  regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousSummerMeanTemp[1:i])
  
  # cat("\n\nModel 5 regression")
  # print(summary(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]]))
  # 
  regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousSummerMeanTemp[1:i])
  #acf(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[i]][["residuals"]])
  
  regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+PreviousSummerMeanTemp[1:i])
  
  #cat("\n\nModel 7 regression")
  #print(summary(regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]]))
  
  regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousWinterMeanTemp[1:i])
  #acf(regressionoutputs_no_SaA_PinkSalmonWinterTemp[[i]][["residuals"]])
  
  regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+PreviousWinterMeanTemp[1:i])
  
  
  regressionoutputs_no_SaA_covariates_pink_abundance[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputs_no_SaA_covariates_pink_year[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  
  
  #Save model results to output lists
  
  Summary_1[[iter]] = summary(regressionoutputsBase[[iter]])
  ANOVA_1[[iter]] = anova(regressionoutputsBase[[iter]])
  
  
  Summary_2[[iter]] = summary(regressionoutputs[[iter]])
  ANOVA_2[[iter]] = anova(regressionoutputs[[iter]])
  
  Summary_3[[iter]] = summary(regressionoutputsPinkSalmon[[iter]])
  ANOVA_3[[iter]] = anova(regressionoutputsPinkSalmon[[iter]])
  
  Summary_4[[iter]] = summary(regressionoutputsPinkSalmon_Even_Odd[[iter]])
  ANOVA_4[[iter]] = anova(regressionoutputsPinkSalmon_Even_Odd[[iter]])
  
  Summary_5[[iter]] = summary(regressionoutputsPinkSalmonSummerTemp[[iter]])
  ANOVA_5[[iter]] = anova(regressionoutputsPinkSalmonSummerTemp[[iter]])
  
  Summary_6[[iter]] = summary(regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]])
  ANOVA_6[[iter]] = anova(regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]])
  
  
  Summary_7[[iter]] = summary(regressionoutputsSummerTemp_No_Pinks[[iter]])
  ANOVA_7[[iter]] = anova(regressionoutputsSummerTemp_No_Pinks[[iter]])
  
  Summary_8[[iter]] = summary(regressionoutputsPinkSalmonWinterTemp[[iter]])
  ANOVA_8[[iter]] = anova(regressionoutputsPinkSalmonWinterTemp[[iter]])
  
  Summary_9[[iter]] = summary(regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]])
  ANOVA_9[[iter]] = anova(regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]])
  
  Summary_10[[iter]] = summary(regressionoutputsWinterTemp_No_Pinks[[iter]])
  ANOVA_10[[iter]] = anova(regressionoutputsWinterTemp_No_Pinks[[iter]])
  
  Summary_11[[iter]] = summary(regressionoutputs_covariates_pink_abundance[[iter]])
  ANOVA_11[[iter]] = anova(regressionoutputs_covariates_pink_abundance[[iter]])
  
  Summary_12[[iter]] = summary(regressionoutputs_covariates_pink_year[[iter]])
  ANOVA_12[[iter]] = anova(regressionoutputs_covariates_pink_year[[iter]])
  
  #SaA EXCLUDED
  Summary_13[[iter]] = summary(regressionoutputs_Salmon_Previous_Year_Only[[iter]])
  ANOVA_13[[iter]] = anova(regressionoutputs_Salmon_Previous_Year_Only[[iter]])
  
  # 
  
  Summary_14[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmon[[iter]])
  ANOVA_14[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmon[[iter]])
  
  Summary_15[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]])
  ANOVA_15[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]])
  
  Summary_16[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]])
  ANOVA_16[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]])
  
  Summary_17[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]])
  ANOVA_17[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]])
  
  
  Summary_18[[iter]] = summary(regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]])
  ANOVA_18[[iter]] = anova(regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]])
  
  Summary_19[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]])
  ANOVA_19[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]])
  
  Summary_20[[iter]] = summary(regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]])
  ANOVA_20[[iter]] = anova(regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]])
  
  Summary_21[[iter]] = summary(regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]])
  ANOVA_21[[iter]] = anova(regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]])
  
  Summary_22[[iter]] = summary(regressionoutputs_no_SaA_covariates_pink_abundance[[iter]])
  ANOVA_22[[iter]] = anova(regressionoutputs_no_SaA_covariates_pink_abundance[[iter]])
  
  Summary_23[[iter]] = summary(regressionoutputs_no_SaA_covariates_pink_year[[iter]])
  ANOVA_23[[iter]] = anova(regressionoutputs_no_SaA_covariates_pink_year[[iter]])
    print(iter)

  
  iter = iter+1
  
}


ANOVA_Info_All_Years =list(Summary_1,ANOVA_1,
                           Summary_2,ANOVA_2,
                           Summary_3,ANOVA_3,
                           Summary_4,ANOVA_4,
                           Summary_5,ANOVA_5,
                           Summary_6,ANOVA_6,
                           Summary_7,ANOVA_7,
                           Summary_8,ANOVA_8,
                           Summary_9,ANOVA_9,
                           Summary_10,ANOVA_10,
                           Summary_11,ANOVA_11,
                           Summary_12,ANOVA_12,
                           Summary_13,ANOVA_13,
                           Summary_13,ANOVA_14,
                           Summary_13,ANOVA_15,
                           Summary_13,ANOVA_16,
                           Summary_13,ANOVA_17,
                           Summary_13,ANOVA_18,
                           Summary_13,ANOVA_19,
                           Summary_13,ANOVA_20,
                           Summary_13,ANOVA_21,
                           Summary_13,ANOVA_22,
                           Summary_13,ANOVA_23)

ModelPredictions = array(dim = c(length(time[which(time >= start_test_year)]),23))



iter = 1

#Loop through test years to generate predictions for each model

for(i in (nrow(meanLengths)-(length(time[which(time >= start_test_year)]))):(nrow(meanLengths)-1))
{
  
  
  #Extract coefficients for each model
  
  c0 = as.vector(regressionoutputsBase[[iter]][["coefficients"]])
  c1 = as.vector(regressionoutputs[[iter]][["coefficients"]])

  c2 = as.vector(regressionoutputsPinkSalmon[[iter]][["coefficients"]])
  c3 = as.vector(regressionoutputsPinkSalmon_Even_Odd[[iter]][["coefficients"]])
  c4 = as.vector(regressionoutputsPinkSalmonSummerTemp[[iter]][["coefficients"]])
  c5 = as.vector(regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]][["coefficients"]])
  c6 = as.vector(regressionoutputsSummerTemp_No_Pinks[[iter]][["coefficients"]])
  c7 = as.vector(regressionoutputsPinkSalmonWinterTemp[[iter]][["coefficients"]])
  c8 = as.vector(regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]][["coefficients"]])
  c9 = as.vector(regressionoutputsWinterTemp_No_Pinks[[iter]][["coefficients"]])
  c10 = as.vector(regressionoutputs_covariates_pink_abundance[[iter]][["coefficients"]])
  c11 = as.vector(regressionoutputs_covariates_pink_year[[iter]][["coefficients"]])
  
  #NO SaA
  c12 = as.vector(regressionoutputs_Salmon_Previous_Year_Only[[iter]][["coefficients"]])
  c13 = as.vector(regressionoutputs_no_SaA_PinkSalmon[[iter]][["coefficients"]])
  c14 = as.vector(regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]][["coefficients"]])
  c15 = as.vector(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]][["coefficients"]])
  c16 = as.vector(regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]][["coefficients"]])
  c17 = as.vector(regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]][["coefficients"]])
  c18 = as.vector(regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]][["coefficients"]])
  c19 = as.vector(regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]][["coefficients"]])
  c20 = as.vector(regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]][["coefficients"]])
  c21 = as.vector(regressionoutputs_no_SaA_covariates_pink_abundance[[iter]][["coefficients"]])
  c22 = as.vector(regressionoutputs_no_SaA_covariates_pink_year[[iter]][["coefficients"]])

  
  #Extract residuals for each model
  r0 = as.vector(regressionoutputsBase[[iter]][["residuals"]])
  r1 = as.vector(regressionoutputs[[iter]][["residuals"]])
  r2 = as.vector(regressionoutputsPinkSalmon[[iter]][["residuals"]])
  r3 = as.vector(regressionoutputsPinkSalmon_Even_Odd[[iter]][["residuals"]])
  r4 = as.vector(regressionoutputsPinkSalmonSummerTemp[[iter]][["residuals"]])
  r5 = as.vector(regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]][["residuals"]])
  r6 = as.vector(regressionoutputsSummerTemp_No_Pinks[[iter]][["residuals"]])
  r7 = as.vector(regressionoutputsPinkSalmonWinterTemp[[iter]][["residuals"]])
  r8 = as.vector(regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]][["residuals"]])
  r9 = as.vector(regressionoutputsWinterTemp_No_Pinks[[iter]][["residuals"]])
  r10 = as.vector(regressionoutputs_covariates_pink_abundance[[iter]][["residuals"]])
  r11 = as.vector(regressionoutputs_covariates_pink_year[[iter]][["residuals"]])
  
  #NO SaA
  
  r12 = as.vector(regressionoutputs_Salmon_Previous_Year_Only[[iter]][["residuals"]])
  r13 = as.vector(regressionoutputs_no_SaA_PinkSalmon[[iter]][["residuals"]])
  r14 = as.vector(regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]][["residuals"]])
  r15 = as.vector(regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]][["residuals"]])
  r16 = as.vector(regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]][["residuals"]])
  r17 = as.vector(regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]][["residuals"]])
  r18 = as.vector(regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]][["residuals"]])
  r19 = as.vector(regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]][["residuals"]])
  r20 = as.vector(regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]][["residuals"]])
  r21 = as.vector(regressionoutputs_no_SaA_covariates_pink_abundance[[iter]][["residuals"]])
  r22 = as.vector(regressionoutputs_no_SaA_covariates_pink_year[[iter]][["residuals"]])
  
  
  years = seq(start_test_year, 2023, 1)
  
#Generate predictions using the input data vectors and model term coefficients extracted above for each model in year t+1
#IMPORTANT NOTE: Autoregressive input vectors should already be lagged. You will note there is no lag added here (all vectors are year i+1)
#Any lagged vectors must already start and end 1 year below the non-lagged vectors for lag equals 1 year.
  
  p0 = c0[1]+c0[2]*meanLengths[i+1,day-164]
  p1 = c1[1]+c1[2]*meanLengths[i+1,day-164]+c1[3]*Total_Run_Previous_Year[i+1]

  p2 = c2[1]+c2[2]*meanLengths[i+1,day-164]+c2[3]*Total_Run_Previous_Year[i+1]+c2[4]*pinksalmonPrevious_Year[i+1]
  p3 = c3[1]+c3[2]*meanLengths[i+1,day-164]+c3[3]*Total_Run_Previous_Year[i+1]+c3[4]*Odd_Even_Year[i+1]
  
  p4 = c4[1]+c4[2]*meanLengths[i+1,day-164]+c4[3]*Total_Run_Previous_Year[i+1]+c4[4]*pinksalmonPrevious_Year[i+1]+c4[5]*PreviousSummerMeanTemp[i+1]
  p5 = c5[1]+c5[2]*meanLengths[i+1,day-164]+c5[3]*Total_Run_Previous_Year[i+1]+c5[4]*Odd_Even_Year[i+1]+c5[5]*PreviousSummerMeanTemp[i+1]
  p6 = c6[1]+c6[2]*meanLengths[i+1,day-164]+c6[3]*Total_Run_Previous_Year[i+1]+c6[4]*PreviousSummerMeanTemp[i+1]
  
  p7 = c7[1]+c7[2]*meanLengths[i+1,day-164]+c7[3]*Total_Run_Previous_Year[i+1]+c7[4]*pinksalmonPrevious_Year[i+1]+c7[5]*PreviousWinterMeanTemp[i+1]
  p8 = c8[1]+c8[2]*meanLengths[i+1,day-164]+c8[3]*Total_Run_Previous_Year[i+1]+c8[4]*Odd_Even_Year[i+1]+c8[5]*PreviousWinterMeanTemp[i+1]
  p9 = c9[1]+c9[2]*meanLengths[i+1,day-164]+c9[3]*Total_Run_Previous_Year[i+1]+c9[4]*PreviousWinterMeanTemp[i+1]
  
  p10 = c10[1]+c10[2]*meanLengths[i+1,day-164] + c10[3]*Total_Run_Previous_Year[i+1]+c10[4]*pinksalmonPrevious_Year[i+1]+c10[5]*PreviousSummerMeanTemp[i+1]+c10[6]*PreviousWinterMeanTemp[i+1]
  p11 = c11[1]+c11[2]*meanLengths[i+1,day-164] + c10[3]*Total_Run_Previous_Year[i+1]+c11[4]*Odd_Even_Year[i+1]+c11[5]*PreviousSummerMeanTemp[i+1]+c11[6]*PreviousWinterMeanTemp[i+1]
 
  #No SaA
   
  p12 = c12[1] + c12[2]*Total_Run_Previous_Year[i+1]
  
  p13 = c13[1]+c13[2]*Total_Run_Previous_Year[i+1]+c13[3]*pinksalmonPrevious_Year[i+1]
  p14 = c14[1]+c14[2]*Total_Run_Previous_Year[i+1]+c14[3]*Odd_Even_Year[i+1]
  
  p15 = c15[1]+c15[2]*Total_Run_Previous_Year[i+1]+c15[3]*pinksalmonPrevious_Year[i+1]+c15[4]*PreviousSummerMeanTemp[i+1]
  p16 = c16[1]+c16[2]*Total_Run_Previous_Year[i+1]+c16[3]*Odd_Even_Year[i+1]+c16[4]*PreviousSummerMeanTemp[i+1]
  p17 = c17[1]+c17[2]*Total_Run_Previous_Year[i+1]+c17[3]*PreviousSummerMeanTemp[i+1]
  
  p18 = c18[1]+c18[2]*Total_Run_Previous_Year[i+1]+c18[3]*pinksalmonPrevious_Year[i+1]+c18[4]*PreviousWinterMeanTemp[i+1]
  p19 = c19[1]+c19[2]*Total_Run_Previous_Year[i+1]+c19[3]*Odd_Even_Year[i+1]+c19[4]*PreviousWinterMeanTemp[i+1]
  p20 = c20[1]+c20[2]*Total_Run_Previous_Year[i+1]+c20[3]*PreviousWinterMeanTemp[i+1]
  
  p21 = c21[1]+ c21[2]*Total_Run_Previous_Year[i+1]+c21[3]*pinksalmonPrevious_Year[i+1]+c21[4]*PreviousSummerMeanTemp[i+1]+c21[5]*PreviousWinterMeanTemp[i+1]
  p22 = c22[1]+ c21[2]*Total_Run_Previous_Year[i+1]+c22[3]*Odd_Even_Year[i+1]+c22[4]*PreviousSummerMeanTemp[i+1]+c22[5]*PreviousWinterMeanTemp[i+1]
  

  #Save model predictions to dataframe
  
  ModelPredictions[iter,1] = p0
  ModelPredictions[iter,2] = p1
  ModelPredictions[iter,3] = p2
  ModelPredictions[iter,4] = p3
  ModelPredictions[iter,5] = p4
  ModelPredictions[iter,6] = p5
  ModelPredictions[iter,7] = p6
  ModelPredictions[iter,8] = p7
  ModelPredictions[iter,9] = p8
  ModelPredictions[iter,10] = p9
  ModelPredictions[iter,11] = p10
  ModelPredictions[iter,12] = p11
  ModelPredictions[iter,13] = p12
  ModelPredictions[iter,14] = p13
  ModelPredictions[iter,15] = p14
  ModelPredictions[iter,16] = p15
  ModelPredictions[iter,17] = p16
  ModelPredictions[iter,18] = p17
  ModelPredictions[iter,19] = p18
  ModelPredictions[iter,20] = p19
  ModelPredictions[iter,21] = p20
  ModelPredictions[iter,22] = p21
  ModelPredictions[iter,23] = p22
 
  #Save model fits
  
  fit0 = (regressionoutputsBase[[iter]][["fitted.values"]])
  fit1 = (regressionoutputs[[iter]][["fitted.values"]])
  fit2 = (regressionoutputsPinkSalmon[[iter]][["fitted.values"]])
  fit3 =  (regressionoutputsPinkSalmon_Even_Odd[[iter]][["fitted.values"]])
  
  
  fit4 = (regressionoutputsPinkSalmonSummerTemp[[iter]][["fitted.values"]])
  fit5 = (regressionoutputsPinkSalmonSummerTemp_Even_Odd[[iter]][["fitted.values"]])
  fit6 = (regressionoutputsSummerTemp_No_Pinks[[iter]][["fitted.values"]])
  
  
  fit7 = (regressionoutputsPinkSalmonWinterTemp[[iter]][["fitted.values"]])
  fit8 = (regressionoutputsPinkSalmonWinterTemp_Even_Odd[[iter]][["fitted.values"]])
  fit9 = (regressionoutputsWinterTemp_No_Pinks[[iter]][["fitted.values"]])
  
  
  fit10 = (regressionoutputs_covariates_pink_abundance[[iter]][["fitted.values"]])
  fit11 = (regressionoutputs_covariates_pink_year[[iter]][["fitted.values"]])
  
  #NO SaA
  
  fit12 = (regressionoutputs_Salmon_Previous_Year_Only[[iter]][["fitted.values"]])

  fit13 = (regressionoutputs_no_SaA_PinkSalmon[[iter]][["fitted.values"]])
  fit14 =  (regressionoutputs_no_SaA_PinkSalmon_Even_Odd[[iter]][["fitted.values"]])
  
  
  fit15 = (regressionoutputs_no_SaA_PinkSalmonSummerTemp[[iter]][["fitted.values"]])
  fit16 = (regressionoutputs_no_SaA_PinkSalmonSummerTemp_Even_Odd[[iter]][["fitted.values"]])
  fit17 = (regressionoutputs_no_SaA_SummerTemp_No_Pinks[[iter]][["fitted.values"]])
  
  
  fit18 = (regressionoutputs_no_SaA_PinkSalmonWinterTemp[[iter]][["fitted.values"]])
  fit19 = (regressionoutputs_no_SaA_PinkSalmonWinterTemp_Even_Odd[[iter]][["fitted.values"]])
  fit20 = (regressionoutputs_no_SaA_WinterTemp_No_Pinks[[iter]][["fitted.values"]])
  
  
  fit21 = (regressionoutputs_no_SaA_covariates_pink_abundance[[iter]][["fitted.values"]])
  fit22 = (regressionoutputs_no_SaA_covariates_pink_year[[iter]][["fitted.values"]])


  yearsvector = time
  
  
  # print(years[iter])
  # print(iter)
  
  iter = iter+1
  
}

#Form a dataframe with within sample fits AND out of sample predictions

rownames(ModelPredictions) = time[which(time >= start_test_year)]

 
retrospectiveandonestepahead1 = c(fit0[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,1])
retrospectiveandonestepahead2 = c(fit1[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,2])
retrospectiveandonestepahead3 = c(fit2[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,3])
retrospectiveandonestepahead4 = c(fit3[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,4])
retrospectiveandonestepahead5 = c(fit4[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,5])
retrospectiveandonestepahead6 = c(fit5[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,6])
retrospectiveandonestepahead7 = c(fit6[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,7])
retrospectiveandonestepahead8 = c(fit7[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,8])
retrospectiveandonestepahead9 = c(fit8[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,9])
retrospectiveandonestepahead10 = c(fit9[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,10])
retrospectiveandonestepahead11 = c(fit10[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,11])
retrospectiveandonestepahead12 = c(fit11[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,12])

#No SaA
retrospectiveandonestepahead13 = c(fit12[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,13])
retrospectiveandonestepahead14 = c(fit13[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,14])
retrospectiveandonestepahead15 = c(fit14[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,15])
retrospectiveandonestepahead16 = c(fit15[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,16])
retrospectiveandonestepahead17 = c(fit16[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,17])
retrospectiveandonestepahead18 = c(fit17[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,18])
retrospectiveandonestepahead19 = c(fit18[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,19])
retrospectiveandonestepahead20 = c(fit19[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,20])
retrospectiveandonestepahead21 = c(fit20[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,21])
retrospectiveandonestepahead22 = c(fit21[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,22])
retrospectiveandonestepahead23 = c(fit22[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,23])


#par(mfrow=c(1,2))




retrospectiveandonestepahead = rbind(
retrospectiveandonestepahead1,  
retrospectiveandonestepahead2,  
retrospectiveandonestepahead3, 
retrospectiveandonestepahead4, 
retrospectiveandonestepahead5,
retrospectiveandonestepahead6,
retrospectiveandonestepahead7,
retrospectiveandonestepahead8,
retrospectiveandonestepahead9,
retrospectiveandonestepahead10,
retrospectiveandonestepahead11,
retrospectiveandonestepahead12,
retrospectiveandonestepahead13,
retrospectiveandonestepahead14,
retrospectiveandonestepahead15,
retrospectiveandonestepahead16,
retrospectiveandonestepahead17,
retrospectiveandonestepahead18,
retrospectiveandonestepahead19,
retrospectiveandonestepahead20,
retrospectiveandonestepahead21,
retrospectiveandonestepahead22,
retrospectiveandonestepahead23
)

row.names(retrospectiveandonestepahead) = c("Model 1", "Model 2","Model 3", "Model 4","Model 5","Model 6", "Model 7", "Model 8","Model 9", "Model 10", "Model 11", "Model 12", "Model 13", "Model 14", "Model 15", "Model 16", "Model 17", "Model 18", "Model 19", "Model 20", "Model 21", "Model 22", "Model 23")

colnames(retrospectiveandonestepahead) = row.names(meanLengths)[which(is.na(meanLengths[,day-164]) == FALSE)]

Predictions=list(retrospective_and_one_step_ahead=retrospectiveandonestepahead,one_step_ahead = ModelPredictions,  ANOVA = ANOVA_Info_All_Years)

#Return a dataframe with within sample fits AND out of sample predictions for each model
return(Predictions)

}


#Generate mean absolute error between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

Mean_Percent_Absolute_Error = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  
  Absolute_Error = array(dim = c(nrow(ModelPredictions),ncol(ModelPredictions)))
 

  for(i in 1:nrow(ModelPredictions))
  {
    for(j in 1:ncol(ModelPredictions))
    {
      Absolute_Error[i,j] = abs((ModelPredictions_All_Ages[i,j]-observations[i])/observations[i])
    }
  }
  
  
  
  return(colMeans(Absolute_Error))
}


#Generate correlation between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

corelation_function = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  correlation= c()
  
  for(j in 1:ncol(ModelPredictions))
  {
    correlation[j] = cor(x=ModelPredictions[,j], y=observations)
  }
  
  print(correlation)
  
}


#Generate maximum absolute error between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

Maximum_Error = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  
  Absolute_Error = array(dim = c(nrow(ModelPredictions),ncol(ModelPredictions)))
  
  Max_error = c()
  
    for(j in 1:ncol(ModelPredictions))
    {
      for(i in 1:nrow(ModelPredictions))
      {
        
      Absolute_Error[i,j] = abs((ModelPredictions_All_Ages[i,j]-observations[i])/observations[i])
    }
      Max_error[j] = max(Absolute_Error[,j])
    } 
  
  
  
  
  return(Max_error)
}

#Generate Standard Deviation of the error between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

SD_Error = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  
  Absolute_Error = array(dim = c(nrow(ModelPredictions),ncol(ModelPredictions)))
  
  SD_error = c()
  
  for(j in 1:ncol(ModelPredictions))
  {
    for(i in 1:nrow(ModelPredictions))
    {
      
      Absolute_Error[i,j] = abs((ModelPredictions_All_Ages[i,j]-observations[i]))
    }
    SD_error[j] = sd(Absolute_Error[,j], na.rm = TRUE)
  } 
  
  

  return(SD_error)
}


#Generate Standard Deviation of the relative error between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

SD_Relative_Error = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  
  Absolute_Error = array(dim = c(nrow(ModelPredictions),ncol(ModelPredictions)))
  
  SD_error = c()
  
  for(j in 1:ncol(ModelPredictions))
  {
    for(i in 1:nrow(ModelPredictions))
    {
      
      Absolute_Error[i,j] = abs((ModelPredictions_All_Ages[i,j]-observations[i]))/observations[i]
    }
    SD_error[j] = sd(Absolute_Error[,j], na.rm = TRUE)
  } 
  
  
  
  return(SD_error)
}



#Generate Percent Absolute Error between model prediction data and total run observation data
#Model prediction data should be in a data.frame format where rows are test years and columns are different models
#This format is the natural output of the regression model function above

Mean_Absolute_Error = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  
  Absolute_Error = array(dim = c(nrow(ModelPredictions),ncol(ModelPredictions)))
  
  
  for(i in 1:nrow(ModelPredictions))
  {
    for(j in 1:ncol(ModelPredictions))
    {
      Absolute_Error[i,j] = abs((ModelPredictions_All_Ages[i,j]-observations[i]))
    }
  }
  
  
  
  return(colMeans(Absolute_Error))
}

