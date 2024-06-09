
one_step_ahead_regression= function(meanLengths,age,day,Total_Run,Total_Run_Previous_Year,pinksalmonPrevious_Year,Odd_Even_Year,PreviousSummerMeanTemp,PreviousWinterMeanTemp,legendLocation = "bottomright",startyear,endyear,start_test_year)
  
{
regressionoutputsBase = list()

regressionoutputs = list()


regressionoutputsPinkSalmon = list() #Binary cyclic pink salmon abundant year or not added as variable (variable indicates odd or even year)

regressionoutputsPinkSalmon_Even_Odd = list()


regressionoutputsPinkSalmonSummerTemp = list() #Binary cyclic pink salmon abundant year or not added as variable 

regressionoutputsPinkSalmonSummerTemp_Even_Odd = list()

regressionoutputsSummerTemp_No_Pinks = list()


regressionoutputsPinkSalmonWinterTemp = list() #Binary cyclic pink salmon abundant year or not added as variable 

regressionoutputsPinkSalmonWinterTemp_Even_Odd = list()

regressionoutputsWinterTemp_No_Pinks = list()



regressionoutputs_covariates_pink_abundance = list()

regressionoutputs_covariates_pink_year = list()

regressionoutputs_Salmon_Previous_Year_Only  = list()

NA_Index = c(rep(1,length(meanLengths[,day-164])))

NA_Index[is.na(meanLengths[,day-164])] = 0 #0 means No data



time=seq(startyear,endyear)[which(NA_Index == 1)]

meanLengths = meanLengths[which(NA_Index == 1),]



Total_Run_Previous_Year = Total_Run_Previous_Year[which(NA_Index == 1)]
pinksalmonPrevious_Year = pinksalmonPrevious_Year[which(NA_Index == 1)]
Odd_Even_Year = Odd_Even_Year[which(NA_Index == 1)]
PreviousSummerMeanTemp = PreviousSummerMeanTemp[which(NA_Index == 1)]
PreviousWinterMeanTemp = PreviousWinterMeanTemp[which(NA_Index == 1)]
Total_Run = Total_Run[which(NA_Index == 1)]


length(time[which(time >= start_test_year)])

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
  
  
  regressionoutputs_covariates_pink_abundance[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+pinksalmonPrevious_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputs_covariates_pink_year[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i]+Odd_Even_Year[1:i]+PreviousSummerMeanTemp[1:i]+PreviousWinterMeanTemp[1:i])
  
  regressionoutputs_Salmon_Previous_Year_Only[[iter]] = lm(Total_Run[1:i]~Total_Run_Previous_Year[1:i])
  
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
  
  Summary_13[[iter]] = summary(regressionoutputs_Salmon_Previous_Year_Only[[iter]])
  ANOVA_13[[iter]] = anova(regressionoutputs_Salmon_Previous_Year_Only[[iter]])
  
  # 
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
                           Summary_13,ANOVA_13)

ModelPredictions = array(dim = c(length(time[which(time >= start_test_year)]),13))



iter = 1



for(i in (nrow(meanLengths)-(length(time[which(time >= start_test_year)]))):(nrow(meanLengths)-1))
{
  
  
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
  c12 = as.vector(regressionoutputs_Salmon_Previous_Year_Only[[iter]][["coefficients"]])
 
  # cat("\n\n######## Model 5 coefficients")
  # print(c4)
  # cat("\n\n######## Model 7 coefficients")
  # print(c6)
  
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
  r12 = as.vector(regressionoutputs_Salmon_Previous_Year_Only[[iter]][["residuals"]])
  
  years = seq(start_test_year, 2023, 1)
  

  
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
  
  p10 = c10[1]+c10[2]*Total_Run_Previous_Year[i+1]+c10[3]*pinksalmonPrevious_Year[i+1]+c10[4]*PreviousSummerMeanTemp[i+1]+c10[5]*PreviousWinterMeanTemp[i+1]
  p11 = c11[1]+c11[2]*Total_Run_Previous_Year[i+1]+c11[3]*Odd_Even_Year[i+1]+c11[4]*PreviousSummerMeanTemp[i+1]+c11[5]*PreviousWinterMeanTemp[i+1]
  p12 = c12[1] + c12[2]*Total_Run_Previous_Year[i+1]
  

  
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
  fit12 = (regressionoutputs_Salmon_Previous_Year_Only[[iter]][["fitted.values"]])

 


  yearsvector = time
  
  
  # print(years[iter])
  # print(iter)
  
  iter = iter+1
  
}

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
retrospectiveandonestepahead13 = c(fit12[1:(nrow(meanLengths)-(length(time[which(time >= start_test_year)])))],ModelPredictions[,13])


#par(mfrow=c(1,2))




retrospectiveandonestepahead = rbind(retrospectiveandonestepahead1,  
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
retrospectiveandonestepahead13

)

row.names(retrospectiveandonestepahead) = c("Model 1", "Model 2","Model 3", "Model 4","Model 5","Model 6", "Model 7", "Model 8","Model 9", "Model 10", "Model 11", "Model 12", "Model 13")

colnames(retrospectiveandonestepahead) = row.names(meanLengths)[which(is.na(meanLengths[,day-164]) == FALSE)]

Predictions=list(retrospective_and_one_step_ahead=retrospectiveandonestepahead,one_step_ahead = ModelPredictions,  ANOVA = ANOVA_Info_All_Years)

return(Predictions)

}



Mean_Absolute_Error = function(ModelPredictions_All_Ages,observations)
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



corelation_function = function(ModelPredictions_All_Ages,observations)
{
  
  ModelPredictions= ModelPredictions_All_Ages
  correlation= c()
  
  #Total_Runstart_test_year_2022 = Total_Runstart_test_year_2022
  for(j in 1:ncol(ModelPredictions))
  {
    correlation[j] = cor(x=ModelPredictions[,j], y=observations)
  }
  
  print(correlation)
  
}



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


Percent_Absolute_Error = function(ModelPredictions_All_Ages,observations)
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
  
  
  
  return((Absolute_Error))
}

