
#Do model without pink salmon as a covariate

#install.packages("readxl")
library("readxl")
library("ggplot2")
library("tidyverse")

data1 = read.csv("data/BB-sockeye-catch.csv")
data2 = read.csv("data/BB-sockeye-escapement.csv")
data3 = read.csv("data/BBasl_to_2020.csv")


Totals = read.csv("data/2021.csv")



estimate_age_returns = read.csv("data/2021_Return_Tables_Bristol_Bay.csv")
estimate_age_returns1980_2021 = estimate_age_returns[which(estimate_age_returns[,"Year"] >= 1980),]

estimate_age_returns1979_2020 = estimate_age_returns[which(estimate_age_returns[,"Year"] >= 1979 & estimate_age_returns[,"Year"] <= 2020),]

estimate_age_returns1980_2021_age2 = estimate_age_returns1980_2021[which(estimate_age_returns1980_2021[,"Salt.Water.Age"] == 2),]
estimate_age_returns1980_2021_age3 = estimate_age_returns1980_2021[which(estimate_age_returns1980_2021[,"Salt.Water.Age"] == 3),]

estimate_age_returns1980_2021_age2_all_rivers = c()
estimate_age_returns1980_2021_age3_all_rivers = c()

years_1980_2021 = seq(1980, 2021, 1)

for (i in 1:length(years_1980_2021)) 
  
{
  
  estimate_age_returns1980_2021_age2_all_rivers[i] = sum(estimate_age_returns1980_2021_age2[which(estimate_age_returns1980_2021_age2[,"Year"] == years_1980_2021[i]),"Abundance"])
  estimate_age_returns1980_2021_age3_all_rivers[i] = sum(estimate_age_returns1980_2021_age3[which(estimate_age_returns1980_2021_age3[,"Year"] == years_1980_2021[i]),"Abundance"])
  
}


estimate_age_returns1979_2020_age2 = estimate_age_returns1979_2020[which(estimate_age_returns1979_2020[,"Salt.Water.Age"] == 2),]
estimate_age_returns1979_2020_age3 = estimate_age_returns1979_2020[which(estimate_age_returns1979_2020[,"Salt.Water.Age"] == 3),]


estimate_age_returns1979_2020_age2_all_rivers = c()
estimate_age_returns1979_2020_age3_all_rivers = c()


years_1979_2020 = seq(1979, 2020, 1)

for (i in 1:length(years_1979_2020)) 
  
{
  
  estimate_age_returns1979_2020_age2_all_rivers[i] = sum(estimate_age_returns1979_2020_age2[which(estimate_age_returns1979_2020_age2[,"Year"] == years_1979_2020[i]),"Abundance"])
  estimate_age_returns1979_2020_age3_all_rivers[i] = sum(estimate_age_returns1979_2020_age3[which(estimate_age_returns1979_2020_age3[,"Year"] == years_1979_2020[i]),"Abundance"])
  
}

datatest = data3[which(data3[,"Year"] == 2010),]
PM2006_2021 = read_excel("data/PMTF_ASL_Data_2006-2021_updated 4-26_V3.xls")


data4 = read.csv("data/pink_chum_salmon_total_abundances.csv")
data5 = data4[20:61,] #truncate so its the starts and ends one year before data1 and 2 (1979 to 2020)
OceanTemps1 = read.csv("data/SST-BristolBay3.csv")
OceanTemps1979to2020 = OceanTemps1[32:73,]#Truncate so it starts and ends one year before data1 and 2 1979 to 2020
OceanTemps1979to2023 = OceanTemps1[32:76,]

pinksalmon = data5[,'pinks']
pinksalmon_1979to2021 = c(pinksalmon,797) # final 2021 pink salmon was 797 million
pinksalmon_1979to2022 = c(pinksalmon_1979to2021, 409) 

seq(from = 1979, to = 2021, by = 1)[16]

pinksalmon_1994to2021 = pinksalmon_1979to2021[16:length(pinksalmon_1979to2021)]

sd(pinksalmon, na.rm = TRUE)

t = c(11,12,13,14,15,16)
t %% 10 

seasondata2021 = read_excel("data/FDMS2021_V3.xls")
seasondata2022 = read_excel("data/FDMS2022.xlsx") #updates
seasondata2023 = read.csv("data/FDMS2023.csv")

seasondata2023$catch_date = format(as.Date(seasondata2023$catch_date, "%m/%d/%Y"), "%Y-%m-%d")

seasondata2023$sample_date = format(as.Date(seasondata2023$sample_date, "%m/%d/%Y"), "%Y-%m-%d")

seasondata2023 <- seasondata2023 %>%
  mutate(Year = rep(2023, nrow(seasondata2023)), .before = district) %>%
  rename(ASLProjectType = project_type,
         Length = length,
         sampleDate = sample_date)

colnames(seasondata2022)
colnames(seasondata2023)

seasondata2021_2023 = rbind(seasondata2021,seasondata2022,seasondata2023)

Salt.Water.Age = seasondata2021_2023[,'age'] %% 10 

seasondata2021_2023[,'Salt.Water.Age'] = Salt.Water.Age

seasondata2021_2023V2 = seasondata2021_2023[which(seasondata2021_2023[,'species_code']==420 ),] #Filter for sockeye


seasondata2021_2023V2 = as.data.frame(seasondata2021_2023V2[,c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")])

seasondata2021_2023V2 = na.omit(seasondata2021_2023V2)

Length2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Length"]))
Salt.Water.Age2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Salt.Water.Age"]))
sampleDate2021_2023 = as.Date(seasondata2021_2023V2$sampleDate)

Year2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Year"]))

ASLProjectType2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"ASLProjectType"]))

WinterMeanTemp1979to2020 = c()
SummerMeanTemp1979to2020 = c()
WinterMeanTemp1979to2023 = c()
SummerMeanTemp1979to2022 = c()



for(i in 2:(nrow(OceanTemps1979to2023)))
{
  WinterMeanTemp1979to2023[i-1] = mean(rowMeans(OceanTemps1979to2023[i,c(2,3)]), OceanTemps1979to2023[i-1,13])
  SummerMeanTemp1979to2022[i-1] = rowMeans(OceanTemps1979to2023[i-1,c(7:9)])
}

plot(SummerMeanTemp1979to2022~seq(1979,2022,1))
plot(WinterMeanTemp1979to2023~seq(1979,2022,1))

acf(SummerMeanTemp1979to2022)
acf(WinterMeanTemp1979to2023)

ProjectType2021_2023 = ASLProjectType2021_2023 

ASLProjectType= c()

for(i in 1:length(ProjectType2021_2023))
{
  if(ProjectType2021_2023[i] == 1)
  {
    ASLProjectType[i] = "commercial catch"
  }
  
  if(ProjectType2021_2023[i] == 3)
  {
    ASLProjectType[i] = "escapement"
  }
  
  
  if(ProjectType2021_2023[i] == 5)
  {
    ASLProjectType[i] = "test fishing"
  }
  
  
  
}

ASLProjectType = as.factor(ASLProjectType)

seasondata2021_2023V2 = cbind.data.frame(Length2021_2023,Salt.Water.Age2021_2023,sampleDate2021_2023,Year2021_2023,ASLProjectType)
colnames(seasondata2021_2023V2) = c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")
seasondata2021_2023V2$sampleDate = as.factor(seasondata2021_2023V2$sampleDate)

data3 = data3[,c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")]
#row.names(data3) = c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")
data3 = rbind(data3,seasondata2021_2023V2)


source('scripts/Byweeklengthatage_Function4.R')
source('scripts/meanrelativeerrorcode.R')
source('scripts/salmon_regression_models_V5_08_03_2023_cyclic_pink_salmon_2.R')

#write.csv(BBasl_to_2020_clean, 'data/BBasl_to_2020.csv')

#Nushagak district doesn't always specify nushagak or igushik river ie lines 188 to 386

#1 = commercial
#3 = escapement
#5 = port moller test fishery


data_escapement = data3[which(data3[,"ASLProjectType"] == 'escapement'),]
data_catch = data3[which(data3[,"ASLProjectType"] == 'commercial catch'),]
data_all = data3 #Can only use at aggregate level since escapement is by river and commercial catch is by district

data_test_fishery = data3[which(data3[,"ASLProjectType"] == 'test fishing'),]

PM2006_2021 = data.frame(PM2006_2021[,c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")])
data_test_fishery_before_2006= data_test_fishery[which(data_test_fishery[,"Year"] < 2006),]

PM2006_2021_Length = PM2006_2021$Length
PM2006_2021_Salt.Water.Age = PM2006_2021$Salt.Water.Age
PM2006_2021_Year = PM2006_2021$Year
PM2006_2021_ASLProjectType = as.factor(PM2006_2021$ASLProjectType)
PM2006_2021_sample_date = as.Date(PM2006_2021$sampleDate)

PM2006_2021 = cbind.data.frame(PM2006_2021_Length,
                               PM2006_2021_Salt.Water.Age,
                               PM2006_2021_sample_date,
                               PM2006_2021_Year,
                               PM2006_2021_ASLProjectType)

colnames(PM2006_2021) = c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")
PM2006_2021$sampleDate = as.factor(PM2006_2021$sampleDate)


data_test_fishery_full=rbind(data_test_fishery_before_2006,PM2006_2021)
data_test_fishery_full = na.omit(data_test_fishery_full)

data_test_fishery_full[which(data_test_fishery_full[,"Year"] == 1999),] #few fish just 1 age 2

#remake data_all with new test fishery data set
data_all=data_all[which(data_all[,"ASLProjectType"] != "test fishing"),] #remove test fishing data
data_all = rbind(data_all,data_test_fishery_full) #add new test fishing data

t = as.Date(data_all[,"sampleDate"])
t2 = format(t,"%j")
days = 170:185
sample_year = c(data_all[,"Year"])
years_unique = unique(sample_year)
sample_size = array(dim = c(length(days),length(years_unique)))


for(j in 1:length(years_unique))
{
for(i in 1:length(days))
  
{
 
  sample_size[i,j] = nrow(data_all[(which(sample_year == years_unique[j] & t2 == days[i])),])
  }
  
  
}

rowMeans(sample_size)

meanlengthatageescapementdata = mean_length_by_week_function2(data_escapement, 1958, endyear = 2023)

meanlengthatagecatchdata = mean_length_by_week_function2(data_catch, 1958, endyear = 2023)

meanlengthatagealldata = mean_length_by_week_function2(data_all, 1958, endyear = 2023) #All data including the test fishery

meanlengthatageportmoller = mean_length_by_week_function2(data_test_fishery_full, 1958, endyear = 2023)


x = data.frame(meanlengthatagealldata[[1]]) #age2
age3 = data.frame(meanlengthatagealldata[[2]])
#x2 = x[,1:14]

x_percent = array(dim = c(63,18))
x_percent = x / x[,18]

age3_percent = array(dim = c(63,18))
age3_percent = age3 / age3[,18]
  
library(tidyverse)
# x_new <- x %>% 
#   mutate(year=rownames(.)) %>% 
#   select(year, everything()) %>% 
#   gather(key="week", value="length_avg", 2:ncol(.)) %>% 
#   mutate(week=gsub("X", "", week) %>% as.numeric(),
#          year=as.numeric(year))
# 
# 
# 
# x_new3 <- x_percent %>% 
#   mutate(year=rownames(.)) %>% 
#   select(year, everything()) %>% 
#   gather(key="week", value="length_avg", 2:ncol(.)) %>% 
#   mutate(week=gsub("X", "", week) %>% as.numeric(),
#          year=as.numeric(year))
# 
# age3percentlong <- age3_percent %>% 
#   mutate(year=rownames(.)) %>% 
#   select(year, everything()) %>% 
#   gather(key="week", value="length_avg", 2:ncol(.)) %>% 
#   mutate(week=gsub("X", "", week) %>% as.numeric(),
#          year=as.numeric(year))

# g <- ggplot(x_new2, aes(x=week, y=length_avg, color=year, group=year)) +
#   geom_line() +
#   labs(x="Week", y="Mean length (mm)") +
#   theme_bw()
# g

# g2 <- ggplot(x_new3, aes(x=week, y=length_avg, color=year, group=year)) +
#   geom_line() +
#   labs(x="Week", y="% of final length") +
#   theme_bw()
# g2
# 
# g3 = ggplot(age3percentlong, aes(x=week, y=length_avg, color=year, group=year)) +
#   geom_line() +
#   labs(x="Week", y="% of final length") +
#   theme_bw()
# g3

#all data
Age2meanLengths_all = meanlengthatagealldata[[1]] #Catch data starts in 1963
Age2meanLengths_all = Age2meanLengths_all[6:nrow(Age2meanLengths_all),] 

Age2meanLengths_all_sample_size = meanlengthatagealldata[[3]]
Age2meanLengths_all_sample_size = Age2meanLengths_all_sample_size[6:nrow(Age2meanLengths_all_sample_size),] 

Age3meanLengths_all = meanlengthatagealldata[[2]]
Age3meanLengths_all = Age3meanLengths_all[6:nrow(Age3meanLengths_all),]

Age3meanLengths_all_sample_size = meanlengthatagealldata[[4]]
Age3meanLengths_all_sample_size = Age3meanLengths_all_sample_size[6:nrow(Age3meanLengths_all_sample_size),] 

Age2meanLengths_all_sample_size_1980_2022 = Age2meanLengths_all_sample_size[18:60,]
colMeans(Age2meanLengths_all_sample_size_1980_2022,na.rm = TRUE)
Age3meanLengths_all_sample_size_1980_2022 = Age3meanLengths_all_sample_size[18:60,]
colMeans(Age3meanLengths_all_sample_size_1980_2022,na.rm = TRUE)

min(Age2meanLengths_all[,1],na.rm = TRUE)
max(Age2meanLengths_all[,1],na.rm = TRUE)

min(Age3meanLengths_all[,1],na.rm = TRUE)
max(Age3meanLengths_all[,1],na.rm = TRUE)



# river 
catchAllRivers = as.vector(rowSums(data1[,2:9]))

#2021 catch was 40,227,985


catchAllRivers = c(catchAllRivers,40227985)

escapementAllRivers = as.vector(rowSums(data2[,2:9]))

#2021 escapement was 25,632,012

escapementAllRivers = c(escapementAllRivers,25632012)

#getting full escapement project type dataframe
# Age2meanLengths_escapement$catch <- catchAllRivers
# Age3meanLengths_escapement$catch <- catchAllRivers
# 
# Age2meanLengths_escapement$escapement <- escapementAllRivers
# Age3meanLengths_escapement$escapement <- escapementAllRivers
# 
# #getting full catch project type dataframe
# 
# Age2meanLengths_catch$catch <- catchAllRivers
# Age3meanLengths_catch$catch <- catchAllRivers
# 
# Age2meanLengths_catch$escapement <- escapementAllRivers
# Age3meanLengths_catch$escapement <- escapementAllRivers

#getting full all project types dataframe

#Age2meanLengths_all$catch <- catchAllRivers
#Age3meanLengths_all$catch <- catchAllRivers

#Age2meanLengths_all$escapement <- escapementAllRivers
#Age3meanLengths_all$escapement <- escapementAllRivers

#For Port Moller

#Age2meanLengths_portmoller$catch <- catchAllRivers[2:59] #pm data goes from 1964 rather than 1963
#Age3meanLengths_portmoller$catch <- catchAllRivers[2:59]

#Age2meanLengths_portmoller$escapement <- escapementAllRivers[2:59]
#Age3meanLengths_portmoller$escapement <- escapementAllRivers[2:59]

years = seq(from = 1963, to = 2021, by = 1)
Total_Run_Original = c()
for(i in 1:length(years))
{
  Total_Run_Original[i] = sum(Totals[which(Totals[,"retYr"] == years[i]),"return"])
}


Total_Run_Original = c(Total_Run_Original,83281.914, 54485.107 ) # add 2022 estimate 80000, add 2023 estimate #2023 Bristol Bay total run 54,485,107
Total_Run = Total_Run_Original
Total_Run_For_Portmoller = Total_Run[2:60]


#Catch
library(ggplot2)

#Just start with catch because catch is done by district but escapement is done by river
#But when aggregating the above doesn't matter

#Aggregating catch, escapement, and test fishery

#Age2

library(tidyr)



#Leave one out
SizeAtAge2 = data_all[which(data_all[,'Salt.Water.Age'] == 2 & data_all[,'Year'] >= 1963 ),]
#No 2006 age 2 length at age
SizeAtAge3 = data_all[which(data_all[,'Salt.Water.Age'] == 3 & data_all[,'Year'] >= 1963),]


#Port Moller


SizeAtAge2portmoller = data_test_fishery_full[which(data_test_fishery_full[,'Salt.Water.Age'] == 2 & data_test_fishery_full[,'Year'] >= 1964 ),]

SizeAtAge3portmoller = data_test_fishery_full[which(data_test_fishery_full[,'Salt.Water.Age'] == 3 & data_test_fishery_full[,'Year'] >= 1964),]

#

years = seq(from = 1980, to = 2023)


MeanSizeAtAge2 = c()


MeanSizeAtAge3 = c() 


MeanSizeAtAge2_June_24 = c()


MeanSizeAtAge3_June_24 = c() 



for(i in 1:length(years))
{
  MeanSizeAtAge2[i] = mean(SizeAtAge2[which(SizeAtAge2[,'Year'] == years[i]),'Length'])

  MeanSizeAtAge3[i] = mean(SizeAtAge3[which(SizeAtAge3[,'Year'] == years[i]),'Length'])

 # MeanSizeAtAge2_June_24[i] = mean(SizeAtAge2[which(SizeAtAge2[,'Year'] == years[i] & format(as.Date(SizeAtAge2[,'sampleDate']), "%j") <= 176 ),'Length'], na.rm = TRUE)


 # MeanSizeAtAge3_June_24[i] = mean(SizeAtAge3[which(SizeAtAge3[,'Year'] == years[i] & format(as.Date(SizeAtAge3[,'sampleDate']), "%j") <= 176 ),'Length'], na.rm = TRUE)

  # row.names(MeanSizeAtAge2) = years
  # row.names(MeanSizeAtAge3) = years


}


yearsvector = c("1963 to 2009",
                "1963 to 2010",
                "1963 to 2011",
                "1963 to 2012",
                "1963 to 2013",
                "1963 to 2014",
                "1963 to 2015",
                "1963 to 2016",
                "1963 to 2017",
                "1963 to 2018",
                "1963 to 2019",
                "1963 to 2020",
                "1963 to 2021",
                "1963 to 2022")


par(mfrow=c(2,2))


years[17]
years[60]

years[32]

#If want to add previous years sockeye abundance as covariate, need to truncate main dataset to 1964 so can use 1963 sockeye abundance

Total_Run_Previous_Year_1979to2021 = Total_Run_Original[17:59]

Total_Run_Previous_Year_1979to2022 = Total_Run_Original[17:60]

Total_Run_Previous_Year_1994to2021 = Total_Run_Original[32:59]


#Truncate Total Run
years[33]
years[48]


Total_Run_1980_2022 = Total_Run_Original[18:60] #1980 to 2022
Total_Run_1995_2022 = Total_Run_Original[33:60] #1995 to 2022

Total_Run_2010_2022 = Total_Run_Original[48:60] #2010 to 2022
Total_Run_2005_2022 = Total_Run_Original[43:60] #2005 to 2022
Total_Run_2000_2022 = Total_Run_Original[38:60] #2000 to 2022


Total_Run_2010_2023 = Total_Run_Original[48:61] #2010 to 2023
Total_Run_2005_2023 = Total_Run_Original[43:61] #2005 to 2023
Total_Run_2000_2023 = Total_Run_Original[38:61] #2000 to 2023
Total_Run_1980_2023 = Total_Run_Original[18:61] #1980 to 2023

#Truncate data frame so its 1980 to 2022

Age2meanLengths_all_1980_2022 = Age2meanLengths_all[18:60,]
Age3meanLengths_all_1980_2022 = Age3meanLengths_all[18:60,]

Age2meanLengths_all_1980_2023 = Age2meanLengths_all[18:61,]
Age3meanLengths_all_1980_2023 = Age3meanLengths_all[18:61,]

Age2meanLengths_all_1995_2022 = Age2meanLengths_all[33:60,]
Age3meanLengths_all_1995_2022 = Age3meanLengths_all[33:60,]

par(mfrow=c(1,2))

Odd_Even_Year = c()
years_1980_2023 = seq(1980,2023,1)

for(i in 1:length(years_1980_2023))
    
{
  years = years_1980_2023[i]/2
  years_rounded = round(years)
  if(years == years_rounded )
   {
     Odd_Even_Year[i] = 1 #1 indicates an even year
  }
  
  else
    
  {
    Odd_Even_Year[i] = 0
  }
      
}


nrow(Age2meanLengths_all_1980_2023)

Age2meanLengths_all_1980_2023[1,]
print((Age2meanLengths_all_1980_2023[30,]))

library(pls)
set.seed(2)

#Scale numeric covariates to mean of 0, standard deviation of 1

row_names = rownames(Age2meanLengths_all_1980_2023)
col_names = colnames(Age2meanLengths_all_1980_2023)
  
Age2meanLengths_all_1980_2023_scaled = t(apply(Age2meanLengths_all_1980_2023,1, scale))
Age3meanLengths_all_1980_2023_scaled = t(apply(Age3meanLengths_all_1980_2023,1, scale))

rownames(Age2meanLengths_all_1980_2023_scaled) = row_names
rownames(Age3meanLengths_all_1980_2023_scaled) = row_names

colnames(Age2meanLengths_all_1980_2023_scaled) = col_names
colnames(Age3meanLengths_all_1980_2023_scaled) = col_names

nrow(Age2meanLengths_all_1980_2023)

#Total_Run_Previous_Year_1979to2022 = log(Total_Run_Previous_Year_1979to2022)

ModelPredictionsAge2 = one_step_ahead_regression(meanLengths= Age2meanLengths_all_1980_2023,age="Age 2", day = 176,  
                                                 Total_Run = Total_Run_1980_2023,
                                                 Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                                 pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                                 Odd_Even_Year = Odd_Even_Year,
                                                 PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                                 PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = 2000)
 
ModelPredictionsAge3 = one_step_ahead_regression(meanLengths=Age3meanLengths_all_1980_2023,age="Age 3", day = 176, 
                                                 Total_Run = Total_Run_1980_2023,
                                                 Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                                 pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                                 Odd_Even_Year = Odd_Even_Year,
                                                 PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                                 PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = 2000)


Age_2_Sum_Sq_DF_Model_7 = array(dim = c(24,3)) #4 variables
Age_2_R2_Model_7 = c()


Age_3_Sum_Sq_DF_Model_5 = array(dim = c(24,4)) #4 variables
Age_3_R2_Model_5 = c()


#Model 7
for(i in 1:24)
{

 Age_2_R2_Model_7[i] = ModelPredictionsAge2[["ANOVA"]][[13]][[i]][["adj.r.squared"]]
 Age_2_Sum_Sq_DF_Model_7[i,] = ModelPredictionsAge2[["ANOVA"]][[14]][[i]][["Sum Sq"]][1:3] #3 variables
 
 Age_3_R2_Model_5[i] = ModelPredictionsAge3[["ANOVA"]][[9]][[i]][["adj.r.squared"]]
 Age_3_Sum_Sq_DF_Model_5[i,] = ModelPredictionsAge3[["ANOVA"]][[10]][[i]][["Sum Sq"]][1:4] #4 variables

}


mean_Age_2_R2_Model_7 = mean(Age_2_R2_Model_7)
mean_Age_3_R2_Model_5 = mean(Age_3_R2_Model_5)

Age2_percent_explained = colMeans(Age_2_Sum_Sq_DF_Model_7/rowSums(Age_2_Sum_Sq_DF_Model_7))
Age3_percent_explained = colMeans(Age_3_Sum_Sq_DF_Model_5/rowSums(Age_3_Sum_Sq_DF_Model_5))

retrospective_and_onestep_ahead_age2= ModelPredictionsAge2[[1]]
retrospective_and_onestep_ahead_age3= ModelPredictionsAge3[[1]]


#source('scripts/meanrelativeerrorcode_model_5.R')


par(mfrow=c(1,1))


years = row.names(Age2meanLengths_all_1980_2023)

regressions2 = array(0, dim = c(length(years),4))  
regressions3 = array(0, dim = c(length(years),4))  

for(i in 1:length(years))
{
  t2 = lm(Total_Run_1980_2023[-i]~MeanSizeAtAge2[-i]+Total_Run_Previous_Year_1979to2022[-i]+SummerMeanTemp1979to2022[-i])
  regressions2[i,] = t2[["coefficients"]] # regression between everything except year i
  
  t3 = lm(Total_Run_1980_2023[-i]~MeanSizeAtAge3[-i]+Total_Run_Previous_Year_1979to2022[-i]+SummerMeanTemp1979to2022[-i])
  regressions3[i,] = t3[["coefficients"]] # regression between everything except year i
  
}

row.names(regressions2) = years
row.names(regressions3) = years

errors2 = data.frame(array(0, dim = c(length(years),90)))
errors3 = data.frame(array(0, dim = c(length(years),90)))

row.names(errors2) = years
names(errors2)<- paste0("day",seq(90))

row.names(errors3) = years
names(errors3)<- paste0("day",seq(90))



for(i in 1:length(years))  
{
  for(j in 1:90)
  {
    estimated_run2 = regressions2[i,1] + regressions2[i,2]*Age2meanLengths_all_1980_2023[i,j] + regressions2[i,3]*Total_Run_Previous_Year_1979to2022[i] + regressions2[i, 4]*SummerMeanTemp1979to2022[i]
    errors2[i,j] = abs(Total_Run[i]-estimated_run2) #prediction error
    #
    estimated_run3 = regressions3[i,1] + regressions3[i,2]*Age3meanLengths_all_1980_2023[i,j] + regressions3[i,3]*Total_Run_Previous_Year_1979to2022[i] + regressions3[i, 4]*SummerMeanTemp1979to2022[i]
    errors3[i,j] = abs(Total_Run[i]-estimated_run3)

    # #errors[i,j]<-abs((Total_Run[i]-estimated_run)/Total_Run[i]) #mean relative error
  }
}

# t = lm(Total_Run_1980_2022~MeanSizeAtAge2+Total_Run_Previous_Year_1979to2022[1:43]+SummerMeanTemp1979to2022[1:43])
# 
# regressions[i,] = t[["coefficients"]] # regression between everything except year i


mean_err_by_week_age2<-apply(errors2,2,function(x) mean(x,na.rm=T))
mean_err_by_week_age3<-apply(errors3,2,function(x) mean(x,na.rm=T))


dates = seq(as.Date(format = "%m/%d", "06/13"), by = "day", length.out = 90)
dates_extended = c(dates,dates)


prediction_over_time_df = data.frame(c(mean_err_by_week_age2,mean_err_by_week_age3))
prediction_over_time_df = cbind(prediction_over_time_df,c(rep("Age 2",90), rep("Age 3", 90)) )
prediction_over_time_df[,"Dates"] = dates_extended


colnames(prediction_over_time_df) = c("Error", "Age","Date")
prediction_over_time_df2 = prediction_over_time_df[which(prediction_over_time_df[,"Date"] <= "2024-08-01"),]

ggplot(data = prediction_over_time_df2, aes(x = Date, y = Error)) +
  geom_point()+
  labs(x="Date", y="Error (in thousands)") +
  facet_wrap(~Age, scales = "free", ncol=2)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15),
        panel.spacing = unit(2, "lines"))

mean_abs_percent_error_age_2 = array(dim = c(nrow = nrow(Age2meanLengths_all_1980_2023), ncol = ncol(Age2meanLengths_all_1980_2023)))
rownames(mean_abs_percent_error_age_2) = rownames(Age2meanLengths_all_1980_2023)
colnames(mean_abs_percent_error_age_2) = colnames(Age2meanLengths_all_1980_2023)

mean_abs_percent_error_age_3 = array(dim = c(nrow = nrow(Age3meanLengths_all_1980_2023), ncol = ncol(Age3meanLengths_all_1980_2023)))
rownames(mean_abs_percent_error_age_3) = rownames(Age3meanLengths_all_1980_2023)
colnames(mean_abs_percent_error_age_3) = colnames(Age3meanLengths_all_1980_2023)


for(i in 1:nrow(Age2meanLengths_all_1980_2023))
    {
      
      for(j in 1:ncol(Age2meanLengths_all_1980_2023))
        
      {
        
        mean_abs_percent_error_age_2[i,j] =  100*abs((MeanSizeAtAge2[i] - Age2meanLengths_all_1980_2023[i,j])/MeanSizeAtAge2[i] )
        mean_abs_percent_error_age_3[i,j] =  100*abs((MeanSizeAtAge3[i] - Age3meanLengths_all_1980_2023[i,j])/MeanSizeAtAge3[i] )
        
      }
     }


average_error_by_day_age2 = colMeans(mean_abs_percent_error_age_2, na.rm = TRUE)
average_error_by_day_age3 = colMeans(mean_abs_percent_error_age_3, na.rm = TRUE)


dates = seq(as.Date(format = "%m/%d", "06/13"), by = "day", length.out = 90)
dates_extended = c(dates,dates)

average_error_df = data.frame(cbind(c(rep("Ocean age-2", ncol(Age2meanLengths_all_1980_2023)), rep("Ocean age-3", ncol(Age3meanLengths_all_1980_2023)))))

#Age 2 is A Age 3 is B

average_error_df[,"Error"] = as.numeric(c(average_error_by_day_age2,average_error_by_day_age3))
average_error_df[,"Date"] = dates_extended

colnames(average_error_df) = c("Age", "Error", "Date")


Mean_Percent_Difference_Length_Plot <- ggplot(data = average_error_df, aes(x = Date, y = Error)) +
  geom_line()+
  labs(x="Date", y="Mean Absolute % Difference") +
  facet_wrap(~Age, scales = "free", ncol=1)+
  theme_classic()+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%m/%d")+
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15, hjust = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))


Mean_Percent_Difference_Length_Plot



Mean_Percent_Difference_Length_Plot_age_2 <- ggplot(data = average_error_df[which(average_error_df[,"Age"] == "Ocean age-2"),], aes(x = Date, y = Error)) +
  geom_line()+
  labs(x="Date", y="Mean Absolute % Difference") +
  theme_classic()+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%m/%d")+
  theme(
        plot.margin = margin(.5,.5,.5,.5, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15, hjust = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))

Mean_Percent_Difference_Length_Plot_age_3 <- ggplot(data = average_error_df[which(average_error_df[,"Age"] == "Ocean age-3"),], aes(x = Date, y = Error)) +
  geom_line()+
  labs(x="Date", y="") +
  theme_classic()+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%m/%d")+
  theme(plot.margin = margin(.5,.5,.5,.5, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15, hjust = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))


# 
# 
# cor1 = cor(y = Total_Run_1980_2022, x = MeanSizeAtAge2_June_24)
# cor2 = cor(y = Total_Run_1980_2022, x = MeanSizeAtAge3_June_24)
# cor3 = cor(y = Total_Run_1980_2022, x = MeanSizeAtAge2)
# cor4 = cor(y = Total_Run_1980_2022, x = MeanSizeAtAge3)

Mean_Length = c(MeanSizeAtAge2_June_24, MeanSizeAtAge3_June_24, MeanSizeAtAge2, MeanSizeAtAge3)
Run = c(Total_Run_1980_2023,Total_Run_1980_2023,Total_Run_1980_2023,Total_Run_1980_2023)
Time = c(rep("A",length(Total_Run_1980_2023)), rep("A", length(Total_Run_1980_2023)),rep("B",length(Total_Run_1980_2023)), rep("B", length(Total_Run_1980_2023)))
Age = c(rep("Age 2",length(Total_Run_1980_2023)), rep("Age 3",length(Total_Run_1980_2023)), rep("Age 2",length(Total_Run_1980_2023)), rep("Age 3",length(Total_Run_1980_2023)))

#A is age june 24, B is end of season


# Age_vs_Run_df2 = data.frame(cbind(Mean_Length, Run))
# Age_vs_Run_df2[,"Time"] = Time
# Age_vs_Run_df2[,"Run"] = Age_vs_Run_df2[,"Run"]/1000
# Age_vs_Run_df2[,"Age"] = Age 
# 
#  ggplot(data = Age_vs_Run_df2, aes(x = Mean_Length, y = Run, color = Age)) +
#   geom_point()+
#   labs(x="Body Length (mm)", y="Run Size (in millions)") +
#   facet_wrap(~Time, scales = "free", ncol=1)+
#   scale_y_continuous(name = "Observed Run (in millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
#   scale_x_continuous(name = "Body Length (mm)", breaks = seq(470,600,20), limits = c(470, 600))+

Mean_Length = c(MeanSizeAtAge2, MeanSizeAtAge3)
Run = c(Total_Run_1980_2023,Total_Run_1980_2023)
Age = c(rep("Ocean age-2",length(Total_Run_1980_2023)), rep("Ocean age-3", length(Total_Run_1980_2023)))



Age_vs_Run_df2 = data.frame(cbind(Mean_Length, Run))
Age_vs_Run_df2[,"Age"] = Age
Age_vs_Run_df2[,"Run"] = Age_vs_Run_df2[,"Run"]/1000

 Length_vs_Run_Plot = ggplot(data = Age_vs_Run_df2, aes(x = Mean_Length, y = Run)) +
  geom_point()+
  labs(x="Body Length (mm)", y="Run Size (in millions)") +
  facet_wrap(~Age, scales = "free", ncol=1)+
  scale_y_continuous(name = "Observed Run (in millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  scale_x_continuous(name = "Body Length (mm)", breaks = seq(470,600,20), limits = c(470, 600))+


  theme_classic()+
 
   theme(
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15, hjust = 0),
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))

 Length_vs_Run_Plot_age_2 = ggplot(data = Age_vs_Run_df2[which(Age_vs_Run_df2[,"Age"] == "Ocean age-2"),], aes(x = Mean_Length, y = Run)) +
   geom_point()+
   labs(x="Body Length (mm)", y="Run Size (in millions)") +

   scale_y_continuous(name = "Observed Run (in millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
   #scale_x_continuous(name = "Body Length (mm)", breaks = seq(470,600,20), limits = c(470, 600))+
   
   
   theme_classic()+
   
   theme(plot.margin = margin(.5,.5,.5,.3, unit = "cm"),
     axis.text.x = element_text(size = 12),
     axis.title.x = element_text(size = 13),
     axis.text.y = element_text(size = 12),
     axis.title.y = element_text(size = 13),
     strip.text.x = element_text(size = 15, hjust = 0),
    
     strip.background = element_rect(color="white", fill="white", linetype="solid"))
 
 Length_vs_Run_Plot_age_3 = ggplot(data = Age_vs_Run_df2[which(Age_vs_Run_df2[,"Age"] == "Ocean age-3"),], aes(x = Mean_Length, y = Run)) +
   geom_point()+
   labs(x="Body Length (mm)", y="") +
   
   scale_y_continuous(name = "", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
   #scale_x_continuous(name = "Body Length (mm)", breaks = seq(470,600,20), limits = c(470, 600))+
   
   
   theme_classic()+
   
   theme(plot.margin = margin(.5,.5,.5,.5, unit = "cm"),
     axis.text.x = element_text(size = 12),
     axis.title.x = element_text(size = 13),
     axis.text.y = element_text(size = 12),
     axis.title.y = element_text(size = 13),
     strip.text.x = element_text(size = 15, hjust = 0),
     
     strip.background = element_rect(color="white", fill="white", linetype="solid"))
 
 
 

par(mfrow=c(1,1))

#meanrelativeerror_model_5


# 
# colnames(retrospective_and_onestep_ahead_age2) = row.names(Age2meanLengths_all_1980_2022)[-which()]
# colnames(retrospective_and_onestep_ahead_age3) = seq(from = 1980, to = 2022)
# 

length(seq(from = 1995, to = 2022))
length(seq(from = 1980, to = 1994))

length(seq(from = 2010, to = 2022))
length(seq(from = 1980, to = 2009))

fit_age_2 = c()
fit_age_3 = c()
years_for_df = c()
Observed_Run_Vector = c()
for(i in 1:(ncol(retrospective_and_onestep_ahead_age2)))
{
  fit_age_2 = c(fit_age_2,retrospective_and_onestep_ahead_age2[,i])
  fit_age_3 = c(fit_age_3,retrospective_and_onestep_ahead_age3[,i])
  
  years_for_df = c(years_for_df,rep(colnames(retrospective_and_onestep_ahead_age2)[i], 13))
  Observed_Run_Vector = c(Observed_Run_Vector,rep(Total_Run_1980_2023[i], 13))
  }

fit_age_2 = as.numeric(fit_age_2)
fit_age_3 = as.numeric(fit_age_3)

years_for_df = as.numeric(years_for_df)

age2_vector = as.factor(rep("Ocean age-2",ncol(retrospective_and_onestep_ahead_age2)))
age3_vector = as.factor(rep("Ocean age-3",ncol(retrospective_and_onestep_ahead_age2)))
#Age 2 is A, Age 3 is B
Type = as.factor(c((rep("Retrospective",(ncol(retrospective_and_onestep_ahead_age2)-24)*13)),(rep("Prospective",24*13))))

Model_Vector = as.factor(rep(c("Model 1","Model 2","Model 3","Model 4","Model 5","Model 6","Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13"), ncol(retrospective_and_onestep_ahead_age2)))

retrospective_and_onestep_ahead_age2_long = cbind.data.frame(Model_Vector, years_for_df, fit_age_2,age2_vector,Type,Observed_Run_Vector)
retrospective_and_onestep_ahead_age3_long = cbind.data.frame(Model_Vector, years_for_df, fit_age_3,age3_vector, Type,Observed_Run_Vector)

colnames(retrospective_and_onestep_ahead_age2_long) = c("Model","Year","Fit","Age","Type","Observed_Run")
colnames(retrospective_and_onestep_ahead_age3_long) = c("Model","Year","Fit","Age","Type","Observed_Run")

full_onestep_ahead_and_prospective_df = rbind(retrospective_and_onestep_ahead_age2_long,retrospective_and_onestep_ahead_age3_long)

full_onestep_ahead_and_prospective_df_model_3 = full_onestep_ahead_and_prospective_df[which(full_onestep_ahead_and_prospective_df[,"Model"] == "Model 3"),]
full_onestep_ahead_and_prospective_df_model_7 = full_onestep_ahead_and_prospective_df[which(full_onestep_ahead_and_prospective_df[,"Model"] == "Model 7"),]
full_onestep_ahead_and_prospective_df_model_5 = full_onestep_ahead_and_prospective_df[which(full_onestep_ahead_and_prospective_df[,"Model"] == "Model 5"),]




#Model 7 = best on average, model 2 = best in 2022
full_onestep_ahead_and_prospective_df_Models_7_2 = full_onestep_ahead_and_prospective_df[which(full_onestep_ahead_and_prospective_df[,"Model"] == "Model 7" | full_onestep_ahead_and_prospective_df[,"Model"] == "Model 2"),]

full_onestep_ahead_and_prospective_df_Models_7_2[,"Fit"] = full_onestep_ahead_and_prospective_df_Models_7_2[,"Fit"]/1000
full_onestep_ahead_and_prospective_df_Models_7_2[,"Observed_Run"] = full_onestep_ahead_and_prospective_df_Models_7_2[,"Observed_Run"]/1000

full_onestep_ahead_and_prospective_df_Models_5_2 = full_onestep_ahead_and_prospective_df[which(full_onestep_ahead_and_prospective_df[,"Model"] == "Model 5" | full_onestep_ahead_and_prospective_df[,"Model"] == "Model 2"),]

full_onestep_ahead_and_prospective_df_Models_5_2[,"Fit"] = full_onestep_ahead_and_prospective_df_Models_5_2[,"Fit"]/1000
full_onestep_ahead_and_prospective_df_Models_5_2[,"Observed_Run"] = full_onestep_ahead_and_prospective_df_Models_5_2[,"Observed_Run"]/1000


full_onestep_ahead_and_prospective_df_Models_7_2_age_2 = full_onestep_ahead_and_prospective_df_Models_7_2[which(full_onestep_ahead_and_prospective_df_Models_7_2[,"Age"] == "Ocean age-2"),]
full_onestep_ahead_and_prospective_df_Models_5_2_age_3 = full_onestep_ahead_and_prospective_df_Models_5_2[which(full_onestep_ahead_and_prospective_df_Models_7_2[,"Age"] == "Ocean age-3"),]
  
Forecasting_Plot_age_2 = ggplot(data = full_onestep_ahead_and_prospective_df_Models_7_2_age_2, aes(x = Year, y = Fit, color = Model)) +
  geom_line() +
  geom_point(data = full_onestep_ahead_and_prospective_df_Models_7_2_age_2, aes(x = Year, y = Observed_Run), color = "black")+
  labs(x="Year", y="Observed (in millions)") +
  geom_vline(xintercept = 2000, color = "black")+
  scale_color_manual(values = c("black","blue"))+
  #scale_linetype_manual(values=c("solid","twodash", "dotted"))+
  scale_y_continuous(name = "Observed Run (millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  theme_classic()+
  
  theme(plot.margin = margin(0,.5,.25,1, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 13, hjust = 0),
        
        legend.direction="vertical",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        
        legend.position = "none", #Model 2 is red, Model 7 is blue
        
        strip.background = element_rect(color="white", fill="white", linetype="solid"))

Forecasting_Plot_age_3 = ggplot(data = full_onestep_ahead_and_prospective_df_Models_5_2_age_3, aes(x = Year, y = Fit, color = Model)) +
  geom_line() +
  geom_point(data = full_onestep_ahead_and_prospective_df_Models_5_2_age_3, aes(x = Year, y = Observed_Run), color = "black")+
  labs(x="Year", y="") +
  geom_vline(xintercept = 2000, color = "black")+
  #scale_linetype_manual(values=c("solid","twodash", "dotted"))+
  scale_color_manual(values = c("black","red"))+
  scale_y_continuous(name = "", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  theme_classic()+
  
  theme(plot.margin = margin(0,.5,.25,1, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 13, hjust = 0),
       
        legend.direction="vertical",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        #legend.position= c(1.1, 0.5),
        legend.position = "none",
        
        strip.background = element_rect(color="white", fill="white", linetype="solid"))

  
one_step_ahead_ModelPredictions_Age2 = ModelPredictionsAge2[[2]]
one_step_ahead_ModelPredictions_Age3 = ModelPredictionsAge3[[2]]



one_step_ahead_ModelPredictions_All_Ages = cbind(one_step_ahead_ModelPredictions_Age2,
                                                 one_step_ahead_ModelPredictions_Age3)

colnames(one_step_ahead_ModelPredictions_All_Ages) = c("Model 1 Age 2", "Model 2 Age 2","Model 3 Age 2", "Model 4 Age 2","Model 5 Age 2","Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13",
                                             "Model 1 Age 3", "Model 2 Age 3","Model 3 Age 3", "Model 4 Age 3","Model 5 Age 3","Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13")



model_predictions_No_Duplicates = one_step_ahead_ModelPredictions_All_Ages[,1:23] #drop last 3 model with just environmental covariates

#Standard error is standard deviation /sqrt(n)

#The below commented out section seems irrelevant/wrong (maybe it was an old test section?)
# df = model_predictions_No_Duplicates[1:10,] #extract 2014 (10)
# se = SD_Error(df, Total_Run_2005_2022[1:10])/sqrt(length(Total_Run_2005_2022[1:10]))
# mape = c()
#   
#   for(i in 1:ncol(df))
#   {
#   mape[i] = mean(abs(df[,i]-Total_Run_2005_2022[1:10])/Total_Run_2005_2022[1:10])
#   }
# 
# mape #Manually  counting, top models are... 7,1,2,13,5
# #manually get variances of top 5
# top_se = se[c(7,1,2,13,5)]
# inv_var = 1/(top_se^2)
# wt = inv_var/(sum(inv_var))
# sum(wt)
# 
# wt_prediction = model_predictions_No_Duplicates[11,c(c(7,1,2,13,5))]*wt #prediction year 2015 (11), extract top 5 models
# sum(wt_prediction)

#preseason_forecasts = c(39.8,35.8,31.7,28.2,29.9,49.5,51.0,43.1,47.6,44.6,49.8,50.9,71.9) #FRI 2010-2022
preseason = read.csv("data/Preseason.csv")
preseason_FRI = preseason[which(preseason[,"agency"] == 1),]
FRI_Preseason_Total_Forecast_1993_2023 = c()

for(i in 1:length(unique(preseason_FRI[,"retYr"])))
  
{
  y = which(preseason_FRI[,"retYr"] == unique(preseason_FRI[,"retYr"])[i])
  FRI_Preseason_Total_Forecast_1993_2023[i] = sum(preseason_FRI[y,"ret"])
}

FRI_Preseason_Total_Forecast_2000_2023 = FRI_Preseason_Total_Forecast_1993_2023[8:31]
FRI_Preseason_Total_Forecast_2005_2023 = FRI_Preseason_Total_Forecast_1993_2023[13:31]
FRI_Preseason_Total_Forecast_2010_2023 = FRI_Preseason_Total_Forecast_1993_2023[18:31]
unique(preseason_FRI[,"retYr"])[13]

#Weighted model

model_standard_errors = list()
model_standard_errors_with_preseason = list()
step = 0
model_MAPEs = list()
model_standard_errors_Top_Four = list()
ordered_MAPES = list()
n_models = 5

model_predictions_No_Duplicates_with_preseason = data.frame(model_predictions_No_Duplicates)
model_predictions_No_Duplicates_with_preseason[,"Pre-season"] = FRI_Preseason_Total_Forecast_2000_2023



#Step 1. Fit Inseason models to data through 1999 
#Step 2. Use within sample predictions to calculate variance for each of the inseason models up to 1999.
#Step 3. Record variance for each inseason model.
#Step 4. Calculate inverse variance weighted inseason ensemble across top 5 inseason models
#Step 5. Determine variance in predictions from weighted inseason ensemble 1993-1999
#Step 6. Calculate 1993-1999 variance for preseason forecast
#Step 7. Weight preseason and inseason ensemble predictions to generate forecast for 2000, based on their variances 1993-1999
#Step 8. Repeat through 2023


test_start_years = seq(2000,2023,1)
n_models = 5
ordered_var = list()
inverse_var = list()
model_weights = list()
weighted_predictions = list()
variance_from_ensemble_within_sample = list()
variance_From_preaseason = list()
inverse_variance_from_inseason_ensemble_and_preseason = list()
final_model_weights = list()
weighted_out_of_sample_prediction = c()
preseason_years = seq(1993,1999,1)
#ModelPredictionsAge2

step = 0

for(i in 1:length(test_start_years))
  
{  
  
  step = step+1
  
Age2 = one_step_ahead_regression(meanLengths= Age2meanLengths_all_1980_2023,age="Age 2", day = 176,  
                                                 Total_Run = Total_Run_1980_2023,
                                                 Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                                 pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                                 Odd_Even_Year = Odd_Even_Year,
                                                 PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                                 PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = test_start_years[i])


Age3 = one_step_ahead_regression(meanLengths= Age3meanLengths_all_1980_2023,age="Age 2", day = 176,  
                                                 Total_Run = Total_Run_1980_2023,
                                                 Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                                 pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                                 Odd_Even_Year = Odd_Even_Year,
                                                 PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                                 PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = test_start_years[i])

retro_and_one_step = rbind(Age2[[1]][1:10,],Age3[[1]]) #Column names are years, remove duplicate models (non length at age models are 11:13)
retro_years = seq(1980,(test_start_years[i]-1),1)
onestep = t(retro_and_one_step[,length(seq(1980,test_start_years[i],1))])
retro = t(retro_and_one_step[,as.character(retro_years)]) #We need models as columns for the function to work properly

#Calculate variance from within sample predictions from observed
#length(seq(1980,1999,1))
obs = Total_Run_1980_2023[1:(length(retro_years))]
within_sample_predictions = retro 

SD = SD_Error(within_sample_predictions, observations=obs)/sqrt(length(obs))
Var = SD^2

#This uses 1980 to t-1 to obtain top 5 models
ordered_var[[step]] = order(Var, decreasing = FALSE)[1:n_models] #Top 5 models by variance

#inverse variance for the top 5 inseason models
inverse_var[[step]] = 1/Var[ordered_var[[step]]] 
model_weights[[step]] = inverse_var[[step]] /sum(inverse_var[[step]])
#Use these weights to obtain a retrospective weighted in-sample prediction

#Generate weighted retrospective prediction starting in 1993 to year t-1
original_predictions = retro[14:length(retro_years),ordered_var[[step]]]
weighted_predictions[[step]] = data.frame(matrix(rowSums(model_weights[[step]]*original_predictions), ncol = 1))

variance_from_ensemble_within_sample[[step]] = SD_Error(weighted_predictions[[step]], observations=obs[14:length(retro_years)])/sqrt(length(obs[14:length(retro_years)]))                            

length(seq(1993, (test_start_years[i]-1),1))

variance_From_preaseason[[step]] = SD_Error(data.frame(as.matrix(FRI_Preseason_Total_Forecast_1993_2023[1:(length(seq(1993, (test_start_years[i]-1),1)))], ncol = 1)), observations = obs[14:length(retro_years)])/sqrt(length(obs[14:length(retro_years)]))

inverse_variance_from_inseason_ensemble_and_preseason[[step]]= 1/c(variance_from_ensemble_within_sample[[step]], variance_From_preaseason[[step]])

#This calculates weights of weighted inseason models and preseason forecast

final_model_weights[[step]] = inverse_variance_from_inseason_ensemble_and_preseason[[step]]/sum(inverse_variance_from_inseason_ensemble_and_preseason[[step]])
#Use the final model weights in year t+1 to generate the out of sample (one step ahead) prediction

#So, we need to extract the correct inseason models based on weights, calculate the inseason forecast based on weights, and multiply them and the preseason by the above weights

one_step_ahead = unlist(onestep)
top_5_one_step = sum(one_step_ahead[ordered_var[[step]]]*model_weights[[step]]) #multiply top 5 by weights and sum to generate weighted inseason prediction
#Extract top 5 by variance
one_step_ahead_top_5_and_preseason = c(top_5_one_step, FRI_Preseason_Total_Forecast_1993_2023[length(seq(1993, (test_start_years[i]),1))])

weighted_out_of_sample_prediction[step] = sum(one_step_ahead_top_5_and_preseason*final_model_weights[[step]])

}


Weighted_Prediction_Each_Year_2000_2023 = weighted_out_of_sample_prediction 

predictions_2 = cbind(FRI_Preseason_Total_Forecast_2000_2023, Weighted_Prediction_Each_Year_2000_2023)
colnames(predictions_2) = c("Preseason", "Weighted") 

overperformbetter = which(Weighted_Prediction_Each_Year_2000_2023 >= Total_Run_2000_2023 & Weighted_Prediction_Each_Year_2000_2023 <= FRI_Preseason_Total_Forecast_2000_2023)
underperformbetter = which(Weighted_Prediction_Each_Year_2000_2023 <= Total_Run_2000_2023 & Weighted_Prediction_Each_Year_2000_2023 >= FRI_Preseason_Total_Forecast_2000_2023)

length(overperformbetter)+length(underperformbetter)

Weighted_Prediction_Each_Year_df = data.frame(cbind(rep("Prospective Weighted", length(Weighted_Prediction_Each_Year_2000_2023)), seq(2000,2023,1)))
Weighted_Prediction_Each_Year_df[,"Fit"] = Weighted_Prediction_Each_Year_2000_2023/1000
Weighted_Prediction_Each_Year_df[,"Age"] = rep("Weighted Average",length(Weighted_Prediction_Each_Year_2000_2023)) #C is weighted average
Weighted_Prediction_Each_Year_df[,"Type"] = rep("Prospective",length(Weighted_Prediction_Each_Year_2000_2023))
Weighted_Prediction_Each_Year_df[,"Observed_Run"] = Total_Run_2000_2023/1000

cor(y = Weighted_Prediction_Each_Year_df[,"Observed_Run"], x = Weighted_Prediction_Each_Year_df[,"Fit"])

MAPE_Weighted = mean(abs(1-(Weighted_Prediction_Each_Year_df[,"Fit"]/Weighted_Prediction_Each_Year_df[,"Observed_Run"])))

colnames(Weighted_Prediction_Each_Year_df) = c("Model","Year","Fit","Age","Type","Observed_Run")

retrospective_1980_1999 = data.frame(matrix(unlist(weighted_predictions[[1]])/1000, ncol = 1))
colnames(retrospective_1980_1999) = "Fit"
retrospective_1980_1999 <- retrospective_1980_1999  %>%
  mutate(Model = rep("Weighted",length(unlist(weighted_predictions[[1]]))), .before = "Fit" ) %>%
  mutate(Year = seq(1993,1999,1), .after = "Model") %>%
  mutate(Age = rep("Weighted",length(unlist(weighted_predictions[[1]]))), .after = "Fit") %>%
  mutate(Type = rep("Retrospective",length(unlist(weighted_predictions[[1]]))), .after = "Type") %>%
  mutate(Observed_Run = Total_Run_1980_2023[14:20], .after = "Type") 

Weighted_Prediction_Each_Year_df = rbind(Weighted_Prediction_Each_Year_df,retrospective_1980_1999)

Forecasting_Plot_Weighted = ggplot(data = Weighted_Prediction_Each_Year_df) +
  geom_line(aes(x = as.numeric(Year), y = Fit, linetype = Type)) +
  geom_point(data = full_onestep_ahead_and_prospective_df_Models_5_2_age_3, aes(x = as.numeric(Year), y = Observed_Run), color = "black")+
  labs(x="Year", y="Run Size") +
  geom_vline(xintercept = 2000, color = "red")+
  scale_linetype_manual(values=c("solid","dotted"))+
  scale_y_continuous(name = "", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  theme_classic()+
  
  theme(plot.margin = margin(0,.5,.25,1, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 13, hjust = 0),
        
        legend.direction="vertical",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        #legend.position= c(1.1, 0.5),
        legend.position = "none",
        
        strip.background = element_rect(color="white", fill="white", linetype="solid"))


Forecasting_Plot_Weighted 

Obs_vs_Pred_Plot_Weighted <- ggplot(data = Weighted_Prediction_Each_Year_df, 
                                    aes(x = Fit, y = Observed_Run)) +
  geom_point() +
  labs(x= "Predicted (in millions)", y="Observed (in millions)") +
  geom_abline(intercept = 0, slope = 1, color = "black")+
  theme_classic()+
  theme(plot.margin = margin(1,1,0,1, unit = "cm"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 13, hjust = 0),
        
        strip.background = element_rect(color="white", fill="white", linetype="solid"))+
  scale_y_continuous(name = "Observed (in millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  scale_x_continuous(name = "Predicted Run (millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))

Mean_error_weighted = Mean_Absolute_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
Max_error_weighted = Maximum_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
sd_error_weighted = SD_Relative_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
correlation_weighted = corelation_function(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)                                      
Percent_error_weighted = Percent_Absolute_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)

Mean_Absolute_Error(data.frame(FRI_Preseason_Total_Forecast_2000_2023), observations=Total_Run_2000_2023)

Maximum_Error(data.frame(FRI_Preseason_Total_Forecast_2000_2023), observations=Total_Run_2000_2023)

                   
(60594.61-54485.11)/54485.11
  
Percent_error = Percent_Absolute_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)
Percent_Error_Model_7_age_2 = Percent_error[,7]
Percent_Error_Model_5_age_3 = Percent_error[,18]

Mean_error = Mean_Absolute_Error(one_step_ahead_ModelPredictions_All_Ages[,],observations=Total_Run_2000_2023)

Max_error = Maximum_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)

#Max errors all occur in 2013
#Mean_error = c(Mean_error,NA)


sd_error = SD_Relative_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)


correlations = corelation_function(one_step_ahead_ModelPredictions_All_Ages, observations=Total_Run_2000_2023) 

Results_Table = t(rbind(c(Mean_error, Mean_error_weighted), c(Max_error, Max_error_weighted), c(sd_error, sd_error_weighted), c(correlations, correlation_weighted)))

Age = c(rep("2",10),"NA","NA","NA", rep("3",10),"NA","NA","NA","All")
Model = (c(c(1:13),c(1:13),"Weighted"))

Updated_Results_Table = (cbind(Results_Table,Age,Model))

Updated_Results_Table = as.matrix(Updated_Results_Table)

colnames(Updated_Results_Table) = c("Mean Error","Max Error", "SD Error", "Correlation", "Age", "Model")

write.csv(Updated_Results_Table,"data/results_table_06_24.csv" )


Preaseason_vs_Models_df = data.frame(c(Total_Run_2000_2023/1000, FRI_Preseason_Total_Forecast_2000_2023/1000, Weighted_Prediction_Each_Year_2000_2023/1000 ))
Preaseason_vs_Models_df[,"Category"] = c(rep("Observed", length(Total_Run_2000_2023)), rep("Pre-season", length(Total_Run_2000_2023)), rep("Weighted", length(Total_Run_2000_2023)))
Preaseason_vs_Models_df[,"Year"] = as.factor(rep(seq(2000,2023,1),3))
colnames(Preaseason_vs_Models_df) = c("Count","Category","Year")
Preaseason_vs_Models_df_Observed = Preaseason_vs_Models_df[which(Preaseason_vs_Models_df[,"Category"] == "Observed"),]
Preaseason_vs_Models_df_2 = Preaseason_vs_Models_df[which(Preaseason_vs_Models_df[,"Category"] != "Observed"),]


Preaseason_vs_Models_df_2$Category = as.factor(Preaseason_vs_Models_df_2$Category)
Preaseason_vs_Models_df_2$Category = ordered(Preaseason_vs_Models_df_2$Category, levels = c("Weighted", "Pre-season") )


Inseason_vs_Preseason_Plot = ggplot()+
  geom_col(data = Preaseason_vs_Models_df_Observed, aes(x = Year, y = Count), 
           col = "black", fill = "lightgrey", width = 0.8) + 
  geom_point(data = Preaseason_vs_Models_df_2, aes(x = Year, y = Count, shape = Category, color = Category), size = 3)+
  scale_shape_manual(values = c(19, 19))+
  scale_color_manual(values = c("blue","red"))+
  labs(x = "Year", y = "Observed Run (millions)", title ="" )+
  theme_classic()+
  scale_y_continuous(name = "Observed Run (millions)", breaks = c(5, 15, 25, 35, 45, 55, 65, 75, 85), limits = c(0, 85))+
  scale_x_discrete(name = "Year", breaks = seq(2000,2023,2))+ 
  theme(legend.margin = margin(0,0,0,0, unit = "cm"),
        plot.margin = margin(0,0,0,1, unit = "cm"), legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
  )


MAPE_preseason = mean(abs(1-(FRI_Preseason_Total_Forecast_2000_2023/(Total_Run_2000_2023))))


library(ggpubr)
library(tidyverse)

Preseason_only_df <- Preaseason_vs_Models_df_2 %>%
  filter(Category == "Pre-season") %>%
  rename("Pre-season" = Count) 

Inseason_only_df <- Preaseason_vs_Models_df_2 %>%
  filter(Category != "Pre-season") %>%
  rename("In-season" = Count) 

observations_df = Preaseason_vs_Models_df_Observed[,c("Count","Year")]

preseason_and_inseason_df_scatter_plot <- Preseason_only_df  %>%
  left_join(Inseason_only_df, by = "Year") %>%
  left_join(observations_df, by = "Year") %>%
  rename("Pre-season Label" = Category.x,
         "Category" = Category.y,
         "Observed" = Count)


preseason_and_inseason_df_scatter_plot$Category = as.factor(preseason_and_inseason_df_scatter_plot$Category)
preseason_and_inseason_df_scatter_plot$`Pre-season Error` = preseason_and_inseason_df_scatter_plot$`Pre-season` - preseason_and_inseason_df_scatter_plot$Observed
preseason_and_inseason_df_scatter_plot$`In-season Error` = preseason_and_inseason_df_scatter_plot$`In-season` - preseason_and_inseason_df_scatter_plot$Observed
write.csv(preseason_and_inseason_df_scatter_plot, "data/Forecast_error.csv")

Inseason_vs_Preseason_Plot_Scatter_Plot = ggplot(data = preseason_and_inseason_df_scatter_plot)+
  geom_point(aes(x = `Pre-season`,  y = `In-season`, color = Category), size = 3)+
  scale_color_manual(values = c("blue","red", "black"))+
  labs(x = "Pre-season Prediction", y = "In-season Forecast", title ="" )+
  theme_classic()+
  theme(legend.margin = margin(0,0,0,0, unit = "cm"),
        legend.position = "none",
        plot.margin = margin(0,0,0,1, unit = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
  )

Inseason_vs_Preseason_Error_Plot_Scatter_Plot = ggplot(data = preseason_and_inseason_df_scatter_plot)+
  geom_point(aes(x = `Pre-season Error`,  y = `In-season Error`, color = Category), size = 3)+
  scale_color_manual(values = c("blue","red","black"))+
  labs(x = "Pre-season Prediction Error", y = "In-season Forecast Error", title ="" )+
  theme_classic()+
  theme(legend.margin = margin(0,0,0,0, unit = "cm"),
        plot.margin = margin(0,0,0,1, unit = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
  )

ggarrange(Inseason_vs_Preseason_Plot_Scatter_Plot, Inseason_vs_Preseason_Error_Plot_Scatter_Plot, ncol = 2, labels = c("A","B"), widths = c(0.67, 1)) + theme(plot.margin = margin(.5,1,0,.5, unit = "cm") )  

cor(x = preseason_and_inseason_df_scatter_plot$`Pre-season`, y = preseason_and_inseason_df_scatter_plot$`In-season`)

#Obs_vs_Pred_Plot = full_prospective_df_with_weighted[,-4]


Forecasting_Plot_Weighted

Obs_vs_Pred_Plot_Weighted

Inseason_vs_Preseason_Plot

ggarrange(Forecasting_Plot_Weighted, Inseason_vs_Preseason_Plot, ncol = 1, labels = c("A","B")) + theme(plot.margin = margin(.5,1,0,.5, unit = "cm"))  

Length_vs_Run_Plot

Mean_Percent_Difference_Length_Plot

ggarrange(ggarrange(Length_vs_Run_Plot_age_2,Length_vs_Run_Plot_age_3, ncol = 2, labels = c("A","B")),
          ggarrange(Mean_Percent_Difference_Length_Plot_age_2,Mean_Percent_Difference_Length_Plot_age_3, ncol = 2, nrow = 1, labels = c("C","D")),
          nrow = 2,ncol = 1
) +
  theme(plot.margin = margin(.5,1,0,.5, unit = "cm"))  

# Model_Runs_Age_2 = list()
# Model_Runs_Age_3 = list()
Total_Run_1980_2023

days_for_MAPE_Graph = seq(172,198,1)

Weighted_MAPE_2000_2023_vector = c()
MAPE = c()

step = 0

for(j in 1:length(days_for_MAPE_Graph))
{
  step = 0
  for(i in 1:length(test_start_years))
  {
    
    
    step = step+1
    
    Age2 = one_step_ahead_regression(meanLengths= Age2meanLengths_all_1980_2023,age="Age 2", day = days_for_MAPE_Graph[j],  
                                     Total_Run = Total_Run_1980_2023,
                                     Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                     pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                     Odd_Even_Year = Odd_Even_Year,
                                     PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                     PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = test_start_years[i])
    
    
    Age3 = one_step_ahead_regression(meanLengths= Age3meanLengths_all_1980_2023,age="Age 2", day = days_for_MAPE_Graph[j],  
                                     Total_Run = Total_Run_1980_2023,
                                     Total_Run_Previous_Year=Total_Run_Previous_Year_1979to2022,
                                     pinksalmonPrevious_Year = pinksalmon_1979to2022,
                                     Odd_Even_Year = Odd_Even_Year,
                                     PreviousSummerMeanTemp=SummerMeanTemp1979to2022,
                                     PreviousWinterMeanTemp=WinterMeanTemp1979to2023,startyear=1980,endyear=2023,start_test_year = test_start_years[i])
    
    
    years = as.character(seq(1980,2023,1))
    
    df = data.frame(as.matrix(years, ncol = 1))
    colnames(df) = "years"
    
    age2df <- data.frame(t(Age2[[1]][1:10,]) ) %>%
      mutate(years = colnames(Age2[[1]]))
    
    age3df <- data.frame(t(Age3[[1]])) %>%
      mutate(years = colnames(Age3[[1]]))
    
    df<-df %>%
      left_join(age2df, by = "years", keep_all = TRUE) %>%
      left_join(age3df, by = "years", keep_all = TRUE)
    
    df<-df[,-1]
    
    retro_years = seq(1980,(test_start_years[i]-1),1)
    onestep = df[length(seq(1980,test_start_years[i],1)),]
    retro = df[1:length(retro_years),] #We need models as columns for the function to work properly
    
    #Calculate variance from within sample predictions from observed
    #length(seq(1980,1999,1))
    obs = Total_Run_1980_2023[1:(length(retro_years))]
    within_sample_predictions = retro 
    
    SD = SD_Error(within_sample_predictions, observations=obs)/sqrt(length(obs))
    Var = SD^2
    
    #This uses 1980 to t-1 to obtain top 5 models
    ordered_var[[step]] = order(Var, decreasing = FALSE)[1:n_models] #Top 5 models by variance
    
    #inverse variance for the top 5 inseason models
    inverse_var[[step]] = 1/Var[ordered_var[[step]]] 
    model_weights[[step]] = inverse_var[[step]] /sum(inverse_var[[step]])
    #Use these weights to obtain a retrospective weighted in-sample prediction
    
    #Generate weighted retrospective prediction starting in 1993 to year t-1
    original_predictions = retro[14:length(retro_years),ordered_var[[step]]]
    weighted_predictions[[step]] = data.frame(matrix(rowSums(model_weights[[step]]*original_predictions), ncol = 1))
    
    variance_from_ensemble_within_sample[[step]] = SD_Error(weighted_predictions[[step]], observations=Total_Run_1980_2023[14:length(retro_years)])/sqrt(length(Total_Run_1980_2023[14:length(retro_years)]))                            
    
    #length(seq(1993, (test_start_years[i]-1),1))
    
    variance_From_preaseason[[step]] = SD_Error(data.frame(as.matrix(FRI_Preseason_Total_Forecast_1993_2023[1:(length(seq(1993, (test_start_years[i]-1),1)))], ncol = 1)), observations = Total_Run_1980_2023[14:length(retro_years)])/sqrt(length(Total_Run_1980_2023[14:length(retro_years)]))
    
    inverse_variance_from_inseason_ensemble_and_preseason[[step]]= 1/c(variance_from_ensemble_within_sample[[step]], variance_From_preaseason[[step]])
    
    #This calculates weights of weighted inseason models and preseason forecast
    
    final_model_weights[[step]] = inverse_variance_from_inseason_ensemble_and_preseason[[step]]/sum(inverse_variance_from_inseason_ensemble_and_preseason[[step]])
    #Use the final model weights in year t+1 to generate the out of sample (one step ahead) prediction
    
    #So, we need to extract the correct inseason models based on weights, calculate the inseason forecast based on weights, and multiply them and the preseason by the above weights
    
    one_step_ahead = unlist(onestep)
    top_5_one_step = sum(one_step_ahead[ordered_var[[step]]]*model_weights[[step]]) #multiply top 5 by weights and sum to generate weighted inseason prediction
    #Extract top 5 by variance
    one_step_ahead_top_5_and_preseason = c(top_5_one_step, FRI_Preseason_Total_Forecast_1993_2023[length(seq(1993, (test_start_years[i]),1))])
    
    weighted_out_of_sample_prediction[step] = sum(one_step_ahead_top_5_and_preseason*final_model_weights[[step]])
    
    observed_return = Total_Run_1980_2023[(length(retro_years)+1)]
    
    MAPE[i] = abs(weighted_out_of_sample_prediction[step]-observed_return)/observed_return
  }
  
  
  Weighted_MAPE_2000_2023_vector[j] = mean(MAPE,na.rm = TRUE) 
  
}


dates_2 = seq(as.Date(format = "%m/%d", "06/20"), by = "day", length.out = 27)

Method = as.factor(c(rep("Weighted",length(dates_2))))

Currys_metrics = read.csv("data/perf.summary_pred.csv")
Currys_metrics = Currys_metrics[which(Currys_metrics[,".metric"] == "mape"),]
dates_curry = seq(as.Date(format = "%m/%d", "06/20"), by = "day", length.out = 27)
dates_curry2 = dates_curry[seq(1, 27, 2)]
Existing_Inseason = as.factor(rep("Existing inseason",14))

format(as.Date("2020-06-20"),"%j")

Curry_MAPE = Currys_metrics[,".estimate"][4:17]
Curry_DF = data.frame(Curry_MAPE)

Curry_DF[,"Date"] = dates_curry2
Curry_DF[,"Method"] = Existing_Inseason
colnames(Curry_DF) = c("MAPE", "Date", "Method")




MAPE_over_time_df = data.frame(c(Weighted_MAPE_2000_2023_vector)*100)

MAPE_over_time_df[,"Dates"] = dates_2
MAPE_over_time_df[,"Method"] = Method
colnames(MAPE_over_time_df) = c("MAPE", "Date" , "Method")

MAPE_over_time_df_2 = rbind(MAPE_over_time_df, Curry_DF)


ggplot(data = MAPE_over_time_df_2, aes(x = Date, y = MAPE, lty = Method, color = Method)) +
  geom_line()+
  labs(x="Date", y="MAPE") +
  scale_color_manual(values = c("red","blue"))+
  scale_linetype_manual(values = c(1,1,2))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.text.x = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        panel.spacing = unit(2, "lines")) 


estimate_age_returns_2023 = read.csv("data/ASL_Export_7-7.csv")
Salt.Water.Age.2023 = estimate_age_returns_2023[,'age'] %% 10
mean.Length.2023.age_2 = mean(estimate_age_returns_2023[which(Salt.Water.Age.2023 == 2),"Length"], na.rm = TRUE)
mean.Length.2023.age_3 = mean(estimate_age_returns_2023[which(Salt.Water.Age.2023 == 3),"Length"], na.rm = TRUE)
