#This script generates model predictions as well as relevant figures and tables for forecasting Bristol Bay Sockeye returns
#The various tested models (approximately a dozen) are read in from the helper script "salmon_regression_models_V5_08_03_2023_cyclic_pink_salmon_2.R"

#Read in packages
library("readxl")
library("ggplot2")
library("tidyverse")
library(ggplot2)
library(tidyr)
library(ggpubr)

#Read in data

#Longterm Bristol Bay catch data
data1 = read.csv("data/BB-sockeye-catch.csv")

#Longterm Bristol Bay escapement data
data2 = read.csv("data/BB-sockeye-escapement.csv")

#Bristol Bay length and age dadta through 2020
data3 = read.csv("data/BBasl_to_2020.csv")

#Length and age dadta for 2021, 2022, and 2023
seasondata2021 = read_excel("data/FDMS2021_V3.xls")
seasondata2022 = read_excel("data/FDMS2022.xlsx") #updates
seasondata2023 = read.csv("data/FDMS2023.csv")

#Preseasons forecast data
preseason = read.csv("data/Preseason.csv")

#Past reconstructed total run size data
Totals = read.csv("data/2021.csv") #Bristol Bay sockeye returns from 1950 to 2021

#North Pacific Pink Salmon and Chum Salmon Time series data
data4 = read.csv("data/pink_chum_salmon_total_abundances.csv")

# 
# #Age specific returns by watershed
# estimate_age_returns = read.csv("data/2021_Return_Tables_Bristol_Bay.csv")

#North Pacific Sea surface temperatures
OceanTemps1 = read.csv("data/SST-BristolBay3.csv")

#Read in helper scripts

source('scripts/lengthatage_Function_clean.R')
source('scripts/meanrelativeerrorcode_clean.R')
source('scripts/salmon_regression_models_clean.R')

#Section 01. 
#Purpose: Reformat ASL updates for 2021, 2022 and 2023 seasons to be in same format

seasondata2023$catch_date = format(as.Date(seasondata2023$catch_date, "%m/%d/%Y"), "%Y-%m-%d")

seasondata2023$sample_date = format(as.Date(seasondata2023$sample_date, "%m/%d/%Y"), "%Y-%m-%d")

seasondata2023 <- seasondata2023 %>%
  mutate(Year = rep(2023, nrow(seasondata2023)), .before = district) %>%
  rename(ASLProjectType = project_type,
         Length = length,
         sampleDate = sample_date)

colnames(seasondata2022)
colnames(seasondata2023)

#Combine the ASL data for 2021 to 2023 (these are the updates to the original ASL dataset which was through 2020)

seasondata2021_2023 = rbind(seasondata2021,seasondata2022,seasondata2023)

#Additional cleaning/reformatting of the dataframe
Salt.Water.Age = seasondata2021_2023[,'age'] %% 10 

seasondata2021_2023[,'Salt.Water.Age'] = Salt.Water.Age

#Ensure only Sockeye salmon are used (species_code = 420)

seasondata2021_2023V2 = seasondata2021_2023[which(seasondata2021_2023[,'species_code']==420 ),] #Filter for sockeye

#Reformatting dataframe, ensure variable types are correct
seasondata2021_2023V2 = as.data.frame(seasondata2021_2023V2[,c("Length","Salt.Water.Age","sampleDate","Year","ASLProjectType")])

seasondata2021_2023V2 = na.omit(seasondata2021_2023V2)

Length2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Length"]))
Salt.Water.Age2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Salt.Water.Age"]))
sampleDate2021_2023 = as.Date(seasondata2021_2023V2$sampleDate)

Year2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"Year"]))

ASLProjectType2021_2023 = as.numeric(unlist(seasondata2021_2023V2[,"ASLProjectType"]))

ProjectType2021_2023 = ASLProjectType2021_2023 

#Convert numeric project type IDs to the correct character string
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

#Add 2021 through 2023 ASL data to the original longterm ASL dataset through 2020
#So this dataset is now from 1979 through 2023
data3 = rbind(data3,seasondata2021_2023V2)

data_all = data3 #Can only use at aggregate level since escapement is by river and commercial catch is by district

#Section 02.
#Purpose: Wrangle SST and pink salmon time series data so they start and end in correct years. 
#Need 1980 to 2023 for year t vectors and 1979 to 2022 for year t-1 vectors

#Wrangle Pink salmon data
data5 = data4[20:61,] #truncate so its the starts and ends one year before data1 and 2 (1979 to 2020)
#Wrangle SST data
OceanTemps1979to2020 = OceanTemps1[32:73,]#Truncate so it starts and ends one year before data1 and 2 1979 to 2020
OceanTemps1979to2023 = OceanTemps1[32:76,]

pinksalmon = data5[,'pinks']
pinksalmon_1979to2021 = c(pinksalmon,797) # final 2021 pink salmon was 797 million
pinksalmon_1979to2022 = c(pinksalmon_1979to2021, 409) 

seq(from = 1979, to = 2021, by = 1)[16]

pinksalmon_1994to2021 = pinksalmon_1979to2021[16:length(pinksalmon_1979to2021)]

sd(pinksalmon, na.rm = TRUE)

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


#Section 03. 
#Purpose: Calculate average length at each for each day of the season for years 1958 through 2023

meanlengthatagealldata = mean_length_by_week_function2(data_all, 1958, endyear = 2023) #All data including the test fishery


x = data.frame(meanlengthatagealldata[[1]]) #age2
age3 = data.frame(meanlengthatagealldata[[2]])
#x2 = x[,1:14]

x_percent = array(dim = c(63,18))
x_percent = x / x[,18]

age3_percent = array(dim = c(63,18))
age3_percent = age3 / age3[,18]

#all data
#Extract age 2 length at age data
Age2meanLengths_all = meanlengthatagealldata[[1]] 
#Catch data starts in 1963, truncate so this ASL data also starts in 1963.
#NOTE: earlier versions of the script combined catch and escapement to obtain total returns. Current version uses reconstructed total return tables
#This truncation at 1963 is a legacy of that earlier methodology but we end up using data only back to 1979 anyway later in the script to the regime shift). 
#This section is still important to ensure all dataframes and vectors start in the correct year
Age2meanLengths_all = Age2meanLengths_all[6:nrow(Age2meanLengths_all),] #Truncate at 1963

#Extract age 2 sample sizes
Age2meanLengths_all_sample_size = meanlengthatagealldata[[3]]
Age2meanLengths_all_sample_size = Age2meanLengths_all_sample_size[6:nrow(Age2meanLengths_all_sample_size),] 

#Extract age 3 length at age
Age3meanLengths_all = meanlengthatagealldata[[2]]

#Catch data starts in 1963, truncate so this ASL data also starts in 1963

#NOTE: earlier versions of the script combined catch and escapement to obtain total returns. Current version uses reconstructed total return tables
#This truncation at 1963 is a legacy of that earlier methodology but we end up using data only back to 1979 anyway later in the script to the regime shift). 
#This section is still important to ensure all dataframes and vectors start in the correct year

Age3meanLengths_all = Age3meanLengths_all[6:nrow(Age3meanLengths_all),] #Truncate at 1963

#Extract age 3 sample sizes
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

#Section 04.
#Purpose: Calcluates total returns and truncates model variabes to correct timeframe
#time frame is either 1980 to 2023 or 1979 to 2022 for lagged variables



#Calculate total return sizes for each year from 1963 to 2021

years = seq(from = 1963, to = 2021, by = 1)
Total_Run_Original = c()
for(i in 1:length(years))
{
  Total_Run_Original[i] = sum(Totals[which(Totals[,"retYr"] == years[i]),"return"])
}

Total_Run_Original = c(Total_Run_Original,83281.914, 54485.107 ) # add 2022 estimate 80000, add 2023 estimate #2023 Bristol Bay total run 54,485,107
Total_Run = Total_Run_Original


#Truncate total returns to relevant start and end year (1979 to 2022) since this is autoregressive dataset

Total_Run_Previous_Year_1979to2022 = Total_Run_Original[17:60]

#Truncate total returns to relevant start and end year (1980 to 2023) since this is the response variable (NOT autoregressive term)

#Tested a few different test periods so that is what these different timeframes are for
Total_Run_2010_2023 = Total_Run_Original[48:61] #2010 to 2023
Total_Run_2005_2023 = Total_Run_Original[43:61] #2005 to 2023
Total_Run_2000_2023 = Total_Run_Original[38:61] #2000 to 2023
Total_Run_1980_2023 = Total_Run_Original[18:61] #1980 to 2023

#Truncate age-length dataframes so they also start in 1980 and end in 2023

Age2meanLengths_all_1980_2023 = Age2meanLengths_all[18:61,]
Age3meanLengths_all_1980_2023 = Age3meanLengths_all[18:61,]

#Identify odd or even year for model term
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



library(pls)
set.seed(2)

#Add row and column names to dataframe
row_names = rownames(Age2meanLengths_all_1980_2023)
col_names = colnames(Age2meanLengths_all_1980_2023)
  
#Make scaled version of DF to try the effect of scaled variables
Age2meanLengths_all_1980_2023_scaled = t(apply(Age2meanLengths_all_1980_2023,1, scale))
Age3meanLengths_all_1980_2023_scaled = t(apply(Age3meanLengths_all_1980_2023,1, scale))

rownames(Age2meanLengths_all_1980_2023_scaled) = row_names
rownames(Age3meanLengths_all_1980_2023_scaled) = row_names

colnames(Age2meanLengths_all_1980_2023_scaled) = col_names
colnames(Age3meanLengths_all_1980_2023_scaled) = col_names

nrow(Age2meanLengths_all_1980_2023)

#Section 05. 
#Purpose: Run the models for age 2 and age 3
#one_step_ahead_regression() is a function from helper script 'scripts/salmon_regression_models_V5_08_03_2023_cyclic_pink_salmon_2.R'

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


#Calculate R squared for the best models for each age 
#For age 2 the best model was model 3
  #for age 2 model 5 was almost equivalent
#For age 3 the best model was model 5
  #For age 3 model 3 was almost equivalent.

Age_2_Sum_Sq_DF_Model_3 = array(dim = c(24,3)) #3 variables
Age_2_R2_Model_3 = c()

Age_3_Sum_Sq_DF_Model_3 = array(dim = c(24,3)) #3 variables
Age_3_R2_Model_3 = c()


#Model 3
for(i in 1:24)
{

 Age_2_R2_Model_3[i] = ModelPredictionsAge2[["ANOVA"]][[5]][[i]][["adj.r.squared"]]
 Age_2_Sum_Sq_DF_Model_3[i,] = ModelPredictionsAge2[["ANOVA"]][[6]][[i]][["Sum Sq"]][1:3] #3 variables
 
 Age_3_R2_Model_3[i] = ModelPredictionsAge3[["ANOVA"]][[5]][[i]][["adj.r.squared"]]
 Age_3_Sum_Sq_DF_Model_3[i,] = ModelPredictionsAge3[["ANOVA"]][[6]][[i]][["Sum Sq"]][1:3] #3 variables

}


#IMPORTANT; ERROR IN RESULTS SECTION OF MANUSCRIPT
#CURRENTLY LISTS BEST AGE 2 MODEL AS INCLUDING SIZE AT AGE, RUN SIZE IN PREVIOUS YEAR, PREVIOUS SUMMERS SST, AND PREVIOUS PINK ABUNDANCE
#HOWEVER: MODEL 3 IS ONLY LENGTH AT AGE, RUN SIZE IN T-1, AND PINK SALMON ABUNDANCE IN T-1
#Fix this after Curry makes his comments/revisions

mean_Age_2_R2_Model_3 = mean(Age_2_R2_Model_3)
mean_Age_3_R2_Model_3 = mean(Age_3_R2_Model_3)

Age2_percent_explained = colMeans(Age_2_Sum_Sq_DF_Model_3/rowSums(Age_2_Sum_Sq_DF_Model_3))
Age3_percent_explained = colMeans(Age_3_Sum_Sq_DF_Model_3/rowSums(Age_3_Sum_Sq_DF_Model_3))

retrospective_and_onestep_ahead_age2= ModelPredictionsAge2[[1]]
retrospective_and_onestep_ahead_age3= ModelPredictionsAge3[[1]]

par(mfrow=c(1,1))


years = row.names(Age2meanLengths_all_1980_2023)


#Section 05.
#Calculate mean size at age for each ocean age class (ocean age 2 and ocean age 3)
#Calculate mean (across years) difference between mean (within season) size at age up through each day of season vs end of season mean size at age

SizeAtAge2 = data_all[which(data_all[,'Salt.Water.Age'] == 2 & data_all[,'Year'] >= 1963 ),]
#No 2006 age 2 length at age
SizeAtAge3 = data_all[which(data_all[,'Salt.Water.Age'] == 3 & data_all[,'Year'] >= 1963),]

#This vector and the ensuing size at age dataset is years 1980 to 2023
years = seq(from = 1980, to = 2023)

MeanSizeAtAge2 = c()
MeanSizeAtAge3 = c() 
MeanSizeAtAge2_June_24 = c()
MeanSizeAtAge3_June_24 = c() 


for(i in 1:length(years))
{
  #Extract average size at age in each year. This gives the full end of year size at age
  MeanSizeAtAge2[i] = mean(SizeAtAge2[which(SizeAtAge2[,'Year'] == years[i]),'Length'])
  
  MeanSizeAtAge3[i] = mean(SizeAtAge3[which(SizeAtAge3[,'Year'] == years[i]),'Length'])
  
}


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


#Section 06.
#Purpose: Graphing average error in mean length at age through each day of the season vs end of season mean length at age
#This section of the script primarily delves into data output, primarily graphs

#Graph average error in length on each day of the season 

#Graphs both ages faceted
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


#Graphs just age 2

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

#Graphs just age 3

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


#Graph Size at age vs run size

Mean_Length = c(MeanSizeAtAge2_June_24, MeanSizeAtAge3_June_24, MeanSizeAtAge2, MeanSizeAtAge3)
Run = c(Total_Run_1980_2023,Total_Run_1980_2023,Total_Run_1980_2023,Total_Run_1980_2023)
Time = c(rep("A",length(Total_Run_1980_2023)), rep("A", length(Total_Run_1980_2023)),rep("B",length(Total_Run_1980_2023)), rep("B", length(Total_Run_1980_2023)))
Age = c(rep("Age 2",length(Total_Run_1980_2023)), rep("Age 3",length(Total_Run_1980_2023)), rep("Age 2",length(Total_Run_1980_2023)), rep("Age 3",length(Total_Run_1980_2023)))

Mean_Length = c(MeanSizeAtAge2, MeanSizeAtAge3)
Run = c(Total_Run_1980_2023,Total_Run_1980_2023)
Age = c(rep("Ocean age-2",length(Total_Run_1980_2023)), rep("Ocean age-3", length(Total_Run_1980_2023)))



Age_vs_Run_df2 = data.frame(cbind(Mean_Length, Run))
Age_vs_Run_df2[,"Age"] = Age
Age_vs_Run_df2[,"Run"] = Age_vs_Run_df2[,"Run"]/1000

#Graph size at age vs run size for both ages

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

 #Graph size at age vs run size for age 2
 
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
 
 #Graph size at age vs run size for age 3
 
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
 
 
#Section 07.
#This section generates the weighted model predictions.
#Methodology further described below
  
one_step_ahead_ModelPredictions_Age2 = ModelPredictionsAge2[[2]]
one_step_ahead_ModelPredictions_Age3 = ModelPredictionsAge3[[2]]



one_step_ahead_ModelPredictions_All_Ages = cbind(one_step_ahead_ModelPredictions_Age2,
                                                 one_step_ahead_ModelPredictions_Age3)

colnames(one_step_ahead_ModelPredictions_All_Ages) = c("Model 1 age 2", "Model 2 age 2","Model 3 age 2", "Model 4 age 2","Model 5 age 2","Model 6 age 2", "Model 7 age 2", "Model 8 age 2","Model 9 age 2", "Model 10 age 2", "Model 11 age 2", "Model 12 age 2", "Model 13 age 2", "Model 14 age 2", "Model 15 age 2", "Model 16 age 2", "Model 17 age 2", "Model 18 age 2", "Model 19 age 2", "Model 20 age 2", "Model 21 age 2", "Model 22 age 2", "Model 23 age 2",
                                                       "Model 1 age 3", "Model 2 age 3","Model 3 age 3", "Model 4 age 3","Model 5 age 3","Model 6 age 3", "Model 7 age 3", "Model 8 age 3","Model 9 age 3", "Model 10 age 3", "Model 11 age 3", "Model 12 age 3", "Model 13 age 3", "Model 14 age 3", "Model 15 age 3", "Model 16 age 3", "Model 17 age 3", "Model 18 age 3", "Model 19 age 3", "Model 20 age 3", "Model 21 age 3", "Model 22 age 3", "Model 23 age 3")



model_predictions_No_Duplicates = one_step_ahead_ModelPredictions_All_Ages[,1:35] #drop last 11 model with just environmental covariates and No SaA (these are duplicates)

#Wrangle pre-season data format, ensure the year ranges are correcft

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

#Weighted model code


#Step 1. Fit Inseason models to data through 1999 
#Step 2. Use within sample predictions to calculate variance for each of the inseason models up to 1999.
#Step 3. Record variance for each inseason model.
#Step 4. Calculate inverse variance weighted inseason ensemble across top 5 inseason models
#Step 5. Determine variance in predictions from observations with weighted inseason ensemble 1993-1999
#Step 6. Calculate 1993-1999 variance for preseason forecast
#Step 7. Weight preseason and inseason ensemble predictions to generate forecast for 2000, based on their variances 1993-1999
#Step 8. Repeat through 2023

model_standard_errors = list()
model_standard_errors_with_preseason = list()
step = 0
model_MAPEs = list()
model_standard_errors_Top_Four = list()
ordered_MAPES = list()
n_models = 5

model_predictions_No_Duplicates_with_preseason = data.frame(model_predictions_No_Duplicates)
model_predictions_No_Duplicates_with_preseason[,"Pre-season"] = FRI_Preseason_Total_Forecast_2000_2023


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

#Run models for age 2 and age 3  
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

retro_and_one_step = rbind(Age2[[1]][1:12,],Age3[[1]]) #Column names are years, remove duplicate models (non length at age models)
retro_years = seq(1980,(test_start_years[i]-1),1)
onestep = t(retro_and_one_step[,length(seq(1980,test_start_years[i],1))])
retro = t(retro_and_one_step[,as.character(retro_years)]) #We need models as columns for the function to work properly

#Calculate variance from within sample predictions from observed

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

#Reformat dataframe, rescale variables

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

#Graph retrospective fit vs weighted model out of sample predictions
#Also graphs observed run sizes in background

Forecasting_Plot_Weighted = ggplot(data = Weighted_Prediction_Each_Year_df) +
  geom_line(aes(x = as.numeric(Year), y = Fit, linetype = Type), size = 1 ) +
  geom_point(data = Weighted_Prediction_Each_Year_df, aes(x = as.numeric(Year), y = Observed_Run), color = "black")+
  labs(x="Year", y="Run Size") +
  geom_vline(xintercept = 2000, color = "red")+
  scale_linetype_manual(values=c("solid","dashed"))+
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

#Graph observations vs weighted predictions to see strength of correlation

Obs_vs_Pred_Plot_Weighted <- ggplot(data = Weighted_Prediction_Each_Year_df %>%
                                      filter(Type == "Prospective"), 
                                    aes(x = Fit, y = Observed_Run)) +
  geom_point(size = 2.5) +
  labs(x= "Predicted (in millions)", y="Observed (in millions)") +
  geom_abline(intercept = 0, slope = 1, color = "black")+
  theme_classic()+
  theme(plot.margin = margin(1,1,0,1, unit = "cm"),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 14, hjust = 0),
        
        strip.background = element_rect(color="white", fill="white", linetype="solid"))+
  scale_y_continuous(name = "Observed (in millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))+
  scale_x_continuous(name = "Predicted Run (millions)", breaks = c(15, 25, 35, 45, 55, 65, 75,85), limits = c(15, 85))

#Calculate summary stats for weighted model out of sample predictions

Mean_error_weighted = Mean_Absolute_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
Max_error_weighted = Maximum_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
sd_error_weighted = SD_Relative_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)
correlation_weighted = corelation_function(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)                                      
Percent_error_weighted = Percent_Absolute_Error(data.frame(Weighted_Prediction_Each_Year_2000_2023),observations=Total_Run_2000_2023)

#Calculate summary stats for preseason forecast

Mean_Absolute_Error(data.frame(FRI_Preseason_Total_Forecast_2000_2023), observations=Total_Run_2000_2023)

Maximum_Error(data.frame(FRI_Preseason_Total_Forecast_2000_2023), observations=Total_Run_2000_2023)

#Calculate summary stats for age specific models

Percent_error = Percent_Absolute_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)
Percent_Error_Model_7_age_2 = Percent_error[,7]
Percent_Error_Model_5_age_3 = Percent_error[,28]

Mean_error = Mean_Absolute_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)

Max_error = Maximum_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)

sd_error = SD_Relative_Error(one_step_ahead_ModelPredictions_All_Ages,observations=Total_Run_2000_2023)


correlations = corelation_function(one_step_ahead_ModelPredictions_All_Ages, observations=Total_Run_2000_2023) 

#Combine summary statistics into table

Results_Table = t(rbind(c(Mean_error, Mean_error_weighted), c(Max_error, Max_error_weighted), c(sd_error, sd_error_weighted), c(correlations, correlation_weighted)))

Age = c(rep("2",12),rep("NA",11), rep("3",12),rep("NA",11),"All")
Model = (c(c(1:23),c(1:23),"Weighted"))

Updated_Results_Table = (cbind(Results_Table,Age,Model))

Updated_Results_Table = as.matrix(Updated_Results_Table)

colnames(Updated_Results_Table) = c("Mean Error","Max Error", "SD Error", "Correlation", "Age", "Model")

#Output summary statistic table

write.csv(Updated_Results_Table,"data/results_table_predictions_for_06_24 (revised oct 13 2024).csv" )

#Graph model inseason predictions versus pre-season prediction

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
  geom_point(data = Preaseason_vs_Models_df_2, aes(x = Year, y = Count, color = Category), size = 3)+

  #scale_shape_manual(values = c(19, 19))+
  scale_color_manual(values = c("blue","red"))+
  labs(x = "Year", y = "Observed Run (millions)", title ="" )+
  theme_classic()+
  scale_y_continuous(name = "Observed Run (millions)", breaks = c(5, 15, 25, 35, 45, 55, 65, 75, 85), limits = c(0, 85))+
  scale_x_discrete(name = "Year", breaks = seq(2000,2023,2))+ 
  theme(legend.position = c(0.4,0.9), legend.margin = margin(0,0,0,0, unit = "cm"),
        plot.margin = margin(0,0,0,1, unit = "cm"), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
  ) + guides(colour = guide_legend(nrow = 1))

Inseason_vs_Preseason_Plot 

MAPE_preseason = mean(abs(1-(FRI_Preseason_Total_Forecast_2000_2023/(Total_Run_2000_2023))))

#Graph scatter plots of preseason predictions versus in-season ensemble forecast
#Also graph correlation of the error between the two variables

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

#Output table of forecast error for preseason forecast vs inseason weighted ensemble model

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


#Generate multipanel graph with two rows
#Top row is retrospective vs prospective fit for weighted model
#Second row is column graph of observed run size vs year, with points depicting weighted and pre-season predictions for each year

Forecasting_Plot_Weighted

Obs_vs_Pred_Plot_Weighted

Inseason_vs_Preseason_Plot

ggarrange(Forecasting_Plot_Weighted, Inseason_vs_Preseason_Plot, ncol = 1, labels = c("A","B")) + theme(plot.margin = margin(.5,1,0,.5, unit = "cm"))  

#Generate multipanel graph with two rows and two columns
#Top row features total sockeye return abundance vs mean length at age graphs
#Second row features mean absolute percent difference (averaged across years) between average length on day d of season vs end of season average length

Length_vs_Run_Plot

Mean_Percent_Difference_Length_Plot

ggarrange(ggarrange(Length_vs_Run_Plot_age_2,Length_vs_Run_Plot_age_3, ncol = 2, labels = c("A","B")),
          ggarrange(Mean_Percent_Difference_Length_Plot_age_2,Mean_Percent_Difference_Length_Plot_age_3, ncol = 2, nrow = 1, labels = c("C","D")),
          nrow = 2,ncol = 1
) +
  theme(plot.margin = margin(.5,1,0,.5, unit = "cm"))  

#This section graphs inseason weighted ensemble model forecast error vs forecast error of current inseason methods for a series of days in the season
#So x axis is day and y axis is error
#One line is for the new inseason method with the weighted ensemble model, other line is for existing inseason bayesian method from Curry Cunningham


#To do this we need to loop through days and run the models and calculate the weighted prediction on each day
#Then calculate error from weighted prediction vs observation

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
    
    age2df <- data.frame(t(Age2[[1]][1:12,]) ) %>%
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

#Read in performance summary for existing in-season methodology from Curry Cunningham

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

#Graph MAPE for existing inseason method and new weighted inseason forecast method versus day of year

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
        panel.spacing = unit(2, "lines"), legend.position = "bottom") 


estimate_age_returns_2023 = read.csv("data/ASL_Export_7-7.csv")
Salt.Water.Age.2023 = estimate_age_returns_2023[,'age'] %% 10
mean.Length.2023.age_2 = mean(estimate_age_returns_2023[which(Salt.Water.Age.2023 == 2),"Length"], na.rm = TRUE)
mean.Length.2023.age_3 = mean(estimate_age_returns_2023[which(Salt.Water.Age.2023 == 3),"Length"], na.rm = TRUE)

