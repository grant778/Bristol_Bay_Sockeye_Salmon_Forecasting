#This function is used to calculate average length at age for fish in year y on day d of the fishing season
#This script is designed to work with the format of the data provided from ADFG, so modifications may be necessary 

mean_length_by_day_function2 = function(data,startyear, endyear)
{
  
  River_age2 = which(data[,"Salt.Water.Age"] == 2 )
  River_age3 = which(data[,"Salt.Water.Age"] == 3 )
  
  
  River_data_age2 = data[River_age2,]
  River_data_age3 = data[River_age3,]
  
  
  
  date_river_age2 = as.Date(River_data_age2[,'sampleDate'])
  max(date_river_age2,na.rm = TRUE)
  
  date_river_age3 = as.Date(River_data_age3[,'sampleDate'])
  
  
  dateplaceholdersage2 = format(date_river_age2,"%j")
  dateplaceholdersage3 = format(date_river_age3,"%j")
  
  dateplaceholdersage2 = as.numeric(dateplaceholdersage2)
  dateplaceholdersage3 = as.numeric(dateplaceholdersage3)
  
  daysage2 = seq(165,165+119,1)
  daysage3 = seq(165,165+119,1)
  
  
  
  format(as.Date("2020-06-16"),"%j")
  
  daydivisionsage2 = list()
  daydivisionsage3 = list()
  
  for (i in 1:120) 
  {
    daydivisionsage2[[i]] = which(dateplaceholdersage2 <= daysage2[i])
    daydivisionsage3[[i]] = which(dateplaceholdersage3 <= daysage3[i])
  }
  
  
  
  yearsequence = seq(from = startyear, to = endyear)
  
  meanlengthbydaytable = function(age_specific_data,daydivisions)
  {
    meanlengthmatrix = array(0, dim = c(length(yearsequence),120))  
    samplesizematrix = array(NA, dim = c(length(yearsequence),120))  
    for(j in 1:120)
    {
      
      dayRiverInfo = age_specific_data[daydivisions[[j]],]
      
      
      for(i in 1:length(yearsequence))
      {
        
         meanlengthmatrix[i,j] = mean(dayRiverInfo[which(dayRiverInfo[,'Year'] == yearsequence[i]),'Length'],na.rm = TRUE)
         samplesizematrix[i,j] = length(dayRiverInfo[which(dayRiverInfo[,'Year'] == yearsequence[i]),'Length'])
         
      }
    } 
    return(list(meanlengthmatrix,samplesizematrix))
  }
  
 
  Age2meanLengths = meanlengthbydaytable(River_data_age2,daydivisionsage2)
  
  RiverAge2meanLengths = Age2meanLengths[[1]]
  row.names(RiverAge2meanLengths) = as.character(yearsequence)
  colnames(RiverAge2meanLengths) = daysage2
  
  RiverAge2samplesize = Age2meanLengths[[2]]
  row.names(RiverAge2samplesize) = as.character(yearsequence)
  colnames(RiverAge2samplesize) = daysage2
  
  Age3meanLengths = meanlengthbydaytable(River_data_age3,daydivisionsage3)
  
  RiverAge3meanLengths = Age3meanLengths[[1]]
  row.names(RiverAge3meanLengths) = as.character(yearsequence)
  colnames(RiverAge3meanLengths) = daysage3
  
  RiverAge3samplesize = Age3meanLengths[[2]]
  row.names(RiverAge3samplesize) = as.character(yearsequence)
  colnames(RiverAge3samplesize) = daysage3
  
  testlist = list(RiverAge2meanLengths,RiverAge3meanLengths,RiverAge2samplesize,RiverAge3samplesize)
  
  return(testlist)
}
