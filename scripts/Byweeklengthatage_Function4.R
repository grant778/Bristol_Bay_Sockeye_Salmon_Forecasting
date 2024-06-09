

mean_length_by_week_function2 = function(data,startyear, endyear)
{
  
  River_age2 = which(data[,"Salt.Water.Age"] == 2 )
  River_age3 = which(data[,"Salt.Water.Age"] == 3 )
  
  
  River_data_age2 = data[River_age2,]
  River_data_age3 = data[River_age3,]
  
  
  
  date_river_age2 = as.Date(River_data_age2[,'sampleDate'])
  max(date_river_age2,na.rm = TRUE)
  
  date_river_age3 = as.Date(River_data_age3[,'sampleDate'])
  
  
  #Bristol_catch = rowSums(data1[,2:9]) #data1 and data2 were previously function parameters
  #Bristol_escapement = rowSums(data2[,2:9])

  
  
  #River_data_age3[which(dateplaceholdersage3 <= "2020-06-12"),"Year"]
  
  dateplaceholdersage2 = format(date_river_age2,"%j")
  dateplaceholdersage3 = format(date_river_age3,"%j")
  
  dateplaceholdersage2 = as.numeric(dateplaceholdersage2)
  dateplaceholdersage3 = as.numeric(dateplaceholdersage3)
  
  weeksage2 = seq(165,165+89,1)
  weeksage3 = seq(165,165+89,1)
  
  
  
  format(as.Date("2020-06-16"),"%j")
  
  weekdivisionsage2 = list()
  weekdivisionsage3 = list()
  
  for (i in 1:90) 
  {
    weekdivisionsage2[[i]] = which(dateplaceholdersage2 <= weeksage2[i])
    weekdivisionsage3[[i]] = which(dateplaceholdersage3 <= weeksage3[i])
  }
  
  
  
  yearsequence = seq(from = startyear, to = endyear)
  
  meanlengthbyweektable = function(age_specific_data,weekdivisions)
  {
    meanlengthmatrix = array(0, dim = c(length(yearsequence),90))  
    samplesizematrix = array(NA, dim = c(length(yearsequence),90))  
    for(j in 1:90)
    {
      
      weekRiverInfo = age_specific_data[weekdivisions[[j]],]
      
      
      for(i in 1:length(yearsequence))
      {
        
         meanlengthmatrix[i,j] = mean(weekRiverInfo[which(weekRiverInfo[,'Year'] == yearsequence[i]),'Length'],na.rm = TRUE)
         samplesizematrix[i,j] = length(weekRiverInfo[which(weekRiverInfo[,'Year'] == yearsequence[i]),'Length'])
         
      }
    } 
    return(list(meanlengthmatrix,samplesizematrix))
  }
  
 
  Age2meanLengths = meanlengthbyweektable(River_data_age2,weekdivisionsage2)
  
  RiverAge2meanLengths = Age2meanLengths[[1]]
  row.names(RiverAge2meanLengths) = as.character(yearsequence)
  colnames(RiverAge2meanLengths) = weeksage2
  
  RiverAge2samplesize = Age2meanLengths[[2]]
  row.names(RiverAge2samplesize) = as.character(yearsequence)
  colnames(RiverAge2samplesize) = weeksage2
  
  Age3meanLengths = meanlengthbyweektable(River_data_age3,weekdivisionsage3)
  
  RiverAge3meanLengths = Age3meanLengths[[1]]
  row.names(RiverAge3meanLengths) = as.character(yearsequence)
  colnames(RiverAge3meanLengths) = weeksage3
  
  RiverAge3samplesize = Age3meanLengths[[2]]
  row.names(RiverAge3samplesize) = as.character(yearsequence)
  colnames(RiverAge3samplesize) = weeksage3
  
  testlist = list(RiverAge2meanLengths,RiverAge3meanLengths,RiverAge2samplesize,RiverAge3samplesize)
  
  return(testlist)
}
