#Calculates relative error using a leave one out analysis between predicted run size on day d of the fishing season and observed run size at the end of the season
#Tested just using a very basic version of the models tested.


meanrelativeerror = function(MeanSizeAtAge,AgeSpecificMeanLengthsByWeek,Total_Run)
  
  
{
  
years = row.names(AgeSpecificMeanLengthsByWeek)

regressions = array(0, dim = c(length(years),2))  


for(i in 1:length(years))
{
  t = lm(Total_Run[-i]~MeanSizeAtAge[-i])
  regressions[i,] = t[["coefficients"]] # regression between everything except year i
  
}

row.names(regressions) = years

errors = data.frame(array(0, dim = c(length(years),90)))

row.names(errors) = years
names(errors)<- paste0("week",seq(90))

for(i in 1:length(years))  
{
  for(j in 1:90)
  {
    estimated_run = regressions[i,2]*AgeSpecificMeanLengthsByWeek[i,j]+regressions[i,1]
    errors[i,j] = abs(Total_Run[i]-estimated_run) #prediction error
    
    #errors[i,j]<-abs((Total_Run[i]-estimated_run)/Total_Run[i]) #mean relative error
  }
}

return(errors)
}
