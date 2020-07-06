#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors
#'particulate matter data from the directory specified in the 'directory' 
#argument and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA.

pollutmean <- function(directory, pollutant, id=1:332){
  
  data_loc<- paste0( getwd(),"/",directory)
  
  data_req<- data.frame()
  
  for(i in id){
    if( i < 10){
      data <- read.csv( paste(data_loc,"/00",as.character(i),".csv",sep="") , header= TRUE, as.is = TRUE)
      data_req<- rbind(data_req,data)
    }
    else if( i < 100){
      data <- read.csv( paste(data_loc,"/0",as.character(i),".csv", sep="") , header= TRUE, as.is = TRUE)
      data_req<- rbind(data_req,data)
    }
    else {
      data <- read.csv( paste(data_loc,"/",as.character(i),".csv", sep="") , header= TRUE, as.is = TRUE)
      data_req<- rbind(data_req,data)
    }
  }
  mean(data_req[,pollutant],na.rm=TRUE)
} 




# Write a function that reads a directory full of files and reports the number 
#of completely observed cases in each data file. The function should return a 
#data frame where the first column is the name of the file and the second column 
#is the number of complete cases. 



complete<- function(directory, id = 1:332){
  dir_loc<- paste0(getwd(),"/",directory)
  
  empty_frame <- data.frame()
  
  for(i in id){
    if (i <10){
      data_file<- read.csv(paste(dir_loc,"/00",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    else if (i <100){
      data_file<- read.csv(paste(dir_loc,"/0",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    else {
      data_file<- read.csv(paste(dir_loc,"/",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    
    nobs <- sum(complete.cases(data_file))
    dataa<- data.frame(i,nobs)
    empty_frame<- rbind(empty_frame,dataa)
    
  }
  empty_frame
}



#Write a function that takes a directory of data files and a threshold for 
#complete cases and calculates the correlation between sulfate and nitrate 
#for monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. The function should 
#return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.


corr <- function(directory,threshold=0){
  location_dir <- paste0(getwd(),"/",directory)
 
  corr_vector<- NULL
  for(i in 1:332){
    if (i <10){
      data_file2<- read.csv(paste(location_dir,"/00",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    else if (i <100){
      data_file2<- read.csv(paste(location_dir,"/0",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    else {
      data_file2<- read.csv(paste(location_dir,"/",as.character(i),".csv",sep=""), header=TRUE, as.is= TRUE)
    }
    corrected_data<- data_file2[complete.cases(data_file2),]
    if( nrow(corrected_data) > threshold){
      corr_vector<- c(corr_vector,cor(corrected_data[,"sulfate"],corrected_data[,"nitrate"]))
    }
  }
  corr_vector
}
