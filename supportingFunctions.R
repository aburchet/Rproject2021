

# This script defines functions to convert files to .csv, combine them into one,
# and

library(ggplot2)
library(cowplot)

txt2csv<- function(directory){ 
  # This function converts space or tab-delimited .txt files in .csv files in a given directory.
  # user must provide directory name that is within working directory, or else a complete path.
  # new .csv files are located within the given directory.
  # given directory should contain only .txt files. 
  
  files<-list.files(directory) # a list of the files in the directory
  for (file in files){
    df<-read.table(file=paste(directory,"/",file, sep=""), header=TRUE)
    name=gsub(".txt", ".csv", file)
    write.csv(x=df, file=paste(directory,"/",name, sep=""), row.names=FALSE)
  } # end file loop. 
}# End txt2csv


compileCSV<- function(directories, na=3){ 
  # User provides a list of directories, and specifies how NA values should be treated:
  # Set na=1 to remove rows with NA, na=2 to keep them, and na=3 to keep with a warning
  # The directories must all be in the current working directory, or else be complete paths. 
  
  combined<-data.frame(matrix(ncol=14, nrow=0)) # Initialize dataframe to hold all data
  colnames(combined)<-c("gender","age","marker01","marker02","marker03", "marker04",
                        "marker05", "marker06","marker07", "marker08", "marker09",
                        "marker10", "country", "dayofYear")
  for (directory in directories){
      files<-list.files(directory, pattern="*.csv")
      country<-directory  
      for (i in 1:length(files)){
            # assume directories are named after the country
          day<-strtoi(gsub("[a-z]|_*|\\.", "", files[i])) # Obtain the day from the number in filename
          df<-read.csv(paste(directory,"/",files[i], sep=""))
          
          # Dealing with NAs
          if (na==1){
            df<-df[complete.cases(df),] #only include lines without NA
          }else if (na==3){
            if (any(is.na(df))){ # if there are any NAs in dataset...
              print("Warning: This dataset contains incomplete entries! NA present")
            } 
          } # end if na== sequence.. else if na==2, will compile as is. 
          
          # Add country and dayofYear columns:  
          countryvec<-rep(gsub("country","",country), times=nrow(df)) # note, could remove "country" from the from to ge the country name
          dayvec<-rep(day, times=nrow(df))
          
          
          df$country<-countryvec 
          df$dayofYear<-dayvec 
          
          combined<-rbind(combined, df) # appends current file info to large combined dataframe
        }# end file loop
  }# End directory loop

  write.csv(x=combined, file="combinedScreeningData.csv", row.names=FALSE)
  print("All done! Data has been compiled to combinedScreeningData.csv the working directory.")

}#end compileCSV


# need to modify so that can also do all data...

summarize<-function(data, countryName="all"){
  # if no country name input, will use all data
  # note - some of this doesn't make sense to me, make it work then come bakc and fix...
  
  # For each row in the data inputted, denote whether that person was infected (1) or not (0)
  infected_patients <- numeric(length <- nrow(data))
  for(i in 1:nrow(data)){
    data$infected_patients[i] <- sum(sum(data[i,3:12]) > 0) # counts number of patients pos for any marker.
  }
  
  #create a separate object for the country inputted to be able to look only at
  #the percent infected in this country
  
  # this is where could put if/else for all or some of data.
  if (countryName=="all"){
    data_countryName <- data
  }else{
    data_countryName <- data[(data$country == countryName),]# subset of data that matches input country
  }
  #find the patients infected per each day in this country
  day <- data_countryName$dayofYear
  infected <- data_countryName$infected_patients
  df <- data.frame(day, infected)
  patients_infected_per_day  <- aggregate(infected ~ day, df, sum)
  
  #find the total patients per each day in this country
  total_patients_per_day <- table(data_countryName$dayofYear)
  total_patients_per_day <- data.frame(total_patients_per_day)
  colnames(total_patients_per_day) <- c("day", "num_patients")
  
  #create a new dataframe with all this new per-day information
  per_day_infections <- data.frame(patients_infected_per_day, total_patients_per_day$num_patients)
  colnames(per_day_infections) <- c("day", "infected", "num_patients")
  
  #add a column for percent of infected patients per day to this new dataframe
  per_day_infections$percent_infected <- (per_day_infections$infected / per_day_infections$num_patients) * 100
  
  #create a scatterplot for the Percent Infected vs Day for this country
  country_countryName <- ggplot(per_day_infections, aes(x = day, y = percent_infected)) +
    geom_point() +
    theme_classic() +
    ylim(0, 100) +
    xlab("Day") +
    ylab("Percent Infected (%)") +
    ggtitle("Country", countryName) +
    theme_classic()
  #return this scatterplot as the output of this function
  return(country_countryName)
}



