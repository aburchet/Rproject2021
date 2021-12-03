

# This script defines functions to convert files to .csv, combine them into one,
# and

library(ggplot2)
library(cowplot)


txt2csv<- function(directory){ 
  # This function converts space or tab-delimited .txt files in .csv files in a given directory.
  # user must provide directory name that is within working directory.
  # new .csv files are located within the given directory.
  # given directory should contain only .txt files. 

  filelist<-list.files(directory) # A list of the files in the directory

  for (file in filelist){
    df<-read.table(file=paste(directory,"/",file, sep=""), header=TRUE)
    name=gsub(".txt", ".csv", file)
    write.csv(x=df, file=paste(directory,"/",name, sep=""), row.names=FALSE)
  } # End file loop.
}# End txt2csv


compileCSV<- function(directories, na=3){ 
  # User provides a list of directories, and specifies how NA values should be treated:
  # Set na=1 to remove rows with NA, na=2 to keep them, and na=3 to keep with a warning
  # The directories must all be in the current working directory and all files to be 
  # included must be .csv with columns: gender, age, and marker01-marker10
  # Directories must be named with "country" followed by country name (eg, countryX),
  # and files must be named according to day of year: screen_DDD.csv.
  
  combined<-data.frame(matrix(ncol=14, nrow=0)) # Initialize dataframe to hold all data
  colnames(combined)<-c("gender","age","marker01","marker02","marker03", "marker04",
                        "marker05", "marker06","marker07", "marker08", "marker09",
                        "marker10", "country", "dayofYear")
  for (directory in directories){
      files<-list.files(directory, pattern="*.csv")
      country<-directory  
      for (i in 1:length(files)){
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
          countryvec<-rep(gsub("country","",country), times=nrow(df))
          dayvec<-rep(day, times=nrow(df))
          
          
          df$country<-countryvec 
          df$dayofYear<-dayvec 
          
          combined<-rbind(combined, df) # appends current file info to large combined dataframe
        }# end file loop
  }# end directory loop

  write.csv(x=combined, file="combinedScreeningData.csv", row.names=FALSE)
  print("All done! Data has been compiled to combinedScreeningData.csv the working directory.")

}#end compileCSV


summarize<-function(data, countryFolder){
# Summarizes data and produces a figure with percent male and female patients, 
# patient age distribution, and percent infected over time. 
# Input dataframe should include all screening data in standard format from compileCSV(),
# and input country folder vector should contain exactly two country folder names to be compared.

    
  print(paste("Total number of screens run: ", nrow(data)))
  pFemale<-100*sum(data$gender=="female")/nrow(data)
  pMale<-100*sum(data$gender=="male")/nrow(data)
  pOther<-100-(pFemale+pMale) # To generalize for datasets with missing gender information
  gender<-data.frame(group=c("Female", "Male", "Other/NA"), percent=c(pFemale, pMale, pOther))
  
  genderpie<-ggplot(gender, aes(x="", y=percent, fill=group)) +
                geom_bar(stat="identity", width=1) +
                coord_polar("y", start=0) +
                ggtitle("Patient Gender")
  
  print(paste("Of all patients screened,",pFemale,"% were female and",pMale, "% were male."))
  
  age<-mean(data$age)
  sd<-sd(data$age)
  max<-max(data$age) # problem! max age is 423.....but that's in the given dataset too. 
  min<- min(data$age)
  print(paste("The average patient age was",age,"years, plus or minus", sd, "years."))
  print(paste("The oldest reported patient was", max, "years old, and the youngest was", min, "years old."))
  
  ageplot<-ggplot(data, aes(x=age)) +  # note, this looks bad because the max age is 423 in the dataset...
    geom_density() +
    theme_classic() +
    ggtitle("Patient Age Distribution")
  
  print("Please wait while we prepare your figure...")
  
  # For each row in the data inputted, denote whether that person was infected (1) or not (0)
  infected_patients <- numeric(length <- nrow(data))
  for(i in 1:nrow(data)){
    data$infected_patients[i] <- sum(sum(data[i,3:12]) > 0) # counts number of patients pos for any marker.
  }
  
  
  plotInfected <- function(data, country){
    # This function created plot of percent infected over time for each of the
    # two given countries. Not to be modified by user.
    data_countryName <- data[(data$country == country),]
    
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
    countryPlot <- ggplot(per_day_infections, aes(x = day, y = percent_infected)) +
      geom_point() +
      theme_classic() +
      ylim(0, 100) +
      xlab("Day") +
      ylab("Percent Infected (%)") +
      theme_classic() +
      ggtitle(paste("Country",country))
    #return this scatterplot as the output of this function
    return(countryPlot)
  }
  
  plot1 <- plotInfected(data, gsub("country", "", countryFolder[1])) # removes "country" from folder name
  plot2 <- plotInfected(data, gsub("country", "", countryFolder[2]))
  allPlot<-plot_grid(genderpie, ageplot, plot1, plot2,
                   labels="AUTO")
  return(allPlot)
  print("Summary completed.")
  
}


vaccine<-function(alldata, countries){
  # Compares marker expression similarity on the last day for two countries,
  # and predicts whether they are similar enough for a vaccine developed in 
  # one to work in another. INput requirements are the same as for 
  # summarize(), countries must be a vector of two country folder names.
  
  files1<-list.files(countries[1])
  files2<-list.files(countries[2])# lists all files
  
  X <- read.csv(paste(countries[1],"/",tail(files1,1), sep=""), header=T, stringsAsFactors = F)
  Y <- read.csv(paste(countries[2],"/",tail(files1,1), sep=""), header=T, stringsAsFactors = F)
  
  #Make a dataframe with only the marker data
  Xm<-data.frame(X[,3:ncol(X)])
  Ym<-data.frame(Y[,3:ncol(Y)])
  
  #transpose the dataframe made above such that the rows become columns and visa versa
  #(do this because the next step acts on columns, not rows)
  Xm_transposed <- data.frame(t(Xm))
  Ym_transposed <- data.frame(t(Ym))
  
  #Determine how many patients' marker combinations in country X exactly match those
  #from country Y
  sim_Xm_Ym <- sum(Xm_transposed %in% Ym_transposed)
  
  #Calculate the percent of exact similarity between the two:
  #percent similarity = |A n B| / ((|A| + |B|) - |A n B|)
  percent_sim <- (sim_Xm_Ym / (sum(nrow(Xm), nrow(Ym))- sim_Xm_Ym)) *100
  print(paste("Percent similarity of markers 1-10 on last day: ",percent_sim))
  
  # For an arbitrary similarity cut-off, 50%, make vaccine prediction:
  if (percent_sim < 50){
    print("Low similarity in marker expression - vaccine unlikely to work.")
  }else {
    print("significant similarity in marker expression - vaccine may work.")
  }

  lastday<-alldata[alldata$dayofYear==175,]
  lastday<-na.omit(lastday)
  lastday<-lastday[,3:13]
  
  
  data<-melt(lastday, id.vars="country") #converts lastday into long format
  data2<-data[data$value==1,] #removes 0s from value column (will only show occurrences of markers)
  ggplot(data2, aes(x=variable, y=value, fill=country))+
    geom_col(stat="count")+
    xlab("Markers")+ylab("Occurrences of Marker") +
    theme(axis.text.x=element_text(angle = 45))
}







