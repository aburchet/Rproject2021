

# This script defines functions to convert files to .csv, combine them into one,
# and 

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
}


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
          countryvec<-rep(country, times=nrow(df)) 
          dayvec<-rep(day, times=nrow(df))
          
          
          df$country<-countryvec 
          df$dayofYear<-dayvec 
          
          combined<-rbind(combined, df) # appends current file info to large combined dataframe
        }# end file loop
  }# end directory loop

  write.csv(x=combined, file="combinedScreeningData.csv", row.names=FALSE)
  print("All done! Data has been compiled to combinedScreeningData.csv the working directory.")

}#end function

