# Set dataframe
setwd("logs/")

# Read all csv's in log folder
filenames <- list.files()
dfnames  <- gsub(".txt", "", filenames)


for(i in 1:length(filenames)) {
    
  assign(dfnames[i], read.table(filenames[i], sep = "", header=TRUE, skip=1,  blank.lines.skip = FALSE))
  dfnames[i]
  
}

?aggregate.data.frame
