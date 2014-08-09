# Set dataframe
setwd("logs/")

# Read all csv's in log folder
filenames <- list.files()

for(i in 1:length(filenames)) {
  
  data  <- filenames[i]
  assign(data, read.table(filenames[i], sep = "", header=TRUE, skip=1,  blank.lines.skip = FALSE))
  
}

final  <- rbind(class302.txt, class303.txt, class304.txt, class305.txt)

names(final)
head(final$Hora)
