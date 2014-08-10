# Set dataframe
setwd("logs/")

# Read log file
logData  <- read.table("log.txt", sep = "", header = FALSE, blank.lines.skip = FALSE, col.names= c("turma", "curso", "hora", "ip",  "fullname",	"action",	"info"), stringsAsFactors=FALSE)

# Remove curso var
logData  <- logData[, -2]
                   
# Convert PT months into numbers
logData$hora  <- gsub('dezembro', "12", logData$hora)
logData$hora  <- gsub('novembro', "11", logData$hora)
logData$hora  <- gsub('outubro', "10", logData$hora)
logData$hora  <- gsub('setembro', "09", logData$hora)
logData$hora  <- gsub('agosto', "08", logData$hora)
logData$hora  <- gsub('julho', "07", logData$hora)
logData$hora  <- gsub('junho', "06", logData$hora)
logData$hora  <- gsub('maio', "05", logData$hora)
logData$hora  <- gsub('abril', "04", logData$hora)
logData$hora  <- gsub('março', "03", logData$hora)
logData$hora  <- gsub('fevereiro', "02", logData$hora)
logData$hora  <- gsub('janeiro', "01", logData$hora)

# Convert to date 
logData$hora  <- as.Date(logData$hora, "%d %m %Y, %H:%M")

# Access by Date
library(ggplot2)
graphAccess  <- ggplot(logData, aes(hora))
graphAccess + geom_freqpoly(colour = "blue", binwidth = 10) + labs(x = "Month", y = "Frequency") 

