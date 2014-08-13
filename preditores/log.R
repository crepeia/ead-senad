# Libraries
library(ggplot2)
library(reshape2)

# Set dataframe
setwd("logs/")

# Read log file
logData  <- read.delim("log.txt", col.names= c("turma", "curso", "hora", "ip",  "fullname",  "action",	"info"), header = FALSE)

## PRE-PROCESSING DATE ----

# Remove curso var
logData  <- logData[, -2]

### Variable Date
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
logData$hora <- strptime(logData$hora, format="%d %m %Y, %H:%M")


### Variable Activity
# Create var without the url path
logData$activity <- gsub("\\s\\(.*\\)", "", logData$action)

cbind(sort(table(logData$activity), decreasing = TRUE))

barplot(sort(table(logData$turma), decreasing = TRUE))
mean(table(logData$turma))
abline(h = mean(table(logData$turma)))

# QUESTION 1 - IS THE PLATFORM ACTIVITY CHANGING OVER TIME - MONTHS?

# Activity per month
table(months(logData$hora))

#Graph
graphAccess  <- ggplot(logData, aes(logData$hora))
graphAccess + geom_density(colour = "blue", fill="blue") + labs(x = "Month", y = "Frequency") 

# QUESTION 2 - IS THE PLATFORM ACTIVITY CHANGING OVER TIME - WEEKDAYS?

# Weekdays
table(weekdays(logData$hora))

# QUESTION 2 - HOW DOES THE PLATFORM ACTIVITY CHANGE OVER DAYTIME?

cbind(sort(table(logData$activity), decreasing = TRUE))

# QUESTION 3 - WHO ARE THE MOST ACTIVE AND UNACTIVE USERS?
cbind(sort(table(logData$fullname), decreasing = FALSE))


# QUESTION 4 - WHAT ARE THE MOST VIEWED?
by(logData$activity, logData$turma, table)
tapply(logData$activity, logData$fullname, table, simplify=FALSE)

tapply(logData$activity, logData$fullname, table, simplify=FALSE)
tapply(logData$activity, logData$turma, table)


teste  <- tapply(logData$activity, logData$turma, table, simplify=FALSE)
df <- ldply(teste, data.frame)

head(df)

# QUESTION 5 - WHAT ARE THE MOST ACTIVE CLASSES?
cbind(sort(table(logData$turma), decreasing = FALSE))

#Graph
graphAccess  <- ggplot(logData, aes(logData$))
graphAccess + geom_density(colour = "blue", fill="blue") + labs(x = "Month", y = "Frequency") 


