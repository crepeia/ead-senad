# Libraries
library(ggplot2)
library(reshape2)
library(plyr)

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
## PostIx
logData$hora <- strptime(logData$hora, format="%d %m %Y, %H:%M")
## as Date
logData$time  <- as.Date(logData$hora, "%d %m %Y, %H:%M")


### Variable Activity
# Create var without the url path
logData$activity <- gsub("\\s\\(.*\\)", "", logData$action)

head(logData$action)


# QUESTION 1 - IS THE PLATFORM ACTIVITY CHANGING OVER TIME - MONTHS?

# Activity per month
table(months(logData$hora))

#Graph
graphAccess  <- ggplot(logData, aes(logData$hora))
graphAccess + geom_density(colour = "pink", fill="pink") + labs(x = "Month", y = "Frequency") 

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


# QUESTION 5 - WHAT ARE THE MOST ACTIVE CLASSES?

activeClasses  <- tapply(logData$activity, logData$fullname, table, simplify=FALSE)
df <- ldply(activeClasses, data.frame)

dfCast  <- dcast(df, .id ~ Var1)
names(dfCast)
head(dfCast)

Column <- gvisColumnChart()

barplot(sort(table(logData$turma), decreasing = TRUE), col = "blue", ylim = c(0,40000))
abline(h = mean(table(logData$turma)), colour="blue")

#Graph
graphAccess  <- ggplot(logData, aes(logData$))
graphAccess + geom_density(colour = "blue", fill="blue") + labs(x = "Month", y = "Frequency") 


## PERGUNTA - Quem são os usuários ativos - ativo = usou a plataforma nos último 15 dias.
dataUltimo <- by(logData$time, logData$fullname, max, simplify=FALSE)
ultimoAcesso  <- do.call("c", dataUltimo)
dfUltimoAcesso <- as.data.frame(ultimoAcesso)
table(dfUltimoAcesso$ultimoAcesso > "2014-08-18")
write.csv(dfUltimoAcesso, "ultimoAcesso.csv")

write.csv(dfCast, "ultimoAcesso.csv")

