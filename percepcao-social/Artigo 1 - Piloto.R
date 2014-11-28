###########################
# First Paper -----------
###########################

# Load packages

library(car) # Function Recode
library(psych) # Function Describe


# Import data ----
# Questions
questions  <- read.csv("percepcaosocial_questions.csv", header = FALSE)

# Analysis----
## Import dataframe
socialPer  <- read.csv("percepcaosocial_df.csv")
## Summing scales to remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,24:65])
## Subset completed observations and consented participation
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & socialPer$estado=="Finalizadas" & !is.na(socialPer$scaleSum))