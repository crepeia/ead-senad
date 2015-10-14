###########################
# 2nd PAPER     -----------
###########################

###########################
# OPEN DATAFRAME
###########################

# Load Libraries
library(car)      # Load packge - used function Recode
library(psych)    # Load packge - used for EFA
library(mirt)     # CFA
library(xtable)   # Load xtable package
library(ltm)      # LTM analysis

# Open Data Frame
praticasPro  <- read.csv("praticasprofissionais_df.csv", stringsAsFactors = FALSE)

# Convert items into numeric
# for (i in c(32:68)){
#   praticasPro[,i]   <-  Recode(praticasPro[,i], "'Discordo totalmente'=1 ; 'Discordo'=2 ; 'Nem discordo, nem concordo' = 3; 'Concordo' = 4; 'Concordo totalmente' = 5; else = NA")
#   praticasPro[,i]   <-  as.numeric(praticasPro[,i])  
# }

## Sum items to remove NA's
praticasPro$scaleSum  <- rowSums(praticasPro[,32:68])

# Subset only with participants who consented - N 3247 agreed
praticasPro <- subset(praticasPro, praticasPro$termo == "Sim")

################
# DEMOGRAPHICS
################

## Age
praticasPro$age <- gsub("[[:alpha:]]", " ", praticasPro$idade) # Remove all alphabetic characters
praticasPro$age  <- as.numeric(praticasPro$age) # Convert into numeric
praticasPro$age[praticasPro$age < 18 | praticasPro$age > 68 ]  <- NA # Recode values with conversion problem into NA

## Sex
cbind(round(prop.table(sort(table(praticasPro$sexo), decreasing = TRUE)),3))*100

## Degree
cbind(round(prop.table(sort(table(praticasPro$escolaridade), decreasing = TRUE)),3))*100

## Contact 
cbind(round(prop.table(sort(table(praticasPro$contatoanterior), decreasing = TRUE)),3))*100

## Deal with
cbind(round(prop.table(sort(table(praticasPro$lidadiretamente), decreasing = TRUE)),3))*100

################
# EFA AND IRT
################

# Subset only with all cases completed
aScale <- subset(praticasPro, !is.na(praticasPro$scaleSum))

# Select Scale items
aScale  <-  aScale[,32:68]

## Reverse code items according to Theory
# for (i in c(14,19,28,29,32,33,34,35)) {
#   aScale[,i] <- recode(aScale[,i], "1=5;2=4;4=2;5=1")
# }

###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
polyAll <- polychoric(aScale)
# Rotated PCA
faAll_rot <- principal(polyAll$rho, nfactors = 2, rotate="oblimin")
print.psych(faAll_rot, digits=2, cut= .4)
# Results - No tems were removed since all had loadings larger than .4


###########################
# CFA - BFACTOR
###########################

item_pos <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
efaVersion  <-   c(1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2,1,2,2)
theoryVersion <- c(1,1,2,1,1,1,1,1,1,2,1,1,2,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2)
dimShort <-cbind(item_pos, efaVersion, theoryVersion)

# EFA result
cfaEFA <- bfactor(shortScale, efaVersion)
summary(cfaEFA)
coef(cfaEFA)

# Theory result
cfaTheory <- bfactor(shortScale, theoryVersion)
summary(cfaTheory)
coef(cfaTheory)
# Results - We have a few items that have low factor loadings although they have good G loadings. I cannot judge whether we could proceed the IRT analysis using this factor model.

###########################
# IRT
###########################

# Create Short Scale - Version 1
aScale1 <- shortScale[, c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]

# Grade Response Model
triShort1 <- grm(aScale1)

# Estimate Coeficients
coef(triShort1)

# Plot graphs
par(mfrow = c(4,4))
for (i in 1:16) {  
plot(triShort1, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}

# Create Short Scale - Version 2
aScale2 <- shortScale[, -c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]

# # Grade Response Model
triShort2 <- grm(aScale2)

# Estimate Coeficients
coef(triShort2)

# Plot graphs
par(mfrow = c(4,4))
for (i in 1:15) {  
  plot(triShort2, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}
