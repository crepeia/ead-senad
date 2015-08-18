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
attitudesE  <- read.csv("atitudesEducadores_df.csv")

# Convert into numeric
for (i in c(33:71)){
  attitudesE[,i]   <-  Recode(attitudesE[,i], "'Discordo totalmente'=1 ; 'Discordo'=2 ; 'Nem discordo, nem concordo' = 3; 'Concordo' = 4; 'Concordo totalmente' = 5; else = NA")
  attitudesE[,i]   <-  as.numeric(attitudesE[,i])  
}

## Sum items to remove NA's
attitudesE$scaleSum  <- rowSums(attitudesE[,33:71])

# Subset with those who consented
attitudesE <- subset(attitudesE, attitudesE$termo == "Sim" & !is.na(attitudesE$scaleSum))

# Select Scale items
aScale  <-  attitudesE[,33:71]

## Reverse code items according to Theory
for (i in c(14,19,28,29,32,33,34,35)) {
  aScale[,i] <- recode(aScale[,i], "1=5;2=4;4=2;5=1")
}

###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
polyAll <- polychoric(aScale)
# Unrotated PCA
faAll <- principal(polyAll$rho, nfactors = 2)
print.psych(faAll, digits=2, cut= .4)
#Rotated PCA
faAll_rot <- principal(polyAll$rho, nfactors = 2, rotate="oblimin")
print.psych(faAll_rot, digits=2, cut= .4)

# Results - Items 9,11,12,13,18,21,22,25,30,31,36 need to be removed due to small loadings < (.4).

# SECOND EFA -------
# Remove items with low loadings detected in previous step.
shortScale  <- aScale[, -c(9,11,13,18,21,25,31, 39)]
polyShort <- polychoric(shortScale)
#Unrotated PCA
faShortScale <- principal(polyShort$rho, nfactors = 2)
print.psych(faShortScale, digits=2, cut= .4)
#Rotated PCA
faShortScale_rot <- principal(polyShort$rho, nfactors = 2, rotate="oblimin")
print.psych(faShortScale_rot, digits=2, cut= .4)


###########################
# CFA - BFACTOR
###########################

item_pos <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
efaVersion  <-   c(1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2,1,2,2)
theoryVersion <- c(1,1,2,1,1,1,1,1,1,2,1,1,2,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2)
dimShort<-cbind(item_pos, efaVersion, theoryVersion)

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

aScale1 <- shortScale[, c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]

triShort1 <- grm(aScale1)
coef(triShort1)

par(mfrow = c(4,4))
for (i in 1:16) {  
plot(triShort1, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}

aScale2 <- shortScale[, -c(1,2,3,4,5,6,7,8,9,10,12,14,16,17,24,29)]
triShort2 <- grm(aScale2)
coef(triShort2)

par(mfrow = c(4,4))
for (i in 1:15) {  
  plot(triShort2, type = "ICC", items = i,  main = "CCI", ylab = "Probabilidade", xlab = "Proficiência", annot=TRUE, legend=FALSE)
}
