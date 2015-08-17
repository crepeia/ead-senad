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

###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
faAll <- fa.poly(aScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut= .4)
# Results - Items 9,11,12,13,18,21,22,25,30,31,36 need to be removed due to small loadings < (.4).

# SECOND EFA -------
# Remove items with low loadings detected in previous step.
shortScale  <- aScale[, -c(9,11,12,13,18,21,22,25,30,31,36)]
faShortScale <- fa.poly(shortScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShortScale, digits=2, cut= .4)
# Results - Items 20 needs to be removed due to small loading < (.4).

# THIRD EFA -------
# Remove item 20 # Caution - Do not integrate this step into previous one because 15 represents the position of variable (not its name).
shortaScaleF  <- shortScale[, -c(15)] 
faAll_F <- fa.poly(shortaScaleF, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll_1, digits=2, cut= .4)
# Results - All items have good enough loadings. Please, refer to these results for further analysis.

# RECODE ITEMS WITH NEGATIVE LOADINGS
# Items 14,19,28,29,32,33,34,35.
for (i in c(10,14,19,20,21,22,23,24)){
  shortaScaleF[,i]   <-  Recode(shortaScaleF[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")
}

###########################
# CFA - BFACTOR
###########################

efaVersion  <-   c(1,1,1,1,1,1,1,1,1,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
theoryVersion <- c(1,1,2,1,1,1,1,1,1,1,1,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2)

# EFA result
cfaEFA <- bfactor(shortaScaleF, efaVersion)
summary(cfaEFA)
coef(cfaEFA)

# Theory result
cfaTheory <- bfactor(shortaScaleF, theoryVersion)
summary(cfaTheory)
coef(cfaTheory)
# Results - We have a few items that have low factor loadings although they have good G loadings. I cannot judge whether we could proceed the IRT analysis using this factor model.

###########################
# IRT
###########################
