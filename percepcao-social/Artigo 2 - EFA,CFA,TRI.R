###########################
# 2nd PAPER     -----------
###########################

# Load Libraries
library(psych)  # Function Describe
library(mirt)   # Function mirt IRT 
library(car)    # Function Recode

# Open Data Frame
attitudesE  <- read.csv("atitudesEducadores_df.csv")

# Invert items with negative loadings
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

# Factor analysis using polychoric correlations
faAll <- fa.poly(aScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut= .4)

# Remove items with low loadings
shortaScale  <- aScale[, -c(9,11,12,13,18,21,22,25,30,31,36)]

# shortaScale - Factor analysis using polychoric correlations
faAll_1 <- fa.poly(shortaScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll_1, digits=2, cut= .4)

# Remove item 20
shortaScale  <- shortaScale[, -c(15)]

# shortaScale - Factor analysis using polychoric correlations
faAll_2 <- fa.poly(shortaScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll_2, digits=2, cut= .4)


# Invert items with negative loadings
for (i in c(14,19,28,29,31,33,34,35)){
fullScale[,i]   <-  Recode(fullScale[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")                         
}

## CFA - Confirmatory factor analysis ---
# bfactor - Análise confirmatória usando bifactor  

stVersion  <- c(1,1,2,1,1,1,1,1,1,1,2,2,1,2,1,2,1,2,1,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2)
ndVersion  <- c(1,1,2,1,1,1,1,1,1,1,2,2,1,1,1,2,1,2,1,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2)

# Full Scale - st Version. item 14 in position a
cfa <- bfactor(aScale, stVersion)

summary(cfa)
coef(cfa)

# Full Scale - st Version. item 14 in position b
cfa <- bfactor(aScale, ndVersion)