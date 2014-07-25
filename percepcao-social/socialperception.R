# Libraries ----
library(car) # Function Recode
library(psych) # Function Describe
library(mirt) # Function mirt IRT 

# Import data ----

# Questions
questions  <- read.csv("percepcaosocial_questions.csv")
questionsLabels  <- as.vector(questions[1:39,]); rm(questions)

# Analysis----
## Import dataframe
socialPer  <- read.csv("percepcaosocial_df.csv")
## Summing scales to remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,34:71])
## Subset completed observations and consented participation
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & socialPer$estado=="Finalizadas" & !is.na(socialPer$scaleSum))

# Demographics
## Age
### Clean data
socialPer$idade  <- as.numeric(as.character(socialPer$idade))
socialPer$idade[socialPer$idade < 18 | socialPer$idade > 68 ]  <- NA


### Descriptives
summary(socialPer$idade) # all
by(socialPer$idade, socialPer$sexo, describe) #by sex

## Sex
cbind(round(prop.table(sort(table(socialPer$sexo), decreasing = TRUE)),2))

## Degree
cbind(round(prop.table(sort(table(socialPer$escolaridade), decreasing = TRUE)),2))

## Marital Staus
cbind(round(prop.table(sort(table(socialPer$estadocivil), decreasing = TRUE)),2))

## Education
#cbind(round(prop.table(table(socialPer$formacao)),2)) # Broken, needs manual recoding

## Ocupação
#cbind(round(prop.table(table(socialPer$ocupacao)),2)) # Broken, needs manual recoding

## Time  working
timeWorking  <- as.numeric(as.character(socialPer$tempodeservico))
timeWorking[timeWorking > 59]  <- NA
summary(timeWorking)

## Religion 
cbind(round(prop.table(sort(table(socialPer$religiao), decreasing = TRUE)),2))

## Contact 
cbind(round(prop.table(sort(table(socialPer$contatoanterior), decreasing = TRUE)),2))

## Deal with
cbind(round(prop.table(sort(table(socialPer$lidadiretamente), decreasing = TRUE)),2))

## Where deal with
cbind(round(prop.table(sort(table(socialPer$lida.onde), decreasing = TRUE)),2))


# Scale analysis ---

# Full scale
fullScale  <- socialPer[,34:71]

# descriptives
describe(fullScale)

# alpha
cronbach  <- alpha(fullScale) # Cronbach's alpha = .87

# EFA ----
## All items ----

## KMO
KMO(fullScale) # KMO = .92

# Barlett test of homogeneity # OK
bartlett.test(fullScale) 

# Defining factors
fa.parallel(fullScale, fm="minres", fa="both", ylabel="Eigenvalues") # yields 2 factors

# Factor analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut=0.3)

# Diagram
fa.diagram(faAll)

# RESULTADOS #
# Sem fator  9, 13, 18, 25, 32
# Um fatores e com cargas baixas   36
# MR1  : 1,2,3,4,5,6,7,8,10,15,17,20,22,30,
# MR2  : 11,12,-14,16,-19,21,23,24,26,27,-28,-29,-31,-33,-34,-35, 37,38,39

# Recode negative items
#for (i in c(14,19,28,29,31,33,34,35)){
#    fullScale[,i]   <-  Recode(fullScale[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")                         
#}

# Remove items with low loadings
shortScale  <- fullScale[, -c(9,13,18,25,32,36)]

faShort   <-  fa.poly(shortScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort, digits=2, cut=0.3)

## CFA - Confirmatory factor analysis ---

cfa <- bfactor(shortScale, c(2,2,2,2,2,2,2,2,2,1,1,1,2,1,2,1,2,1,2,1,1,1,1,1,1,2,1,1,1,2,1,1))

### Summary
summary(cfa)

### Coefficients
coef(cfa)
