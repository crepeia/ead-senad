# Libraries ----
library(car) # Function Recode
library(psych) # Function Describe
library(mirt)

# Import data ----

## Import dataframe
praticasPro  <- read.csv("praticasprofissionais_df.csv")

## Summing scales to remove NA's
praticasPro$scaleSum  <- rowSums(praticasPro[,32:68])
## Subset completed observations and consented participation
praticasPro  <- subset(praticasPro, subset=praticasPro$termo=="Sim" & praticasPro$estado=="Finalizadas" & !is.na(praticasPro$scaleSum))

# Demographics
## Age
# Demographics
## Age
### Clean data
praticasPro$idade  <- as.numeric(as.character(praticasPro$idade))
praticasPro$idade[praticasPro$idade < 18 | praticasPro$idade > 68 ]  <- NA


### Descriptives
summary(praticasPro$idade) # all
by(praticasPro$idade, praticasPro$sexo, describe) #by sex

## Sex
cbind(round(prop.table(sort(table(praticasPro$sexo), decreasing = TRUE)),2))

## Degree
cbind(round(prop.table(sort(table(praticasPro$escolaridade), decreasing = TRUE)),2))

## Marital Staus
cbind(round(prop.table(sort(table(praticasPro$estadocivil), decreasing = TRUE)),2))

## Education
#cbind(round(prop.table(table(praticasPro$formacao)),2)) # Broken, needs manual recoding

## Ocupação
#cbind(round(prop.table(table(praticasPro$ocupacao)),2)) # Broken, needs manual recoding

## Time  working
timeWorking  <- as.numeric(as.character(praticasPro$tempodeservico))
timeWorking[timeWorking > 59]  <- NA
summary(timeWorking)

## Religion 
cbind(round(prop.table(sort(table(praticasPro$religiao), decreasing = TRUE)),2))

## Contact 
cbind(round(prop.table(sort(table(praticasPro$contatoanterior), decreasing = TRUE)),2))

## Deal with
cbind(round(prop.table(sort(table(praticasPro$lidadiretamente), decreasing = TRUE)),2))

## Where deal with
cbind(round(prop.table(sort(table(praticasPro$lida.onde), decreasing = TRUE)),2))

# Scale analysis ---
# Full scale
fullScale  <- praticasPro[,32:68]

# descriptives
describe(fullScale)

# correlations
round(cor(fullScale, method="kendal", use="complete.obs"),2) # kendall correlation coef
cor.plot(cor(fullScale, method="kendal", use="complete.obs"), numbers= TRUE)

# alpha
cronbach  <- alpha(fullScale)
cronbach

# EFA ----

## All items ----

## KMO
KMO(fullScale)

# Barlett test of homogeneity
bartlett.test(fullScale)

# Defining factors
fa.parallel(fullScale, fm="minres", fa="both", ylabel="Eigenvalues") # yields 4 components and 4 factors
VSS(fullScale, rotate="none") # VSS = 2; MAP = 4 factors

# Factor Analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
faAll$fa

# Diagram
fa.diagram(faAll)


# Items per factor #
# MR1  : 9,10,11,12,14,15,16,19,20,21,22,26,27,29,30,31,32,-35,-36
# MR2  : -1,2,3,4,5,6,7,-8,13,17,-18,23,-24,-25,-28,-33,-34,37

# Recode negative items
for (i in c(1,8,18,24,25,28,33,34,35,36)){
  fullScale[,i]   <-  Recode(fullScale[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")                         
}

# Factor Analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
faAll$fa

# Diagram
fa.diagram(faAll)


# CFA ---- Not implemented yet.
### Exploratory factor analysis
### Bifactor Model
library(mirt)
factors  <- c(2,2,2,2,2,2,2,2,1,1,1,1,2,1,1,1,2,2,1,1,1,1,2,2,2,1,1,2,1,1,1,1,2,2,1,1,2) # based on efa scores
mbi  <- bfactor(fullScale, factors, verbose = FALSE)
summary(mbi)
residuals(mbi)
