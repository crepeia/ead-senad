# Libraries ----
library(car)   # Function Recode
library(psych) # Function Describe
library(mirt)  # Function bfactor - mirt IRT 

# Import data ----

# Questions
questions  <- read.csv("percepcaosocial_questions.csv")
questionsLabels  <- as.vector(questions[1:39,]); rm(questions)

# Analysis ----
## Import dataframe
socialPer  <- read.csv("atitudesEducadores_df.csv", na.strings = "-")
socialPer  <- socialPer[, -1]


## Convert in character
socialPer[,32:70] <- sapply(socialPer[,32:70], factor, levels = c("Discordo totalmente", "Discordo", "Nem discordo, nem concordo", "Concordo", "Concordo totalmente")) 

# Recode items into numeric
for (i in 32:70){
  socialPer[,i]   <-  Recode(socialPer[,i], "'Discordo totalmente'=1 ; 'Discordo'=2 ; 'Nem discordo, nem concordo' = 3; 'Concordo' = 4; 'Concordo totalmente' = 5; else = NA")                         
}

## Summing scales to remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,32:70])
## Subset completed observations and consented participation
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & !is.na(socialPer$scaleSum))

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
fullScale  <- socialPer[,32:70]

# descriptives
describe(fullScale)

# alpha
alpha(fullScale) # Cronbach's alpha = .87

# EFA ----
## All items ----

## KMO
KMO(fullScale) # KMO = .89

# Barlett test of homogeneity # OK
bartlett.test(fullScale)

# Defining factors
fa.parallel(fullScale, fm="minres", fa="both", ylabel="Eigenvalues") # yields 2 factors

# Factor analysis using polychoric correlations
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2)

# Diagram
fa.diagram(faAll)

# RESULTADOS #
# Sem fator: 9,11,12,13,21,22,25,30,31,36

# Recode negative items
#for (i in c(14,19,28,29,31,33,34,35)){
#    fullScale[,i]   <-  Recode(fullScale[,i], "5=1 ; 4=2 ; 3 = 3; 2 = 4; 1 = 5; else = NA")                         
#}

# Remove items with low loadings
shortScale  <- fullScale[, -c(9,11,12,13,18,21,22,25,30,31,36)]

cbind(names(shortScale))

# EFA with shortScale
faShort   <-  fa.poly(shortScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort, digits=2, cut=0.4)

# EFA with shortScale version 2
shortScale2  <- shortScale[, -c(15)]

faShort2   <-  fa.poly(shortScale2, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort2, digits=2, cut=0.4)

# Cronbach's alpha
alpha(shortScale2)

## CFA - Confirmatory factor analysis ---
cfa <- bfactor(shortScale2, c(2,2,2,2,2,2,2,2,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

summary(cfa)

cfa1f <- bfactor(shortScale2)

itemplot(cfa, 2, shiny = TRUE)
itemplot(cfa, 3, type = 'infotrace')

itemplot(cfa, 1, type = "trace", drop.zeros = TRUE, shiny = TRUE)

### Summary
summary(cfa)

### Coefficients
coef(cfa)


# Factor 1 - feelings
feelingsF1 <- shortScale2[, c("ps001","ps002","ps003","ps004","ps005","ps006","ps007","ps008","ps010","ps015","ps017")]
cognitionF2 <- shortScale2[, c("ps014", "ps016", "ps019", "ps023", "ps024", "ps026", "ps027","ps028", "ps029","ps032","ps033","ps034","ps035","ps037","ps038","ps039")]

modF1  <- mirt(feelingsF1, 1, itemtype="graded")


plot(modF1, type= 'trace')

for (i in 1:11){
itemplot(modF1, i)
}
