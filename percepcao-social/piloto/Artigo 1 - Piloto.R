###########################
# First Paper -----------
###########################

# Load packages

library(car)     # Function Recode
library(psych)   # Function Describe, fa.parallel, fa.poly, plot
library(ggplot2) # For better charts

# ================
# Import data ----
# ================

# Scale questions 
questions  <- read.csv("percepcaosocial_questions.csv", header = FALSE)

# Import dataframe
socialPer  <- read.csv("percepcaosocial_df.csv")

# ================
# Analyses    ----
# ================

# Atrition ----

## 1. How many people did not agree to participate?

## Table - consented vs. not-consented
table(!is.na(socialPer$termo=="Sim"))

# 2. How many people did not answer all scale questions?

## Recode - Sum items to create a general score and then remove NA's
socialPer$scaleSum  <- rowSums(socialPer[,24:65])

## Table - Completed vs not-completed.
table(is.na(socialPer$scaleSum))

## Subset data selecting those who agreed and answered all scale items 
socialPer  <- subset(socialPer, subset=socialPer$termo=="Sim" & !is.na(socialPer$scaleSum))

# ---------------------
# Demographics     ----
# ---------------------

# Age

## Remove answers with character
socialPer$idade <- gsub("[A-z]", "", socialPer$idade)

## Convert into numeric
socialPer$idade  <- as.numeric(socialPer$idade)

### Analyses
summary(socialPer$idade) # all
sd(socialPer$idade, na.rm = TRUE)
by(socialPer$idade, socialPer$sexo, describe) # by sex

## Sex
cbind(round(prop.table(sort(table(socialPer$sexo), decreasing = TRUE)),2))

## Education
cbind(round(prop.table(sort(table(socialPer$escolaridade), decreasing = TRUE)),2))

## Married?
cbind(round(prop.table(sort(table(socialPer$estadocivil), decreasing = TRUE)),2))

## Time working
socialPer$tempodeservico <- gsub("[A-z]", "", socialPer$tempodeservico)
socialPer$tempodeservico <- as.numeric(socialPer$tempodeservico)
summary(socialPer$tempodeservico)

## Religion 
cbind(round(prop.table(sort(table(socialPer$religiao), decreasing = TRUE)),2))

## Contact with SUD users
cbind(round(prop.table(sort(table(socialPer$contato.tema), decreasing = TRUE)),2))

## Deal with SUD users
cbind(round(prop.table(sort(table(socialPer$lida.com), decreasing = TRUE)),2))

## Where deal with SUD users
cbind(round(prop.table(sort(table(socialPer$onde.lida.com), decreasing = TRUE)),2))

# ---------------------
# Scale analysis   ----
# ---------------------

# Select just scale items
fullScale  <- socialPer[ , 24:65]

# Exploratory Factor Analysis

## KMO
KMO(fullScale) # KMO = .82

# Bartlett Test # OK
bartlett.test(fullScale)

# Parallel Analysis
faParalel  <- fa.parallel(fullScale, fm="minres", fa="fa", ylabel="Eigenvalues")
# Create dataframe
screePlot <- data.frame(n = 1:42, fa = faParalel$fa.values, sim = faParalel$fa.sim)
# Select just the first 10 factors
screePlot <- screePlot[1:10,]

## Create ggplot ScreePlot
ggplot(screePlot, aes(y = fa, x=1:10, color ="Dados")) + geom_line(linetype = 1) + xlim(1,10) + geom_line(aes(y = screePlot$sim, color = "1"), linetype=4) + theme_bw() + xlab("Fatores") + ylab("Autovalores") + theme(legend.position=c(.85,.9), legend.title = element_text(size = 14), legend.text = element_text(size = 12), axis.title =element_text(size = 12)) + scale_colour_discrete(name = "Legenda", labels=c("Dados simulados","Dados da amostra")) + scale_x_continuous(breaks=c(1,3,5,7,9))

## Factor Analysis
faAll <- fa(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
plot(faAll)
plot.psych(faAll, )
