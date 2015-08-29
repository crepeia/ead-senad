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
questions  <- read.csv("piloto/percepcaosocial_questions.csv", header = FALSE)

# Import dataframe
socialPer  <- read.csv("piloto/percepcaosocial_df.csv", stringsAsFactors = FALSE)

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
screePlot <- data.frame(n = 1:42, fa = faParalel$pc.values, sim = faParalel$pc.sim)
# Select just the first 10 factors
screePlot <- screePlot[1:10,]

## Create ggplot ScreePlot
ggplot(screePlot, aes(y = fa, x=1:10, color ="Dados")) + geom_line(linetype = 1) + xlim(1,10) + geom_line(aes(y = screePlot$sim, color = "1"), linetype=4) + theme_bw() + xlab("Fatores") + ylab("Autovalores") + theme(legend.position=c(.85,.9), legend.title = element_text(size = 14), legend.text = element_text(size = 12), axis.title =element_text(size = 12)) + scale_colour_discrete(name = "Legenda", labels=c("Dados simulados","Dados da amostra")) + scale_x_continuous(breaks=c(1,3,5,7,9))

## Factor Analysis
faAll <- fa(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
plot(faAll)
plot.psych(faAll)

### Análise de componentes principais com sem rotação / com rotação oblimin
#### 1 Tentativa

## Reverse code items according to Theory - I commented this line. The loop is originally from other script.
#for (i in c(14,19,28,29,32,33,34,35)) {
#  aScale[,i] <- recode(aScale[,i], "1=5;2=4;4=2;5=1")
#}

###########################
# EFA
###########################

# FIRST EFA --------
# Factor analysis using polychoric correlations
polyAll <- polychoric(fullScale)

# Unrotated PCA
faAll <- principal(polyAll$rho, nfactors = 2)
print.psych(faAll, digits=2, cut= .4)

# Rotated PCA
faAll_rot <- principal(polyAll$rho, nfactors = 2, rotate="oblimin")
print.psych(faAll_rot, digits=2, cut= .4)
# Results - Items 10,20,24,34,36,38 need to be removed due to small loadings < (.4).

#### 2 Tentativa sem rotação / com rotação oblimin
# SECOND EFA -------
# Remove items with low loadings detected in previous step.
shortScale  <- fullScale[, -c(10,20,24,34,36,38)]
polyShort <- polychoric(shortScale)
# Unrotated PCA
faShortScale <- principal(polyShort$rho, nfactors = 2)
print.psych(faShortScale, digits=2, cut= .4)
# Rotated PCA
faShortScale_rot <- principal(polyShort$rho, nfactors = 2, rotate="oblimin")
print.psych(faShortScale_rot, digits=2, cut= .4)

### Alfa de Cronbach
factorOne <- shortScale[, c("ps001", "ps002", "ps003", "ps004", "ps005", "ps006", "ps007", "ps008", "ps011", "ps014", "ps016", "ps018", "ps019", "ps022", "ps025", "ps028", "ps033", "ps039", "ps042")]
factorTwo <- shortScale[, c("ps009", "ps012", "ps013", "ps015", "ps017", "ps021", "ps023", "ps026", "ps027", "ps029", "ps030", "ps031", "ps032", "ps035", "ps037", "ps040", "ps041")]

alpha(factorOne, check.keys = TRUE)
alpha(factorTwo, check.keys = TRUE)


