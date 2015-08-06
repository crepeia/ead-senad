library(car)      # Load packge - used function Recode
library(psych)    # Load packge - used for EFA
library(mirt)     # CFA
library(xtable)   # Load xtable package
library(ltm)

# Read data
attitudesDf  <- read.csv("atitudesEducadores_df.csv")

# Convert into scale items into numeric
for (i in 33:71) {
  attitudesDf[,i] <- recode(attitudesDf[,i], "'Discordo totalmente'=1;'Discordo'=2;'Nem discordo, nem concordo'=3;'Concordo'=4;'Concordo totalmente'=5")
  attitudesDf[,i] <- as.numeric(attitudesDf[,i])
}

## Create variable - sum scales to remove NA's
attitudesDf$scaleSum  <- rowSums(attitudesDf[,33:71])

attitudesDf  <- subset(attitudesDf, subset=attitudesDf$termo=="Sim")

attitudesDf$idade  <- as.numeric(as.character(attitudesDf$idade))
attitudesDf$idade[attitudesDf$idade < 18 | attitudesDf$idade > 68 ]  <- NA
summary(attitudesDf$idade) # all
by(attitudesDf$idade, attitudesDf$sexo, describe) # by sex

# Create dataFrame with attitudes scale
fullScale  <- attitudesDf[,33:71]
# Select items without NA's.
fullScale  <- fullScale[complete.cases(fullScale),]

## Reverse code items according to Theory
for (i in c(14,19,28,29,32,33,34,35)) {
  fullScale[,i] <- recode(fullScale[,i], "1=5;2=4;4=2;5=1")
}

# Describe items
describe(fullScale, skew=FALSE)

### Crobach's alfa
alfa<-alpha(fullScale)

Análise Fatorial
----------------------
  
  ### KMO - Adequação da amostra
  ```{r}
# Sample adequacy
KMO(fullScale)
```


### Esfericidade
```{r}
# Sphericity
bartlett.test(fullScale)
```

### Cattel's scree

# Plot Scree Plot
fa.parallel(fullScale, fm="minres", fa="fa", show.legend=FALSE) # yields 4 components


### EFA - Análise fatorial exploratória com dois fatores


faAll <- fa(fullScale, nfactors = 3, rotate = "none", fm="pa")
print.psych(faAll, digits=2, cut=0.3)

faAll_rot <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin")
print.psych(faAll_rot, digits=2, cut=0.3)

triAll <- grm(fullScale)


# RESULTADOS #

* Sem fator 9, 13, 18, 25
* Em dois fatores 12

#### Versão somente com itens com melhores cargas
```{r}
# Remove items with low loadings
shortScale  <- fullScale[, -c(9,11,12,13,15,18,21,22,25,30,31,36)]
faShort   <-  fa.poly(shortScale, nfactors = 2, rotate = "oblimin", fm="pa")
print.psych(faShort, digits=2, cut=0.3)

triShort <- grm(shortScale)



