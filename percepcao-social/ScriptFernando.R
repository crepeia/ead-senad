
# EFA with full scale
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
## Print EFA restults
print.psych(faAll, digits=2, cut=0.4)

# Recode negative items
fullScale <- fullScale
fullScale[,14] <- recode(fullScale[ , 14], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,19] <- recode(fullScale[ , 19], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,28] <- recode(fullScale[ , 28], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,29] <- recode(fullScale[ , 29], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,32] <- recode(fullScale[ , 32], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,33] <- recode(fullScale[ , 33], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,34] <- recode(fullScale[ , 34], "5=1 ; 4=2 ; 2=4; 1=5")
fullScale[,35] <- recode(fullScale[ , 35], "5=1 ; 4=2 ; 2=4; 1=5")

# EFA with reversed items
faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut=0.4)

# Remove items
shortScale3  <- fullScale[, -c(9,11,12,13,15,18,21,22,25,30,31,36)]


# 
faShort2   <-  fa.poly(shortScale3, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort2, digits=2, cut=0.4)
fa.parallel(shortScale3, fm="minres", fa="both", ylabel="Eigenvalues")

scale1 <- shortScale3[ , c(1,2,3,4,5,6,7,8,9,11,13)]
faShort3   <-  fa.poly(scale1, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort3, digits=2, cut=0.4)
fa.parallel(scale1, fm="minres", fa="both", ylabel="Eigenvalues")

scale2 <- shortScale3[ , -c(1,2,3,4,5,6,7,8,9,11,13)]
faShort4   <-  fa.poly(scale2, nfactors = 1, rotate = "oblimin", fm="minres")
print.psych(faShort4, digits=2, cut=0.4)
fa.parallel(scale2, fm="minres", fa="both", ylabel="Eigenvalues")

tri_1<-grm(scale1)

tri_2<-grm(scale2)

summary(tri_1) 

# Recode variables into binary
for (i in 1:16) {
  scale2[,i] <- recode(scale2[,i], "1:2=0; 3:5=1")
}

tri_2p <- ltm(scale2~z1)

faShort5   <-  fa.poly(scale2, nfactors = 1, rotate = "none", fm="minres")
print.psych(faShort4, digits=2, cut=0.4)
