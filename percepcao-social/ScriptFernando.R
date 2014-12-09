faAll <- fa.poly(fullScale, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut=0.4)

fullTeste <- fullScale
fullTeste[,14] <- recode(fullTeste[ , 14], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,19] <- recode(fullTeste[ , 19], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,28] <- recode(fullTeste[ , 28], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,29] <- recode(fullTeste[ , 29], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,32] <- recode(fullTeste[ , 32], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,33] <- recode(fullTeste[ , 33], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,34] <- recode(fullTeste[ , 34], "5=1 ; 4=2 ; 2=4; 1=5")
fullTeste[,35] <- recode(fullTeste[ , 35], "5=1 ; 4=2 ; 2=4; 1=5")

faAll <- fa.poly(fullTeste, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faAll, digits=2, cut=0.4)

shortScale3  <- fullTeste[, -c(9,11,12,13,18,21,22,25,30,31,36)]
shortScale3  <- shortScale3[, -c(15)]


faShort2   <-  fa.poly(shortScale3, nfactors = 2, rotate = "oblimin", fm="minres")
print.psych(faShort2, digits=2, cut=0.4)
fa.parallel(shortScale3, fm="minres", fa="both", ylabel="Eigenvalues")

scale1 <- shortScale3[ , c(1,2,3,4,5,6,7,8,9,11,13)]
faShort3   <-  fa.poly(scale1, nfactors = 1, rotate = "oblimin", fm="minres")
print.psych(faShort3, digits=2, cut=0.4)
fa.parallel(scale1, fm="minres", fa="both", ylabel="Eigenvalues")

scale2 <- shortScale3[ , -c(1,2,3,4,5,6,7,8,9,11,13)]
faShort4   <-  fa.poly(scale2, nfactors = 1, rotate = "oblimin", fm="minres")
print.psych(faShort4, digits=2, cut=0.4)
fa.parallel(scale2, fm="minres", fa="both", ylabel="Eigenvalues")

tri_1<-grm(scale1)

tri_2<-ltm(scale2~z1)

summary(tri_1)

for (i in 1:16) {
  scale2[,i] <- recode(scale2[,i], "1:2=0; 3:5=1")
}