#Estimateur

groupe1<-read.table("groupe1.txt")[,1]
groupe2<-read.table("groupe2.txt")[,1]

# Méthode des moments pour trouver l'estimateur de p

estimateur_Geo <- function(echantillon)
{
  Xn <- mean(echantillon)
  Sn <- sd(echantillon)
  
  pn <- 1/Xn
  
  return(pn)
}

# Méthode des moments pour trouver les estimateurs de p et de r

estimateur_BN <- function(echantillon)
{
  Xn <- mean(echantillon)
  Sn <- sd(echantillon)
  
  rn <- Xn^2 / (Xn + Sn^2)
  rn <- round(rn, digit=0)
  
  pn <- rn/Xn
  
  return(list(pn, rn))
}

#On approxime groupe1 à une loi géométrique
estimateur_Geo(groupe1)
#On approxime groupe1 et groupe2 à une loi binomiale négative
estimateur_BN(groupe1)
estimateur_BN(groupe2)