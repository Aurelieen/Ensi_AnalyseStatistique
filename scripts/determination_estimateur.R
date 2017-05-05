#Estimateur

groupe1<-read.table("groupe1.txt")[,1]
groupe2<-read.table("groupe2.txt")[,1]

# Méthode des moments pour trouver l'estimateur de p

estimateur_Geo <- function(echantillon)
{
  Xn <- mean(echantillon)
  Sn <- sd(echantillon)
  
  pn <- 1/Xn
  
#Intervalle de confiance pour p (cf question 1.2)
  n <- length(echantillon) 
  l <- 1.96^2 * pn/n
  p1 = pn - 0.5*l*(pn^2)*(1+sqrt(1+(4*(1-pn))/(l*pn)))
  p2 = pn - 0.5*l*(pn^2)*(1-sqrt(1+(4*(1-pn))/(l*pn)))

  print(list(pn, p1, p2))
  return(list(pn, p1, p2))
}

# Méthode des moments pour trouver les estimateurs de p et de r

estimateur_BN <- function(echantillon)
{
  Xn <- mean(echantillon)
  Sn <- sd(echantillon)
  
  rn <- Xn^2 / (Xn + Sn^2)
  rn <- round(rn, digit=0)
  
  pn <- rn/Xn
  
  # print(Xn / (Xn + Sn^2))
  print(list(pn, rn))
  return(list(pn, rn))
}

#On approxime groupe1 à une loi géométrique
estimateur_Geo(groupe1)

#On approxime groupe1 et groupe2 à une loi binomiale négative
estimateur_BN(groupe1)
estimateur_BN(groupe2)



############################
# Vérification 
############################

# FONCTION
repartition <- function(echantillon) {
  table_d = c()
  
  for (i in 1:max(echantillon)) {
    table_d[i] = as.double(length(echantillon[echantillon == i]))
  }
  
  print(table_d)
  return(table_d)
}

# FONCTION
g <- function(echantillon, table_d) {
  table_g = c()
  ensemble_i= c()
  
  for (i in 1:max(echantillon)) {
    if (!is.na(table_d[i + 1]) & table_d[i + 1] > 0) {
      table_g = c(table_g, i * table_d[i] / table_d[i + 1])
      ensemble_i = c(ensemble_i, i)
    }
  }
  
  return(list(table_g, ensemble_i))
}

# Dessin du nuage de points (x, g(x))
graphe_probabilites <- function(echantillon, r, p) {
  table_d = repartition(echantillon)
  axes = g(echantillon, table_d)
  
  table_g = head(axes[[1]], -15)
  ensemble_i = head(axes[[2]], -15)
  
  plot(ensemble_i, table_g, xlab="Données x de de loi BN(5, 0.2533)", ylab="g(x)")
  title("Vérification du paramètre p par simulation", col.main="chartreuse4")
  
  # Application de la régression linéaire
  abs = ensemble_i
  ord = table_g
  reg = lm(ord~abs)
  lines(abs, fitted.values(reg))
  
  # Estimation de pg2 et pg3
  a = reg$coefficients[2]
  b = reg$coefficients[1]
  
  pg2 = 1 - (1/a)
  pg3 = 1 - (1 - r)/b
  
  # Renvoi des résultats
  return(list(pg2, pg3))
}

graphe_probabilites(groupe1, 5, 0.2533172)