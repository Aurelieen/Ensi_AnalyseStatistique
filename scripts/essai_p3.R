# Exercice 3

# Fonction simulant m échantillons de taille n de la loi géométrique de paramètre p

simule_geometriques <- function(m,n,p)
{
  # On créée un vecteur contenant les m échantillons
  echantillons <- matrix(nrow = m, ncol = n)
  
  for (k in (1:m))
  {
    # rgeom(n,p) simule un échantillon de taille n d'une variable aléatoire Y 
    # telle que Y+1 suit une loi géométrique de paramètre p
    y = rgeom(n,p)
    echantillons[k,] <- y+1
  }

  echantillons
}

#Q1 - Vérification des intervalles de confiance

# Fonction évaluant l'intervalle de confiance asymptotique de seuil alpha
# pour une échantillon donné suivant une loi géométrique

intervalle <- function(echantillon, alpha)
{
  n <- length(echantillon)
  # On évalue la moyenne empirique de l'echantillon et on en déduit une estimation du paramètre
  # de la loi que suivent les variables aléatoires
  Xn <- mean(echantillon)
  Pn <- 1/Xn
  # On calcule les valeurs nécessaires à l'évaluation de l'intervalle
  Ua <- qnorm(1 - alpha/2)
  inter <- Ua*sqrt((Xn-1)/(n*(Xn^3)))
  # On évalue l'intervalle
  b_inf <- Pn - inter
  b_sup <- Pn + inter
  IC <- c(b_inf,b_sup)
  IC
}

# Fonction simulant m échantillons de taille n de la loi géométrique de paramètre p, et calculant
# ensuite leur intervalle de confiance de seuil alpha pour Pn obtenu via une évaluation empirique. 
# Elle affiche ensuite le taux d'intervalles de confiance contenant p sur les m calculés.
validation_intervalles <- function(m,n,p,alpha)
{
  # Simulation des m échantillons de taille n et de paramètre p
  echantillons = simule_geometriques(m,n,p)
  # Validation des intervalles
  bons_intervalles <- 0
  for (k in (1:m))
  {
    # Calcul de l'interval de confiance
    IC = intervalle(echantillons[k,],alpha)
    # Vérification de la validité de l'intervalle
    if (p >= IC[1] & p <= IC[2])
    {
      bons_intervalles <- bons_intervalles + 1
    }
  }
  taux <- bons_intervalles/m
  cat(taux*100, "% des intervalles encadrent bien le paramètre p. Pourcentage attendu : ", (1 - alpha)*100, "%")

}

validation_intervalles(10000,1000,0.8,0.05)

# Q2 - Vérification de la loi faible des grands nombres

# Fonction simulant m échantillons de taille n de la loi géométrique de paramètre p, et calculant
# ensuite leur moyenne empirique. Elle affiche ensuite si le nombre de moyennes empiriques ayant 
# un écart inférieur à e avec l'espérance

validation_grands_nombres <- function(m,n,p,e)
{
  # Simulation des m échantillons de taille n et de paramètre p
  echantillons <- simule_geometriques(m,n,p)
  # Validation des moyennes empiriques
  esperance <- 1/p
  moyennes_justes <- 0
  for (k in (1:m))
  {
    moyenne_empirique = mean(echantillons[k,])
    if (abs(moyenne_empirique - esperance) < e)
    {
      moyennes_justes <- moyennes_justes + 1
    }
  }
  cat(moyennes_justes, "échantillons (de taille",n,") sur les", m ,"simulés ont un écart entre leur espérance et leur moyenne empirique inférieur à", e, "\n")
}

# Execution en faisant augmenter la valeur de n
validation_grands_nombres(100,500,0.7,0.05)
validation_grands_nombres(100,1000,0.7,0.05)
validation_grands_nombres(100,5000,0.7,0.05)
# On peut conclure de ces résultats que la moyenne empirique se rapproche bien de l'espérance
# quand n augmente, ce qui confirme le résultat de la loi faible des grands nombre

# Q3 - Validation du théorème central limite

validation_theoreme_central <- function(m,n,p)
{
  # Simulation des m échantillons de taille n et de paramètre p
  echantillons <- simule_geometriques(m,n,p)
  # Calcul des moyennes empiriques
  moyennes <- matrix(0,1,m)
  moy <- 1/p
  std <- sqrt(1-p)/p
  for (k in (1:m))
  {
    moyennes[k] <- mean(echantillons[k,])
  }
  # Affichage de l'histogramme à classes des moyennes empiriques ainsi que de la loi normale associée
  min <- min(moyennes)
  max <- max(moyennes)
  brk <- seq(min,max,length = 20)
  hist(moyennes,xlab = "", ylab = "Effectif", breaks = brk, main = "Histogramme des moyennes et loi normale associée")
  par(new=TRUE)
  plot(function(x) dnorm(x,moy,std),  moy-4*std, moy+4*std,xlab="Moyenne empirique de l'échantillon (loi géométrique)",ylab="",main = "",xaxt="n", yaxt="n", col = "red")
}

validation_theoreme_central(1000,1000,0.2)

# Pour un nombre d'échantillons suffisement grand (m = 1000), on voit que quand n augmente la loi de répartition
# des moyennes empiriques se rapproche d'une loi normale



