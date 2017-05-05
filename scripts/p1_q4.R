# Partie 1 - Question 4
# Estimations graphiques de la loi binomiale négative

# Simulation d'un grand jeu de données de rnbinom(nb_mesures, r, p)
p_r = 4
p_p = 0.4

donnees = rnbinom(10000000, p_r, p_p) + p_r
donnees_ord = sort(donnees)

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
  
  table_g = head(axes[[1]], -length(axes[[1]])/5)
  ensemble_i = head(axes[[2]], -length(axes[[2]])/5)
  
  plot(ensemble_i, table_g, xlab="Données x de loi binomiale négative simulées", ylab="g(x)")
  title("Vérification de la méthode 1.4 par simulation", col.main="chartreuse4")
  
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

resultat = graphe_probabilites(donnees_ord, p_r, p_p)
pg2 = resultat[[1]][1]
pg3 = resultat[[2]][1]

# Coefficients
print(pg2)
print(pg3)