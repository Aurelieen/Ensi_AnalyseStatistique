# Partie 1 - Question 4
# Estimations graphiques de la loi binomiale négative

# Simulation d'un grand jeu de données de rnbinom(nb_mesures, r, p)
p_r = 4
p_p = 0.4

donnees = rnbinom(10000, p_r, p_p) + p_r
donnees_ord = sort(donnees)

# Fonction g(x) expérimentale suggérée dans le compte-rendu
g <- function(x) {
  if (length(x[x == (x+1)]) > 0) {
    return(x * (length(x[x == x]) / length(x[x == (x+1)])))
  }
}

# Dessin du nuage de points (x, g(x))
graphe_probabilites <- function(echantillon, r, p) {
  table_d = repartition(echantillon)
  plot(echantillon, g(echantillon))
  
  # Application de la régression linéaire
  abs = echantillon
  ord = g(echantillon)
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
pg2
pg3