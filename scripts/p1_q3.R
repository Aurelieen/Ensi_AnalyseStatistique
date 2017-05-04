# Exercice 1 - Question 3
# Graphe de probabilités pour la loi géométrique

# Récupération des données
# (!) Il faut se trouver dans le dossier des scripts
# (!) Sous RStudio, menu "Session > Set Working directory > To Source File Location"
groupe1 <- read.table("groupe1.txt")[,1]
groupe2 <- read.table("groupe2.txt")[,1]

# Tri des données
groupe1_ord <- sort(groupe1)
groupe2_ord <- sort(groupe2)

# Fonction pour tracer un graphe de probabilités
# par rapport à la loi géométrique (cas r = 1)
# PRECONDITIONS : <echantillon> est trié
graphe_probabilites <- function(echantillon)
{
  # Le graphe de probabilités est de la forme h[F(k)] alpha(p)g(k) + beta(p)
  # avec, dans le cas de la loi géométrique :
  #   -> h[F(k)] = ln(1 - F(k))
  #   -> alpha(p) = ln(1 - p)
  #   -> g(k) = k
  #   -> beta(p) = 0
  
  # On trace donc (g(k_i*), h(i/n))
  # NB : on évite le cas ln(0) indéfini en allant jusqu'à n - 1
  n = length(echantillon)
  plot(head(echantillon, -1), log(1 - seq(1:(n-1))/n))
  
  # Si on considère les points alignés, on peut essayer d'en déduire une pente
  # qui représentera le paramètre p. On trace la droite des moindres carrés. 
  abs = head(echantillon,  -1)
  ord = log(1 - seq(1:(n-1))/n)
  reg = lm(ord~abs)
  lines(abs, fitted.values(reg))
  
  # On estime et retourne un paramètre p
  # La pente de la droite des moindres carrés est ln(1 - p)
  pente = reg$coefficients[2]
  # TODO. Retourner la pente
}

graphe_probabilites(groupe1_ord)
# graphe_probabilites(groupe2_ord)

# Jeu de données simulé pour la mesure de qualité
nb_simulations = 1000;
simulations = rgeom(nb_simulations, 0.5)