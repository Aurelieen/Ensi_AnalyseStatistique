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
# PRECONDITIONS : <groupe> est trié
graphe_probabilites <- function(groupe)
{
  # TODO
}  
