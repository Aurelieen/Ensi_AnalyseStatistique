# Exercice 3 - Question 1
# Simulation d'une loi géométrique et vérification des intervalles de confiance

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

simule_geometriques(3,5,0.1)

