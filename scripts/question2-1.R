Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Groupe1

groupe1<-read.table("groupe1.txt")[,1]
groupe1 <- sort(groupe1)

n<-length(groupe1)
# Indicateurs statistiques
summary(groupe1)

# Moyenne, variance et écart-type empiriques (sans biais)
mean(groupe1)
var(groupe1)
sd(groupe1)
coef_var_g1 = sd(groupe1) / mean(groupe1)
coef_var_g1

# Affichage de lhistogramme
barplot(table(groupe1), xlab="Nombre de fixations pour la lecture des textes", ylab="Nombre de personnes pour ce nombre de fixations")
title("Diagramme en colonnes du nombre de fixations pour le groupe 1", col.main="chartreuse4")


# Groupe2

groupe2<-read.table("groupe2.txt")[,1]
groupe2 <- sort(groupe2)

n<-length(groupe2)
median(groupe2)
# Indicateurs statistiques
summary(groupe2)

# Moyenne, variance et écart-type empiriques (sans biais)
mean(groupe2)
var(groupe2)
sd(groupe2)
coef_var_g2 = sd(groupe2) / mean(groupe2)
coef_var_g2

# Affichage de lhistogramme

barplot(table(groupe2), xlab="Nombre de fixations pour la lecture des textes", ylab="Nombre de personnes pour ce nombre de fixations")
title("Diagramme en colonnes du nombre de fixations pour le groupe 2", col.main="chartreuse4")

Mode(groupe1)