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

# Affichage de lhistogramme

barplot(table(groupe1))


# Groupe2

groupe2<-read.table("groupe2.txt")[,1]
groupe2 <- sort(groupe2)

n<-length(groupe2)

# Indicateurs statistiques
summary(groupe2)

# Moyenne, variance et écart-type empiriques (sans biais)
mean(groupe2)
var(groupe2)
sd(groupe2)

# Affichage de l'histogramme

barplot(table(groupe2))

