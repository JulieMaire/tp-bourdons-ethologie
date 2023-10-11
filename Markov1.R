library(ggplot2)
library(markovchain)
# Importez votre séquence sous forme de vecteur.

# Soit depuis excel, soit en la tapant directement dans R comme ici.
test1 = c("a","a","a","a","b","b","a","b","c","c","d","d","c","a","b","d","b","b")
test2 = c("b","b","c","a","c","a","b","b","c","c","b",NA,NA,NA,NA,NA,NA,NA)
test3 = c("b","d","c","d","d","d","a","b","d","c","d","d","c","a","b","d",NA,NA)
test4 = c("b","d","d","a","d","a","d","b","d","c","b",NA,NA,NA,NA,NA,NA,NA)
test5 = c("c","c","a","a","c","b","c","b","c","c","d","d","c","a","b","d","b","b")
test6 = c("c","b","c","a","c","c","c","b","c","c","b",NA,NA,NA,NA,NA,NA,NA)
# Pour que le code fonctionne au mieux, complétez les séquences les plus courtes avec des NA pour faire des séquences de même longueur.

#On compte le nombre d'occurences de chaque comportement pour chaque expérience
compte1 = cbind(as.data.frame(table(test1)),rep("Réplica 1",nrow(table(test1))),rep("Expérience 1",nrow(table(test1))))
colnames(compte1) = c("Etho","Freq","Rep","Exp")
compte2 = cbind(as.data.frame(table(test2)),rep("Réplica 2",nrow(table(test2))),rep("Expérience 1",nrow(table(test2))))
colnames(compte2) = c("Etho","Freq","Rep","Exp")
compte3 = cbind(as.data.frame(table(test3)),rep("Réplica 1",nrow(table(test3))),rep("Expérience 2",nrow(table(test3))))
colnames(compte3) = c("Etho","Freq","Rep","Exp")
compte4 = cbind(as.data.frame(table(test4)),rep("Réplica 2",nrow(table(test4))),rep("Expérience 2",nrow(table(test4))))
colnames(compte4) = c("Etho","Freq","Rep","Exp")
compte5 = cbind(as.data.frame(table(test5)),rep("Réplica 1",nrow(table(test5))),rep("Expérience 3",nrow(table(test5))))
colnames(compte5) = c("Etho","Freq","Rep","Exp")
compte6 = cbind(as.data.frame(table(test6)),rep("Réplica 2",nrow(table(test6))),rep("Expérience 3",nrow(table(test6))))
colnames(compte6) = c("Etho","Freq","Rep","Exp")
# Jeu de données au complet
dataset = rbind(compte1,compte2,compte3,compte4,compte5,compte6)

#Permet de voir les occurences par expérience ou par réplica, mais pas les 2 en même temps
ggplot(dataset, aes(x = Etho, y = Freq, fill = Exp)) +
  geom_col(position = "dodge") + ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")

#Graphique complet
p <- ggplot(data = dataset, aes(x = Etho, y = Freq, fill = Rep)) + scale_fill_grey()
p <- p + geom_col(width = 1, position = position_dodge2(width = 0.9, preserve = "single"))
p <- p + facet_grid(. ~ Exp)+ ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
p


# On crée la fonction markov1 qui va générer la matrice de transition pour nous.

markov1 <- function (seq){ # On donne un nom à la fonction, on nomme les arguments dont elle a besoin
  seq = seq[!is.na(seq)]# On retire les NA
  l = length(unique(seq)) # Nombre de comportements uniques dans la séquence (taille de l'éthogramme) =l
  m = matrix(nrow = l, ncol = l) # On crée une matrice vide lxl
  rownames(m)= unique(seq)[order(unique(seq))] # On nomme les lignes et colonnes avec les comportements
  colnames(m)= unique(seq)[order(unique(seq))] # order permet de les ranger par ordre alphabétique
  
  transition <-function(x,y){ # On crée une fonction intermédiaire qui permettra de calculer la valeur de chaque case
    a = 0 # Un compteur qui débute à Zéro.
    previous = 0 # La valeur de l'élément précédent dans la séquence, au départ il n'y en a pas
    for (i in seq) { # Boucle en for. Pour tout élément i de la séquence seq (seq = argument donné à la fonction)
      if (i == x & previous == y) { # Si i = x et l'élément qui le précède = y
        a = a+1 # Alors on augmente le compteur de 1
      }
      previous = i # L'élément i devient l'élément précédent et on relance la boucle avec le i suivant.
    }
    return(a) # Quand la séquence est terminée, on sauve la valeur du compteur.
  }

# Maintenant on va remplir la matrice vide à l'aide de la fonction intermédiaire, case par case.

  for (row in 1:nrow(m)){ # Pour chaque ligne de la matrice.
    for (col in 1:ncol(m)) { # Et pour chaque colonne,
      m[row,col]=transition(rownames(m)[row],colnames(m)[col]) # L'élément correspondant à la ligne et à la colonne  = résultat de la fonction intermédiaire pour les noms de ligne et de colonne correspondant
    }
  }

  return(m) # On sauve la matrice
}

transition<-markov1(test1) # Il n'y a plus qu'à appliquer la fonction sur la séquence !

# Si on veut obtenir la fréquence des transitions en %
markov1pourcent <- function (m){ # On crée une fonction qui va transformer la matrice que nous venons de créer
  m2 = m # On crée une matrice m2 copie de la matrice m
  for (row in 1:nrow(m2)){ # Pour chaque ligne de la matrice m2
    for (col in 1:ncol(m2)) { # Et pour chaque colonne,
      m2[row,col]=m[row,col]/sum(m[,col]) # On divise la valeur correspondante dans la matrice m par la somme de la colonne
      }
  }
  m2[is.na(m2)] <- 0 # On remplace les NA par des zéros
  return (m2) # On sauve m2
}

transitionpourcent <- markov1pourcent(transition) # Utilisons la fonction sur la matrice que nous venons de créer.

# Traçons le diagramme de flux avec le package markovchain
library(markovchain)

#Transformation de la matrice au format chaîne de Markov
markov <- new('markovchain',
               transitionMatrix = t(transitionpourcent), # On doit prendre la transposée
               states = c("a","b","c","d"))# On donne le nom des comportements observés dans l'expérience

#Le graphique
plot(markov,
     edge.arrow.size = 0.25, edge.arrow.width = 1,edge.color ="grey",edge.lty = 1,
     edge.label.cex = 1,edge.label.color = "black",
     vertex.color = "lightgreen", vertex.shape = "circle",vertex.size = 10*table(test1),
     vertex.frame.color= NA,
     vertex.label.color ="black",vertex.label.cex = 2.5
     )
# Plus d'options ici : https://igraph.org/r/doc/plot.common.html

#Histogramme

#Poids
poidsW = data.frame(poids = (rnorm(300, mean=5, sd=1)))
ggplot(data = poidsW, mapping = aes(x =poids)) + geom_histogram()
