library(ggplot2)
library(markovchain)
# Importez votre s?quence sous forme de vecteur.
Hello les gars !!
# Soit depuis excel, soit en la tapant directement dans R comme ici.
test1 = c("a","a","a","a","b","b","a","b","c","c","d","d","c","a","b","d","b","b")
test2 = c("b","b","c","a","c","a","b","b","c","c","b",NA,NA,NA,NA,NA,NA,NA)
test3 = c("b","d","c","d","d","d","a","b","d","c","d","d","c","a","b","d",NA,NA)
test4 = c("b","d","d","a","d","a","d","b","d","c","b",NA,NA,NA,NA,NA,NA,NA)
test5 = c("c","c","a","a","c","b","c","b","c","c","d","d","c","a","b","d","b","b")
test6 = c("c","b","c","a","c","c","c","b","c","c","b",NA,NA,NA,NA,NA,NA,NA)
# Pour que le code fonctionne au mieux, compl?tez les s?quences les plus courtes avec des NA pour faire des s?quences de m?me longueur.

#On compte le nombre d'occurences de chaque comportement pour chaque exp?rience
compte1 = cbind(as.data.frame(table(test1)),rep("R?plica 1",nrow(table(test1))),rep("Exp?rience 1",nrow(table(test1))))
colnames(compte1) = c("Etho","Freq","Rep","Exp")
compte2 = cbind(as.data.frame(table(test2)),rep("R?plica 2",nrow(table(test2))),rep("Exp?rience 1",nrow(table(test2))))
colnames(compte2) = c("Etho","Freq","Rep","Exp")
compte3 = cbind(as.data.frame(table(test3)),rep("R?plica 1",nrow(table(test3))),rep("Exp?rience 2",nrow(table(test3))))
colnames(compte3) = c("Etho","Freq","Rep","Exp")
compte4 = cbind(as.data.frame(table(test4)),rep("R?plica 2",nrow(table(test4))),rep("Exp?rience 2",nrow(table(test4))))
colnames(compte4) = c("Etho","Freq","Rep","Exp")
compte5 = cbind(as.data.frame(table(test5)),rep("R?plica 1",nrow(table(test5))),rep("Exp?rience 3",nrow(table(test5))))
colnames(compte5) = c("Etho","Freq","Rep","Exp")
compte6 = cbind(as.data.frame(table(test6)),rep("R?plica 2",nrow(table(test6))),rep("Exp?rience 3",nrow(table(test6))))
colnames(compte6) = c("Etho","Freq","Rep","Exp")
# Jeu de donn?es au complet
dataset = rbind(compte1,compte2,compte3,compte4,compte5,compte6)

#Permet de voir les occurences par exp?rience ou par r?plica, mais pas les 2 en m?me temps
ggplot(dataset, aes(x = Etho, y = Freq, fill = Exp)) +
  geom_col(position = "dodge") + ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")

#Graphique complet
p <- ggplot(data = dataset, aes(x = Etho, y = Freq, fill = Rep)) + scale_fill_grey()
p <- p + geom_col(width = 1, position = position_dodge2(width = 0.9, preserve = "single"))
p <- p + facet_grid(. ~ Exp)+ ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
p


# On cr?e la fonction markov1 qui va g?n?rer la matrice de transition pour nous.

markov1 <- function (seq){ # On donne un nom ? la fonction, on nomme les arguments dont elle a besoin
  seq = seq[!is.na(seq)]# On retire les NA
  l = length(unique(seq)) # Nombre de comportements uniques dans la s?quence (taille de l'?thogramme) =l
  m = matrix(nrow = l, ncol = l) # On cr?e une matrice vide lxl
  rownames(m)= unique(seq)[order(unique(seq))] # On nomme les lignes et colonnes avec les comportements
  colnames(m)= unique(seq)[order(unique(seq))] # order permet de les ranger par ordre alphab?tique
  
  transition <-function(x,y){ # On cr?e une fonction interm?diaire qui permettra de calculer la valeur de chaque case
    a = 0 # Un compteur qui d?bute ? Z?ro.
    previous = 0 # La valeur de l'?l?ment pr?c?dent dans la s?quence, au d?part il n'y en a pas
    for (i in seq) { # Boucle en for. Pour tout ?l?ment i de la s?quence seq (seq = argument donn? ? la fonction)
      if (i == x & previous == y) { # Si i = x et l'?l?ment qui le pr?c?de = y
        a = a+1 # Alors on augmente le compteur de 1
      }
      previous = i # L'?l?ment i devient l'?l?ment pr?c?dent et on relance la boucle avec le i suivant.
    }
    return(a) # Quand la s?quence est termin?e, on sauve la valeur du compteur.
  }

# Maintenant on va remplir la matrice vide ? l'aide de la fonction interm?diaire, case par case.

  for (row in 1:nrow(m)){ # Pour chaque ligne de la matrice.
    for (col in 1:ncol(m)) { # Et pour chaque colonne,
      m[row,col]=transition(rownames(m)[row],colnames(m)[col]) # L'?l?ment correspondant ? la ligne et ? la colonne  = r?sultat de la fonction interm?diaire pour les noms de ligne et de colonne correspondant
    }
  }

  return(m) # On sauve la matrice
}

transition<-markov1(test1) # Il n'y a plus qu'? appliquer la fonction sur la s?quence !

# Si on veut obtenir la fr?quence des transitions en %
markov1pourcent <- function (m){ # On cr?e une fonction qui va transformer la matrice que nous venons de cr?er
  m2 = m # On cr?e une matrice m2 copie de la matrice m
  for (row in 1:nrow(m2)){ # Pour chaque ligne de la matrice m2
    for (col in 1:ncol(m2)) { # Et pour chaque colonne,
      m2[row,col]=m[row,col]/sum(m[,col]) # On divise la valeur correspondante dans la matrice m par la somme de la colonne
      }
  }
  m2[is.na(m2)] <- 0 # On remplace les NA par des z?ros
  return (m2) # On sauve m2
}

transitionpourcent <- markov1pourcent(transition) # Utilisons la fonction sur la matrice que nous venons de cr?er.

# Tra?ons le diagramme de flux avec le package markovchain
library(markovchain)

#Transformation de la matrice au format cha?ne de Markov
markov <- new('markovchain',
               transitionMatrix = t(transitionpourcent), # On doit prendre la transpos?e
               states = c("a","b","c","d"))# On donne le nom des comportements observ?s dans l'exp?rience

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
