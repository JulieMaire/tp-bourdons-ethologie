library(ggplot2)
library(markovchain)
# Importez votre s?quence sous forme de vecteur.

# Soit depuis excel, soit en la tapant directement dans R comme ici.
# Comportements de la colonie.
test1 = c("M","M","B","B","B","B","B","B","B","B","P","M","B","P","B","B","P","B","M","B",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test2 = c("N","M","N","M","N","M","N","M","M","M","M","M","M","P","M","M","M","M","M","M","M","M","M","P","M","M","M","N","M","M","N","M","B","M","B","M","B","M","B","M","B","M","B","M","B","M","B","M","B","M","M","B","M","B","M","N","M","N","M","M","B","B","M","B","M","B","M","B","M","B","L","M","B","M","B","M","B","M","B","M","B","L","M","B","M","B","M","M","L",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test3 = c("M","D","M","D","M","M","P","P","M","P","M","B","P","M","P","M","M","M","N","M","B","N","P","M","N","M","N","M","N","P","M","N","M","N","M","N","M","N","L","M","N","L","P","M","N","M","N","M","N","M","N","M","N","M","N","M","N","M","M","N","M","N","M","N","C","M","N","M","N","P","M","N","P","M","N","M","N","M","N","P","M","N","P","M","N","P","N","M","N","M","N","M","N","M","N","L","M","N","L","M","N",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test4 = c("M","P","M","P","M","D","P","M","D","P","M","D","B","M","D","M","M","M","I","M","I","M","N","M","N","M","N","M","N","L","M","N","L","P","M","N","L","M","N","L","M","N","M","M","M","N","L","M","N","L","M","N","L","M","N","L","M","L","M","L","M","C","M","I","M","I","M","C","M","L","M","N","I","M","L","M","N","I","M","N","M","N","M","L","M","N","M","L","M","N","M","N","M","L","M","B","L","I","M","B","N","P","M","N","B","M","I","M","M","M","M","B",NA,NA,NA,NA,NA,NA,NA)
test5 = c("M","B","M","M","M","M","Po","M","M","P","M","M","M","P","M","M","M","M","P","M","M","L","M","L","M","M","M","M","L","M","M","M","N","M","M","M","L","M","M","M","C","M","M","M","C","M","M","M","N","Po","M","M","Po","M","Po","M","Po","M","N","Po","M","N","M","M","M","M","C","M","M","M","M","M","L","M","Po","M","M","M","Po",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test6 = c("B","M","D","M","M","I","M","Po","M","N","M","N","M","N","M","N","M","N","M","N","Po","M","I","M","N","I","M","N","I","Po","M","N","I","Po","M","N","I","Po","M","N","I","M","I","Po","M","N","I","M","N","I","M","N","I","M","N","I","M","N","I","M","N","I","Po","M","N","I","Po","M","N","I","M","N","I","Po","M","N","I","Po","M","N","I","M","N","I","M","N","I","M","N","I","T","Po","M","N","I","T","Po","M","I","T","M","I","T","M","I","T","M","M","M","M","N","M","N","M","N","Po","M","N","Po")
#Comportement du bourdon introduit.
test1A = c("M","M","M","M","M","I","I","I","P","M","M","M","M","I","I","I","I","M","M",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test2A = c("M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","I","I","I","M","M","M","M","M","M","M","M","M","M","M","M","M","M","I","I","M","M","I","I","I","P","I","P","P","P","P","P","P","P","P",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test3A = c("D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","M","M","M","N","N","N","N","N","N","N","N","N","N","N",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
test4A = c("M","M","M","M","M","M","M","M","I","M","M","M","M","N","N","N","N","N","N","N","N","N","N","M","M","M","I","I","I","I","M","I","I","I","I","I","I","I","I","I","I","M","M","M","M","I","I","I","I","I","M","I","N","N","N","N","N","N","N","N","N",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

# Pour que le code fonctionne au mieux, compl?tez les s?quences les plus courtes avec des NA pour faire des s?quences de m?me longueur.

#On compte le nombre d'occurences de chaque comportement pour chaque exp?rience
#Occurences des comportements de la colonie.
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
#Occurences des comportement du bourdon introduit.
compte1A = cbind(as.data.frame(table(test1A)),rep("Réplica 1",nrow(table(test1A))),rep("Expérience 1",nrow(table(test1A))))
colnames(compte1A) = c("Etho","Freq","Rep","Exp")
compte2A = cbind(as.data.frame(table(test2A)),rep("Réplica 2",nrow(table(test2A))),rep("Expérience 1",nrow(table(test2A))))
colnames(compte2A) = c("Etho","Freq","Rep","Exp")
compte3A = cbind(as.data.frame(table(test3A)),rep("Réplica 1",nrow(table(test3A))),rep("Expérience 2",nrow(table(test3A))))
colnames(compte3A) = c("Etho","Freq","Rep","Exp")
compte4A = cbind(as.data.frame(table(test4A)),rep("Réplica 2",nrow(table(test4A))),rep("Expérience 2",nrow(table(test4A))))
colnames(compte4A) = c("Etho","Freq","Rep","Exp")

# Jeu de donn?es au complet
dataset1 = rbind(compte1,compte2,compte3,compte4,compte5,compte6)
dataset2 = rbind(compte1A,compte2A,compte3A,compte4A)

#Permet de voir les occurences par exp?rience ou par r?plica, mais pas les 2 en m?me temps
ggplot(dataset1, aes(x = Etho, y = Freq, fill = Exp)) +
  geom_col(position = "dodge") + ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")
#Bourdon introduit
ggplot(dataset2, aes(x = Etho, y = Freq, fill = Exp)) +
  geom_col(position = "dodge") + ggtitle("Comportement du bourdon introduit") +
  labs(x ="Actes comportementaux", y = "Occurences")

#Graphique complet
p <- ggplot(data = dataset1, aes(x = Etho, y = Freq, fill = Rep)) + scale_fill_grey()
p <- p + geom_col(width = 1, position = position_dodge2(width = 0.9, preserve = "single"))
p <- p + facet_grid(. ~ Exp)+ ggtitle("Comportement de la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
p
#Bourdon introduit
p2 <- ggplot(data = dataset2, aes(x = Etho, y = Freq, fill = Rep)) + scale_fill_grey()
p2 <- p2 + geom_col(width = 1, position = position_dodge2(width = 0.9, preserve = "single"))
p2 <- p2 + facet_grid(. ~ Exp)+ ggtitle("Comportement du bourdon introduit dans la colonie") +
  labs(x ="Actes comportementaux", y = "Occurences")+theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
p2

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

transition<-markov1(test1A) # Il n'y a plus qu'? appliquer la fonction sur la s?quence !

# Si on veut obtenir la fr?quence des transitions en %
markovpourcent <- function (m){ # On cr?e une fonction qui va transformer la matrice que nous venons de cr?er
  m2 = m # On cr?e une matrice m2 copie de la matrice m
  for (row in 1:nrow(m2)){ # Pour chaque ligne de la matrice m2
    for (col in 1:ncol(m2)) { # Et pour chaque colonne,
      m2[row,col]=m[row,col]/sum(m[,col]) # On divise la valeur correspondante dans la matrice m par la somme de la colonne
      }
  }
  m2[is.na(m2)] <- 0 # On remplace les NA par des z?ros
  return (m2) # On sauve m2
}

transitionpourcent <- markovpourcent(transition) # Utilisons la fonction sur la matrice que nous venons de cr?er.

# Tra?ons le diagramme de flux avec le package markovchain
library(markovchain)

#Transformation de la matrice au format cha?ne de Markov
markov <- new('markovchain',
  transitionMatrix = t(transitionpourcent),
  states = c("M","I","P"))# On donne le nom des comportements observ?s dans l'exp?rience

#Le graphique
plot(markov,
     edge.arrow.size = 0.25, edge.arrow.width = 1,edge.color ="grey",edge.lty = 1,
     edge.label.cex = 1,edge.label.color = "black",
     vertex.color = "lightgreen", vertex.shape = "circle",vertex.size = 10*table(test1A),
     vertex.frame.color= NA,
     vertex.label.color ="black",vertex.label.cex = 2.5
     )
#Markov2
markov1 <- function (seq){ 
  seq = seq[!is.na(seq)]
  l = length(unique(seq)) 
  m = matrix(nrow = l, ncol = l) 
  rownames(m)= unique(seq)[order(unique(seq))] 
  colnames(m)= unique(seq)[order(unique(seq))] 
  
  transition <-function(x,y){ 
    a = 0 
    previous = 0 
    for (i in seq) { 
      if (i == x & previous == y) { 
        a = a+1 
      }
      previous = i 
    }
    return(a) 
  }
  for (row in 1:nrow(m)){ 
    for (col in 1:ncol(m)) { 
      m[row,col]=transition(rownames(m)[row],colnames(m)[col]) 
    }
  }
  return(m) 
}
transition<-markov1(test2A) 

markovpourcent <- function (m){ 
  m2 = m 
  for (row in 1:nrow(m2)){ 
    for (col in 1:ncol(m2)) { 
      m2[row,col]=m[row,col]/sum(m[,col]) 
    }
  }
  m2[is.na(m2)] <- 0 
  return (m2) 
}
transitionpourcent <- markovpourcent(transition) 

library(markovchain)
markov <- new('markovchain',
  transitionMatrix = t(transitionpourcent),
  states = c("M","I","P"))

#Le graphique
plot(markov,
  edge.arrow.size = 0.25, edge.arrow.width = 1,edge.color ="grey",edge.lty = 1,
  edge.label.cex = 1,edge.label.color = "black",
  vertex.color = "lightgreen", vertex.shape = "circle",vertex.size = 3*table(test2A),
  vertex.frame.color= NA,
  vertex.label.color ="black",vertex.label.cex = 2.5
)
#Markov3
markov1 <- function (seq){ 
  seq = seq[!is.na(seq)]
  l = length(unique(seq)) 
  m = matrix(nrow = l, ncol = l) 
  rownames(m)= unique(seq)[order(unique(seq))] 
  colnames(m)= unique(seq)[order(unique(seq))] 
  
  transition <-function(x,y){ 
    a = 0 
    previous = 0 
    for (i in seq) { 
      if (i == x & previous == y) { 
        a = a+1 
      }
      previous = i 
    }
    return(a) 
  }
  for (row in 1:nrow(m)){ 
    for (col in 1:ncol(m)) { 
      m[row,col]=transition(rownames(m)[row],colnames(m)[col]) 
    }
  }
  return(m) 
}
transition<-markov1(test3A) 

markovpourcent <- function (m){ 
  m2 = m 
  for (row in 1:nrow(m2)){ 
    for (col in 1:ncol(m2)) { 
      m2[row,col]=m[row,col]/sum(m[,col]) 
    }
  }
  m2[is.na(m2)] <- 0 
  return (m2) 
}
transitionpourcent <- markovpourcent(transition) 

library(markovchain)
markov <- new('markovchain',
  transitionMatrix = t(transitionpourcent),
  states = c("M","D","N"))

#Le graphique
plot(markov,
  edge.arrow.size = 0.25, edge.arrow.width = 1,edge.color ="grey",edge.lty = 1,
  edge.label.cex = 1,edge.label.color = "black",
  vertex.color = "lightgreen", vertex.shape = "circle",vertex.size = 3*table(test3A),
  vertex.frame.color= NA,
  vertex.label.color ="black",vertex.label.cex = 2.5
)
#Markov4
markov1 <- function (seq){ 
  seq = seq[!is.na(seq)]
  l = length(unique(seq)) 
  m = matrix(nrow = l, ncol = l) 
  rownames(m)= unique(seq)[order(unique(seq))] 
  colnames(m)= unique(seq)[order(unique(seq))] 
  
  transition <-function(x,y){ 
    a = 0 
    previous = 0 
    for (i in seq) { 
      if (i == x & previous == y) { 
        a = a+1 
      }
      previous = i 
    }
    return(a) 
  }
  for (row in 1:nrow(m)){ 
    for (col in 1:ncol(m)) { 
      m[row,col]=transition(rownames(m)[row],colnames(m)[col]) 
    }
  }
  return(m) 
}
transition<-markov1(test4A) 

markovpourcent <- function (m){ 
  m2 = m 
  for (row in 1:nrow(m2)){ 
    for (col in 1:ncol(m2)) { 
      m2[row,col]=m[row,col]/sum(m[,col]) 
    }
  }
  m2[is.na(m2)] <- 0 
  return (m2) 
}
transitionpourcent <- markovpourcent(transition) 

library(markovchain)
markov <- new('markovchain',
  transitionMatrix = t(transitionpourcent),
  states = c("M","I","N"))

#Le graphique
plot(markov,
  edge.arrow.size = 0.25, edge.arrow.width = 1,edge.color ="grey",edge.lty = 1,
  edge.label.cex = 1,edge.label.color = "black",
  vertex.color = "lightgreen", vertex.shape = "circle",vertex.size = 3*table(test4A),
  vertex.frame.color= NA,
  vertex.label.color ="black",vertex.label.cex = 2.5
)
# Plus d'options ici : https://igraph.org/r/doc/plot.common.html

#Importation des données.
SciViews::R
googlesheets_as_csv <- "https://docs.google.com/spreadsheets/d/{id}/export?format=csv"
bourdon_data_id <- "10IN_8qOXWK6XJ8fVWGKJ_lpUcc_ziuS6iiJztwK1AnU"
(bourdon_url<- glue::glue(googlesheets_as_csv, id = bourdon_data_id))
bourdon<- read$csv(bourdon_url)


#Repartition de la masse des instars larvaires.
colnames(bourdon)[colnames(bourdon)=="Oeufs, larves et pupes"] <- "masse"
bourdon$masse <- as.numeric(as.character(bourdon$masse))

bourdon$...6 <- as.numeric(as.character(bourdon$...6))
bourdon$Imagos <- as.numeric(as.character(bourdon$Imagos))
bourdon$...7 <- as.numeric(as.character(bourdon$...7))

#Distribution de la masse des instars larvaires
library(dplyr)
bourdon$...2 <- as.numeric(as.character(bourdon$...2))
masses_instars <- bourdon %>%
filter(...2 == "Dernier instar post-défécation" | ...2 == "Premiers instars" |  ...2 == "Pupe") %>%
  select(masse)

chart(data = masses_instars, ~ masse |...2) +
  geom_histogram(bins = 25) +
  xlab ("Masse des instars(mg)")+
  ylab("Effectifs")




  chart(data = masses_instars, ~masse) +
    geom_histogram(bins = 40, color = "black",alpha = 1) +
  labs(title = "Distribution de la masse des instars larvaires",x = "Masse des instars(mg)", y = "Effectifs")
  
#Distribution de la masse des ouvrières
  library(dplyr)
  masses_ouvrières <- bourdon %>%
    filter(...2 == "Ouvrière ???") %>%
    select(...6)
  chart(data =masses_ouvrières, ~ ...6) + geom_histogram(bins = 35, color = "black",alpha =1)+
    labs(title= "Distribution de la masse des ouvrières",x="Masse des ouvrières(mg)", y="Effectifs")
  
  #Distribution de la cellule radiale des imagos
  library(dplyr)
  cellule_radiale <- bourdon %>%
    filter(...2=="Ouvrière ???") %>%
    select(Imagos)
  chart(data=cellule_radiale, ~ Imagos) + geom_histogram(bins=35, color="black",alpha=1)+
    labs(title= "Distribution de la taille de la cellule radiale des ouvrières",x="Taille de la cellule radiale des ouvrières(mm)", y="Effectifs")
  
  #Distribution de l'ITD des imagos
  library(dplyr)
  distance_intertégulaire <- bourdon %>%
    filter(...2=="Ouvrière ???") %>%
    select(...7)
  chart(data=distance_intertégulaire, ~ ...7) + geom_histogram(bins=35, color="black",alpha=1)+
    labs(title= "Distribution de la distance intertégulaire des ouvrières",x="Distance intertégulaire des ouvrières(mm)", y="Effectifs")
  
  
  
  



bourdons1 = data.frame(bourdons = (rnorm(300, mean=5, sd=1)))
ggplot(data = bourdons1, mapping = aes(x =Masse_fraiche_individuelle)) + geom_histogram()

