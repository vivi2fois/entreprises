library(RMySQL)
library(dbConnect)
library(tidyverse)
library(DBI)
library(leaflet)
library(geosphere)
library(ggplot2)
library(gridExtra)
library(maps)
library(geosphere)


con <- DBI::dbConnect(RMySQL::MySQL(),user='root', password='root', dbname='entreprise', host='127.0.0.1')



# graphique des entreprises créées en 2018 par région
cr <- select(creation,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)

france <- leaflet() %>% setView(lng = 2.195506, lat = 46.231015, zoom = 6)
france %>% addTiles()

graph_creation <- leaflet(cr) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lng = ~longitude, lat = ~latitude, popup = paste("Ville:", creation$ville, "<br>",
                                                              "Siren:", creation$siren, "<br>",
                                                              "Statut:", creation$forme_juridique, "<br>",
                                                              "Capital:", creation$capital, "<br>",
                                                              "Adresse:", creation$adresse, "<br>",
                                                              "Dénomination:", creation$denomination, "<br>",
                                                              "Secteur:", creation$secteur))
graph_creation

# graphique des entreprises fermées en 2018 par région
fe <- select(fermeture,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)

france <- leaflet() %>% setView(lng = 2.195506, lat = 46.231015, zoom = 6)
france %>% addTiles()

graph_fermeture <- leaflet(fe) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lng = ~longitude, lat = ~latitude, popup = paste("Ville:", creation$ville, "<br>",
                                                              "Siren:", creation$siren, "<br>",
                                                              "Statut:", creation$forme_juridique, "<br>",
                                                              "Capital:", creation$capital, "<br>",
                                                              "Adresse:", creation$adresse, "<br>",
                                                              "Dénomination:", creation$denomination, "<br>",
                                                              "Secteur:", creation$secteur))
graph_fermeture


# TOP 10 des secteurs où il y a le plus de créations d'entreprises
cr_top <- select(creation,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)
# Suppréssion des secteurs en NA
cr_top <- cr_top %>% filter(!is.na(secteur))

sc <- cr_top %>% group_by(secteur) %>% summarise(volnov=n())
s_top <- sc %>% arrange(desc(volnov)) %>% head(11)

#Supprimer les données incohérentes (secteur en instance d'activité)
sc_top <- dplyr::slice(s_top,2:11) 

# Graphique tendance des secteurs d'activités
graph_cr <-ggplot(data=sc_top, aes(x=secteur, y=volnov)) +
  geom_bar(stat="identity")
# Barplot horizontal
graph_cr + coord_flip()


# TOP 10 des secteurs où il y a le plus de fermetures d'entreprises
cr_moins <- select(fermeture,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)
# Suppréssion des secteurs en NA
cr_moins <- cr_moins %>% filter(!is.na(secteur))

fer <- cr_moins %>% group_by(secteur) %>% summarise(volnov=n())
s_moins <- fer %>% arrange(desc(volnov)) %>% head(10)

#Supprimer les données incohérentes (secteur en instance d'activité)
#sc_top <- dplyr::slice(s_top,2:11) 

# Graphique tendance des secteurs d'activités
graph_cr <-ggplot(data=s_moins, aes(x=secteur, y=volnov)) +
  geom_bar(stat="identity")
# Barplot horizontal
graph_cr + coord_flip()


#Les formes juridiques les plus utilisées durant une création d'entreprise
fj <- select(creation,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)
# Suppréssion des secteurs en NA
fj <- fj %>% filter(!is.na(secteur))

fjcr <- fj %>% group_by(forme_juridique) %>% summarise(volnov=n())
fj_top <- fjcr %>% arrange(desc(volnov)) %>% head(3)
#graphique 
graph_fj_top <-ggplot(data=fj_top, aes(x=forme_juridique, y=volnov)) +
  geom_bar(stat="identity")
# Barplot horizontal
graph_fj_top + coord_flip()


#Les formes juridiques les plus radiées 
fjm <- select(fermeture,region,ville,siren,forme_juridique,capital,adresse,denomination,secteur,latitude,longitude)
# Suppréssion des secteurs en NA
fjm<- fjm %>% filter(!is.na(secteur))

fjmf <- fjm %>% group_by(forme_juridique) %>% summarise(volnov=n())
fjm_top <- fjmf %>% arrange(desc(volnov)) %>% head(3)
#graphique 
graph_fjm <-ggplot(data=fjm_top, aes(x=forme_juridique, y=volnov)) +
  geom_bar(stat="identity")
# Barplot horizontal
graph_fjm + coord_flip()

# graph capital par secteur 
cr_top <- select(creation,capital,secteur,longitude,latitude)
# Suppréssion des secteurs en NA
cr_top <- cr_top %>% filter(!is.na(secteur))

sc <- cr_top %>% group_by(secteur,capital) %>% summarise(total_secteur=n())
s_top <- sc %>% arrange(desc(total_secteur)) %>% head()


graph_cap_secteur <- ggplot(s_top) + 
  aes(x=total_secteur, y=capital, col=secteur) + geom_point()
graph_cap_secteur + coord_flip()


# Prédiction




