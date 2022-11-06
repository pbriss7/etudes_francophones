# Importation des données issues de la recherche sur la francophonie dans la littérature savante (ProQuest)

setwd("~/github/PERSONNEL/Francophonie_ProQuest")
library(data.table)
library(xlsx)
library(dplyr)
library(stringr)
library(ggplot2)
if(!"textcat" %in% rownames(installed.packages())) {install.packages("textcat")}
library(textcat)
if(!"topicmodels" %in% rownames(installed.packages())) {install.packages("topicmodels")}
if(!"stm" %in% rownames(installed.packages())) {install.packages("stm")}
library(stm)
library(topicmodels)
library(tm)
library(lsa)
if(!"LDAvis" %in% rownames(installed.packages())) {install.packages("LDAvis")}
library(LDAvis)
library(udpipe)
if(!"countrycode" %in% rownames(installed.packages())) {install.packages("countrycode")}
library(countrycode)
library(proustr)
library(tidygeocoder)
library(maps)
library(viridis)
library(gt)

# # Importation des fichiers xml
# list.files()
# liste_fichiers <- list.files("donnees/20221026corpus_leger/", pattern = ".xls")
# liste_fichiers_long <- paste0("donnees/20221026corpus_leger/", liste_fichiers)
# 
# # Lecture des fichiers xml
# liste_read <- lapply(liste_fichiers_long, read.xlsx, 1)
# 
# # assemblage de la liste en df
# corpus <- do.call(bind_rows, liste_read)
# setDT(corpus)
# corpus <- corpus[!Title %in% c("Éditorial", "Comments", "Conclusions", 	
# "Francophony[ies]", "Multiple Francophones", "Opening Address", "Advertisement 11 -- No Title")]
# 
# # Réduction de la structure
# corpus <- corpus[!documentType %in% c("review", "Book Review") & !Authors == "[Unknown]" & !duplicated(Title)]
# corpus <- corpus[, c("Title", "Abstract", "StoreId", "ArticleType",
#                          "documentType", "isbn", "language", "languageOfSummary",
#                          "year", "pubdate", "classificationCodes","identifierKeywords",
#                          "majorClassificationCodes", "subjectClassifications",
#                          "subjectTerms", "subjects")]
# 
# # Correction des types de données
# corpus[, year:=as.integer(year)]
# 
# corpus$languageOfSummaryTextCat <- textcat(corpus$Abstract)
# corpus$languageOfTitleTextCat <- textcat(corpus$Title)
# 
# # Inspection
# corpus[languageOfTitleTextCat == "french", .(Title)][1:100]
# 
# 
# # Ordre des colonnes
# setcolorder(corpus, neworder = c("Title", "Abstract", "languageOfSummaryTextCat",
#                                  "StoreId", "ArticleType", "documentType", "isbn",
#                                  "language","year", "classificationCodes",
#                                  "identifierKeywords", "majorClassificationCodes", 
#                                  "subjectClassifications","subjectTerms", "subjects"))
# 
# # Correction
# corpus <- corpus[!duplicated(Title)]
# corpus[Title == "Towards an Independent and Ethnically Pure Flanders", year:=1994]
# corpus[Title == "Translating African Literature from French into English", year:=1994]
# corpus[Title == "Towards an Independent and Ethnically Pure Flanders", year:=1994]
# 
# corpus[Title == "On Onesime Reclus's Language Conception", languageOfTitleTextCat:="english"]
# corpus[Title == "Colonial conscripts. The Tirailleurs sénégalais in French West Africa, 1857-1960", languageOfTitleTextCat:="english"]
# corpus <- corpus[!Title %like% "Reviews\\s+--" & !Title %like% "Review\\s?$"]
# corpus <- corpus[!Title == "Une échelle ordinale permettant de classer les répondants en « satisfait », « indifférent » et « insatisfait »: INTRODUCTION REVUE DE LA LITTÉRATURE La mesure de satisfaction Le concept de la zone d'indifférence LES OBJECTIFS DE CETTE ÉTUDE ÉTUDE EMPIRIQUE La méthode des intervalles apparemment égaux Etalonnage des énoncés de satisfaction LES RÉSULTATS Hiérarchisation et catégorisation des énoncés Développement d'une échelle ordinale de satisfaction CONCLUSIONS, LIMITES ET VOIES DE RECHERCHE FUTURES RÉFÉRENCES BIBLIOGRAPHIQUES ANNEXE.--EXEMPLES D'APPLICATION DE L'ÉCHELLE"]
# corpus <- corpus[Title == "Le probleme du livre face au lecteur en Afrique: quelques lignes d'actions proposees. The book problem confronting African readers: some suggested lines of action", Title:= "Le probleme du livre face au lecteur en Afrique: quelques lignes d'actions proposees"]
# corpus <- corpus[Title == "Le Planetaire. Le Monde francophone (Afrique) [(Planetarium. The French-Speaking World (Africa)]", Title:="Le Planetaire. Le Monde francophone (Afrique)"]
# corpus <- corpus[Title == "Le Francais de la Saskatchewan: Etude de lexique (The French of Saskatchewan: Lexical Study)", Title:="Le Francais de la Saskatchewan: Etude de lexique"]
# corpus <- corpus[Title == "Les francophones et les etudes postsecondaires (Francophones and Postsecondary Studies)", Title:="Les francophones et les etudes postsecondaires"]
# corpus <- corpus[Title == "Le disque optique compact, nouveau support de diffusion pour le developpement des reseaux documentaures cooperatifs. The optical compact disc, new support for the development of cooperative documentation networks", Title:="Le disque optique compact, nouveau support de diffusion pour le developpement des reseaux documentaures cooperatifs"]
# 
# corpus <- corpus[Title == "Impact of social change on job values: a longitudinal study of Quebec business students: Revue Canadienne des Sciences de l'Administration", `:=`(Title = "Impact of social change on job values: a longitudinal study of Quebec business students",
#                                                                                                                                                                                   languageOfTitleTextCat="english")]
# corpus <- corpus[Title == "Failure of l'Action Libérale Nationale", languageOfTitleTextCat:="english"]
# corpus <- corpus[Title == "Les francophones et les etudes postsecondaires (Francophones and Postsecondary Studies)", Title:="Les francophones et les etudes postsecondaires"]
# corpus <- corpus[Title == "Enjeux de la responsabilisation en milieu educatif minoritaire franco-ontarien (Accountability in the Education Arena: What's at Stake for Francophone Schools in Ontario)", Title:="Enjeux de la responsabilisation en milieu educatif minoritaire franco-ontarien"]
# corpus <- corpus[Title == "Les francophones et les etudes postsecondaires (Francophones and Postsecondary Studies)", Title:="Les francophones et les etudes postsecondaires"]
# corpus <- corpus[Title == "[L'Amerique du Nord francaise dans les archives religieuses de Rome, 1600-1922]: Report - Canadian Catholic Historical Association", Title:="L'Amerique du Nord francaise dans les archives religieuses de Rome, 1600-1922"]
# corpus <- corpus[Title == "Achard-Bayle, Guy, Grammaire des métamorphoses. Référence, identité, changement. (Champs linguistiques - recherches.) Bruxelles: Duculot, 2001, 300 pp. ISBN: 2 8011 1284 4 ISSN 1374 089 X", Title:="Grammaire des métamorphoses"]
# corpus <- corpus[Title == "Les francophones et les etudes postsecondaires (Francophones and Postsecondary Studies)", Title:="Les francophones et les etudes postsecondaires"]
# corpus <- corpus[!Title == "Chauveau, Jean-Paul (ed.), Französisches Etymologisches Wörterbuch, fasc. 158, tome XXV (refonte du tome 1er): auscultare - autós, pp. 1057-1122 (2000); Chauveau, Jean-Paul, Lagueunière, France and Thibault, André (eds.), Französisches Etymologisches Wörterbuch, fasc. 159, tome XXI (2e partie): Matériaux d'origine inconnue ou incertaine: tannerie-luxe, pp.193-322 (2001); Greub, Yan, Französisches Etymologisches Wörterbuch, fasc. 160: Table des matières et index des concepts des volumes 21 à 23 [= Matériaux d'origine inconnue ou incertaine], 36 pp. (2001). Bâle: Zbinden."]
# corpus <- corpus[Title == "Armstrong, Nigel, Bauvois, Cécile, Beeching, Kate (éds.), et Bruyninckx, Marielle (éditrice adjointe), La langue française au féminin. Le sexe et le genre affectent-ils la variation linguistique? Paris: L'Harmattan, 2001. 236 pp. 2 7475 0459 X", Title:="La langue française au féminin. Le sexe et le genre affectent-ils la variation linguistique?"]
# corpus <- corpus[Title == "Daveluy, Michelle, 2006. Les langues étendards. Allégeances langagières en français parlé. Québec: Éditions Nota Bene, 2006, 131 pp. 2 89518 155 1", Title:="Les langues étendards. Allégeances langagières en français parlé"]
# corpus <- corpus[Title == "Brasseur, Patrice et Falkert, Anika, Français d'Amérique: approches morphosyntaxiques. Agence Intergouvernementale de la Francophonie, Paris: L'Harmattan, 2005, 329 pp. 2 74759 596 X", Title:="Français d'Amérique: approches morphosyntaxiques"]
# 
# 
# corpus$Title <- str_remove_all(corpus$Title, "I:\\s|II:\\s|III:\\s")
# corpus$Title <- str_remove_all(corpus$Title, "Quelques themes - |Quelques formes - ")
# corpus$Title <- str_replace_all(corpus$Title, "&amp;snot;", "e")
# corpus$Title <- str_remove_all(corpus$Title, "&lt;&lt;|IV. ")
# corpus$Title <- str_remove_all(corpus$Title, "&lt;&lt;|IV. ")
# 
# corpus[languageOfTitleTextCat == "french", .(Title)][201:300]
# 
# # Transfert de la structure de données dans OpenRefine: les modifications à faire sont trop nombreuses pour les faire dans R
# fwrite(corpus, "~/Downloads/francophonie.csv")

corpus_abs_tr_en <- fread("donnees/20221105_PB_corpus_traduit_georeference.csv")
# corpus_abs_tr_en[, `:=`(language=NULL,
#                      languageOfSummary=NULL)]
# 
# corpus_ti_tr_en <- fread("donnees/corpus_francophonie_titres_tr_en.csv")
# 
# identical(corpus_ti_tr_en$StoreId, corpus_abs_tr_en$StoreId)
# 
# corpus_ti_tr_en <- corpus_ti_tr_en[, .(StoreId, Title)]
# setnames(corpus_ti_tr_en, old = "Title", new = "Title_tr_en")
# 
# corpus_abs_tr_en <- merge(corpus_abs_tr_en, corpus_ti_tr_en, by="StoreId")
# 
# setnames(corpus_abs_tr_en, old = "Abstract", new = "Abs_tr_en")
# 
# setcolorder(corpus_abs_tr_en, neworder = c("StoreId", "Title_tr_en", "Abs_tr_en"))

rm(list=setdiff(ls(), "corpus_abs_tr_en"))

########################################## Enrichissement du corpus ==> ajout d'une variable lieux
# corp_abs <- corpus[!Abstract == "" & !is.na(Abstract)]


# Importation d'un tableau des villes du monde 
# library(maps)

cities <- maps::world.cities
setDT(cities)

# french_countries <- fread("donnees/20221105_WorldPopulationReview_french_speaking_countries.csv")

# all_french_cities <- cities[country.etc %in% french_countries$country]

# all_french_large_cities <- all_french_cities[pop>75000]

large_cities <- cities[pop>300000]

cities_regex <- paste0(large_cities$name, collapse = "\\b|\\b")



# Importation d'un tableau avec tous les noms de pays (extension countryname)+création regex

library(spData)

spData::world
world_poly <- spData::world

world_poly <- world_poly %>% select(iso_a2, name_long, continent, geom)
setDT(world_poly)

setnames(world_poly, new = c("iso_a2", "nom_pays", "continent", "geom"))

world_poly[, nom_pays:=nom_pays %>% str_replace_all(
  c("Democratic Republic of the Congo"="D\\.?(emocratic)?.?R\\.?(epublic)?.([Oo]f.[Tt]he.)?Congo",
    "Côte d'Ivoire"="(Côte d'Ivoire)|(Ivory Coast)",
    "Republic of the Congo"="Rep\\.?(ublic)?\\s(of\\s)?(the\\s)?Congo",
    "Russian Federation"="(Russian Federation)|(Russia)",
    "The Gambia"="Gambia",
    "Dem. Rep. Korea"="(Dem\\.?|DR|D\\.R(ep)?\\.)(ocratic\\s|\\s(Rep\\.?\\s)|\\s(of\\s)?Korea)\\s?(Republic)?\\s?(of)?(\\sKorea)?(Korea)?|North Korea",
    "Republic of Korea"="(Rep\\.?(ublic)?\\sof\\s)|(South\\s)?Korea"))]

world_poly[1:100]

country_regex <- paste0(world_poly$nom_pays, collapse = "\\b|\\b")


# Création d'une regex pour continents
continents_names <-
  c(
    "Asia",
    "Africa",
    "N(orth|\\.).America",
    "S(outh|\\.).America",
    "Antarctica",
    "Europe",
    "Oceania"
  )
continents_regex <- paste0(continents_names, collapse = "\\b|\\b")
# On utilise ces regex pour étiqueter les noms propres correspondant à un nom de ville ou de pays

# # Élimination de tous les accents
# corpus_abs_tr_en[, `:=`(
#   Abs_tr_en_unaccent = proustr::pr_unacent(Abs_tr_en),
#   Ti_tr_en_unaccent = proustr::pr_unacent(Title_tr_en)
# )]


setcolorder(
  corpus_abs_tr_en,
  neworder = c(
    "StoreId",
    "Title_tr_en",
    "Ti_tr_en_unaccent",
    "Abs_tr_en",
    "Abs_tr_en_unaccent"
  )
)


# Élimination de toutes les parenthèses
corpus_abs_tr_en[, `:=`(
  Ti_tr_en_unaccent = str_remove_all(Ti_tr_en_unaccent, "\\(([^\\)]+)\\)"),
  Abs_tr_en_unaccent = str_remove_all(Abs_tr_en_unaccent, "\\(([^\\)]+)\\)")
)]


# Élimination des sous-titres (1 seul trouvé) formé de titres de colloques
corpus_abs_tr_en[Ti_tr_en_unaccent %ilike% ":\\s?(papers?|articles?) from.+$", .N]

corpus_abs_tr_en[, `:=`(
  Ti_tr_en_unaccent = str_remove_all(Ti_tr_en_unaccent, ":\\s?papers from.+$"),
  Abs_tr_en_unaccent = str_remove_all(Abs_tr_en_unaccent, ":\\s?papers from.+$")
)]


# Extraction des noms de villes des titres traduits (au besoin) en anglais
corpus_abs_tr_en[, villes := str_extract_all(Ti_tr_en_unaccent, cities_regex)]

# Extraction des noms de pays des résumés traduits (au besoin) en anglais
corpus_abs_tr_en[, pays := str_extract_all(Abs_tr_en_unaccent, country_regex)]

# Extraction des noms de continents des résumés traduits (au besoin) en anglais
corpus_abs_tr_en[, continents := str_extract_all(Abs_tr_en_unaccent, continents_regex)]

corpus_abs_tr_en[, pays := lapply(pays, unique)]
corpus_abs_tr_en[, villes := lapply(villes, unique)]
corpus_abs_tr_en[, continents := lapply(continents, unique)]

# Séparation des modalités multiples de certaines observations en de multiples colonnes dans OpenRefine
fwrite(corpus_abs_tr_en,
       "donnees/20221105_PB_corpus_traduit_georeference.csv")

# Importation de la structure enrichie
corpus_abs_tr_en_sep <-
  fread("donnees/20221105-PB-corpus-traduit-georeference-separe.csv")


# Élimination des notices sans titre
corpus_abs_tr_en_sep <-
  corpus_abs_tr_en_sep[!is.na(Title_tr_en) & !Title_tr_en == ""]



# Liste de tous les pays accompagnés des identifiants
pays_melt <- melt(
  corpus_abs_tr_en_sep,
  id.vars = "StoreId",
  measure.vars = patterns("pays.+"),
  value.name = "nom_pays"
)

pays_melt <- pays_melt[!nom_pays == ""]

ggplot(pays_melt[, .N, "nom_pays"][order(N, decreasing = T)][1:25][!nom_pays ==
                                                                     "NA" & !nom_pays == "Brunswick"], aes(x = reorder(nom_pays, N), y = N)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Greys")+
  theme_classic()+
  geom_text(aes(label = N),
            hjust = 1.2,
            size = 2.5,
            colour = "white")+
  labs(
    title = "Pays dans les résumés",
    caption = "Données: ProQuest, 2022",
    subtitle = paste0("Nombre total de notices: ", nrow(corpus_abs_tr_en_sep))
  ) +
  ylab("Occurrences uniques") +
  xlab(NULL)

ggsave("resultats/20221105_PB_Distrib_pays_resumes_barplot.png", dpi = 300)

# Liste de tous les continents accompagnés des identifiants
continent_melt <- melt(
  corpus_abs_tr_en_sep,
  id.vars = "StoreId",
  measure.vars = patterns("continent.+"),
  value.name = "nom_continent"
)

continent_melt <-
  continent_melt[!nom_continent == "" & !is.na(nom_continent)]

ggplot(continent_melt[, .N, "nom_continent"][order(N, decreasing = T)][1:25][!nom_continent ==
                                                                               "NA"], aes(x = reorder(nom_continent, N), y = N)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()+
  geom_text(aes(label = N),
            hjust = 1.1,
            size = 2.5,
            colour = "white")+
  labs(title = "Continents dans les résumés",
       subtitle = paste0("Nombre total de notices: ", nrow(corpus_abs_tr_en_sep)),
       caption = "Données: ProQuest, 2022") +
  ylab("Occurrences uniques") +
  xlab(NULL)

ggsave("resultats/20221105_PB_Distrib_continents_resumes_barplot.png", dpi = 300)


# Liste de toutes les villes accompagnées des identifiants
# modifier la stratégie et utiliser les TITRES ici. Les noms de villes sont souvent extraites de notices bibliographiques. Faux positifs
ville_melt <- melt(
  corpus_abs_tr_en_sep,
  id.vars = "StoreId",
  measure.vars = patterns("ville.+"),
  value.name = "nom_ville"
)

# ville_melt <- ville_melt[!nom_ville==""&!nom_ville %in% c()]


ville_melt_N <- ville_melt[, .N, nom_ville][order(N, decreasing = T)]
ville_melt_N[nom_ville == "Orleans", nom_ville:="New Orleans"]
ggplot(ville_melt_N[!nom_ville == "" &
                                       !is.na(nom_ville) &
                                       !nom_ville == "NA" &
                                       !nom_ville %in% c("Quebec", "London")][order(N, decreasing = T)], aes(x =
                                                                                                               reorder(nom_ville, N), y = N)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()+
  geom_text(aes(label = N),
            hjust = 1.1,
            size = 2.5,
            colour = "white")+
  labs(
    title = "Villes de pays francophones dans les titres",
    subtitle = paste0("Nombre total de notices: ", nrow(corpus_abs_tr_en_sep)),
    caption = "Données: ProQuest, 2022"
  ) +
  ylab("Occurrences uniques") +
  xlab(NULL)

ggsave("resultats/20221105_PB_Distrib_villes_titres_barplot.png", dpi = 300)


############################################# Encodage des lieux
ville_melt_geo <- tidygeocoder::geocode(ville_melt, nom_ville, full_results = TRUE)
pays_melt_geo <- tidygeocoder::geocode(pays_melt, nom_pays, full_results = TRUE)
continent_melt_geo <- tidygeocoder::geocode(continent_melt, nom_continent, full_results=TRUE)

saveRDS(ville_melt_geo, "donnees/20221105_PB_ville_melt_geo.RDS")
saveRDS(pays_melt_geo, "donnees/20221105_PB_pays_melt_geo.RDS")
saveRDS(continent_melt_geo, "donnees/20221105_PB_continent_melt_geo.RDS")

############################################ Cartes géographiques
world <- map_data("world")

# Carte des pays cités dans les résumés en anglais
setDT(pays_melt_geo)
pays_melt_geo

# Données agrégées
pays_melt_geo_N <- pays_melt_geo[, .N, by=list(nom_pays)][order(N, decreasing = T)]



pays_melt_geo_N[nom_pays=="Brunei", nom_pays:="Brunei Darussalam"]
pays_melt_geo_N[nom_pays=="Dominica", nom_pays:="Dominican Republic"]
pays_melt_geo_N[nom_pays=="French Guiana", nom_pays:="Guyana"]
pays_melt_geo_N[nom_pays=="Gambia", nom_pays:="The Gambia"]
pays_melt_geo_N[nom_pays=="North Korea", nom_pays:="Republic of Korea"]
pays_melt_geo_N[nom_pays=="Russia", nom_pays:="Russian Federation"]
pays_melt_geo_N[nom_pays=="South Korea", nom_pays:="Dem. Rep. Korea"]
pays_melt_geo_N[nom_pays=="Mauritius", nom_pays:="Mauritania"]



pays_poly <- merge(world_poly, pays_melt_geo_N, by.x="nom_pays", by.y= "nom_pays", all.x=FALSE, all.y=TRUE)
pays_poly <- pays_poly[!is.na(iso_a2)]


# 
# # Transformation des lon/lat en simple feature (SF)
library(sf)
# pays_melt_geo_N_sf <- pays_melt_geo_N %>%
#   st_as_sf(
#     coords = c("long", "lat"),
#     crs = 4326
#   )
# 
# # # Visualisation selon la valeur N
# # library(mapview)
# pays_melt_geo_N_sf %>%
#   mapview(
#     cex     = "N",
#     col.region = "yellow",
#     alpha.regions = 0.5)



pays_poly_sf <- pays_poly %>% st_as_sf(crs=4326)


# Carte des pays les plus cités
library(mapview)
mapview_pays <- pays_poly_sf[, c("nom_pays", "N", "geom")] %>% 
  mapview(zcol = "N",
          color = "black",
          layer.name = "Nombre documents",
          label = pays_poly_sf$nom_pays
          )

mapshot(mapview_pays, url = "resultats/20221105_PB_carte_pays_resumes.html")




################ Mapping des noms de continents dans les résumés

continent_melt_N <- continent_melt[, .N, "nom_continent"]

continents_shp <- st_read("donnees/World_Continents/World_Continents.shp")

continents_poly_sf <- continents_shp %>% st_as_sf(crs=4326)

continents_poly_N_sf <- left_join(continents_poly_sf, continent_melt_N, by=c("CONTINENT" = "nom_continent"))

# Carte des pays les plus cités
mapview_continent <- continents_poly_N_sf[, c("CONTINENT", "N", "geometry")] %>% 
  mapview(zcol = "N",
          color = "black",
          layer.name = "Nombre documents",
          label = continents_poly_N_sf$CONTINENT
  )


mapshot(mapview_continent, url = "resultats/20221105_PB_carte_continents_resumes.html")


################ Mapping des villes dans les titres
setDT(ville_melt_geo)
ville_melt_geo_not_null <- ville_melt_geo[!is.na(lat), .(StoreId, nom_ville, lat, long)]

ville_melt_geo_N <- ville_melt_geo_not_null[, .N, c("nom_ville", "lat", "long")][order(N, decreasing = T)]

villes_map <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(x=long, y=lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1) +
  geom_point(
    data = ville_melt_geo_N[!nom_ville %in% c("Quebec", "Benin")],
    aes(long, lat,color=N, size = N),
    alpha = 0.5,
    show.legend = T) +
  geom_text(data = ville_melt_geo_N[!nom_ville %in% c("Quebec", "Benin")],
            aes(x=long, y=lat, label=paste(nom_ville, N, sep = ": ")),
            hjust=-0.2, vjust=0.5, check_overlap = TRUE, size = 3.5) +
  labs(title = "Distribution géographique des villes dans les titres du corpus",
       subtitle = "N = fréquence documentaire") + 
  xlab("Longitude") + 
  ylab("Latitude")+
  xlim(-120, 155)+
  ylim(-40, 65)

ggsave("resultats/20221106_PB_Distrib_geo_villes.png", dpi = 300)


corpus_abs_tr_en_sep[Title %ilike% "\\bbenin\\b", .(Title)]



############################################## Silhouette du corpus - distribution chronologique
ggplot(corpus_abs_tr_en_sep[, .(StoreId, year)][,.N, by="year"], aes(x=year, y=N))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = N),
            hjust = 0.6,
            vjust = -1,
            size = 1.7,
            colour = "black")+
  theme_classic()+
  labs(title = "Distribution chronologique des notices du corpus",
       subtitle = "Mot de la requête: «francophon*»",
       caption = "Données: ProQuest, 2022")+
  ylab("Nombre de notices")+
  xlab("Années")

ggsave("resultats/20221105_PB_DistribChronologique_corpus_trad_en.png", dpi=300)



setnames(corpus_abs_tr_en_sep, old = c("StoreId", "Abstract"), new = c("doc_id", "text"))

# Catégorisation de variables
corpus$languageOfSummaryTextCat <- as.factor(corpus$languageOfSummaryTextCat)
corpus$documentType <- as.factor(corpus$documentType)

############### Note: pour contrôler les paramètres de prétraitement, exécuter ceux-ci préalablement à la fonction textPreprocessor de stm
corpus_abs_tr_en_sep[Abs_tr_en_unaccent =="", .N]
corpus_abs_tr_en_sep[, text:=ifelse(Abs_tr_en_unaccent!="", Abs_tr_en_unaccent, Ti_tr_en_unaccent)]


# * default parameters
processed_en <- textProcessor(corpus_abs_tr_en_sep$text, metadata = as.data.frame(corpus_abs_tr_en_sep[, .(StoreId, ArticleType, documentType, year, identifierKeywords, subjects,`villes 1`, `villes 2`,`villes 3`,`pays 1`,`pays 2`,`pays 3`,`continents 1`,`continents 2`,`continents 3`)]),
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = TRUE, #*
                           wordLengths = c(3,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = TRUE, # not def
                           striphtml = FALSE, #*
                           customstopwords = c("director", "directed", "editor", "edited",
                                               "abstract","one", "will", "les", "des"), #*
                           v1 = FALSE) #*


# filter out terms that don’t appear in more than 10 documents,
out <- prepDocuments(processed_en$documents, processed_en$vocab, processed_en$meta, lower.thresh=10)

# L'analyse porte au final sur 2988 documents, car 4 n'ont aucun mot

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# Construire un premier modèle
set.seed(831)
system.time({
  First_STM <- stm(docs, vocab, 40,
                   prevalence =~ s(year),
                   data = meta,
                   seed = 15, max.em.its = 5
  )
})

# Observation du premier modèle
plot(First_STM)



# Deuxième modèle
set.seed(832)
system.time({
  Second_STM <- stm(documents = out$documents, vocab = out$vocab,
                    K = 18, 
                    prevalence =~ s(year),
                    max.em.its = 75, data = out$meta,
                    init.type = "Spectral", verbose = TRUE
  )
})

# Plot second Topic Model
plot(Second_STM)

# Trouver le nombre idéal de topiques (K) 
set.seed(833)
system.time({
  findingk <- searchK(out$documents, out$vocab, K = c(20:40),
                      prevalence =~ s(year), data = meta, verbose=TRUE
  )
})

# Plot
plot(findingk)

# Find k: Approach 2
set.seed(834)
system.time({
  findingk_ver2 <- searchK(documents = out$documents, 
                           vocab = out$vocab,
                           K = c(10,20,30,40,50,60,70), #specify K to try
                           N = 500, # matches 10% default
                           proportion = 0.5, # default
                           heldout.seed = 1234, # optional
                           M = 10, # default
                           cores = 1, # default=1
                           prevalence =~s(year),
                           max.em.its = 75, #was 75
                           data = meta,
                           init.type = "Spectral",
                           verbose=TRUE
  )
})

# Plot
plot(findingk_ver2)


# Find k: Approach 3
set.seed(835)
system.time({
  findingk_ver3.lee_mimno <- stm(documents = out$documents, 
                                 vocab = out$vocab,
                                 K = 0, # K=0 instructs STM to run Lee-Mimno
                                 seed = 1234, # randomness now, seed matters
                                 prevalence =~ s(year),
                                 max.em.its = 75,
                                 data = meta,
                                 init.type = "Spectral",
                                 verbose=TRUE
  )
})

# Plot
plot(findingk_ver3.lee_mimno)

# Run final topic model at 20 topics and see how long it takes
set.seed(836)
system.time({
  Third_STM <- stm(documents = out$documents, vocab = out$vocab,
                   K = 39, prevalence =~ s(year),
                   max.em.its = 75, data = out$meta,
                   init.type = "Spectral", verbose = FALSE
  )
})

#Plot
png("resultats/test.png")
plot(Third_STM,
     main = "39 thèmes des études francophones\nNombre de documents avec contenu textuel analysés: 2988",
     sub = "\nDonnées: ProQuest, 2022",
     xlab = "Proportions des thèmes"
     )
dev.off()


str(Third_STM)
label_topics_matrix <- labelTopics(Third_STM)[[1]]

noms_themes_dt <- data.table(theme_no = 1:nrow(label_topics_matrix),
                          theme_nom = paste(label_topics_matrix[, 1], label_topics_matrix[, 2], label_topics_matrix[, 3], sep = "_"))

noms_themes <- gt(noms_themes_dt)|>tab_header(
  title = "Numéro des thèmes et principaux mots associés")|>
  cols_label(theme_nom = "Trois premiers mots-clés",
             theme_no = "Numéro du thème")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
noms_themes|>gtsave(filename = "resultats/20221105_PB_table_themes_mots.png")

# Top Words

labelTopics(Third_STM)


# We can find the top documents associated with a topic with the findThoughts function:
# top 2 paragraps for Topic 1 to 10

findThoughts(Third_STM, texts = meta$year,n = 1, topics = 1:39)


# We can look at multiple, or all, topics this way as well. 
# For this we’ll just look at the shorttext.
# top 3 paragraps for Topic #1 to 15

findThoughts(Third_STM, texts = meta$subjects, n = 1, topics = 15)





# # Graphical display of topic correlations
# 
# topic_correlation <- topicCorr(Third_STM)
# str(topic_correlation)
# attributes(topic_correlation)
# str(topic_correlation$poscor)
# 
# plot.topicCorr <- function(x, topics=NULL,
#                            vlabels=NULL, layout=NULL,
#                            vertex.color="white", vertex.label.cex=.65, 
#                            vertex.label.color="black",vertex.size=NULL, ...){
#   if(!requireNamespace("igraph", quietly=TRUE)) stop("Install the igraph package to use this function.")
#   if(is.null(topics)) topics <- 1:nrow(x$posadj)
#   x <- x$posadj[topics, topics]
#   
#   g <- igraph::graph.adjacency(x, mode="undirected", weighted=TRUE, diag=FALSE)
#   if(is.null(vlabels)) vlabels <-  paste("Topic", topics)
#   igraph::E(g)$size <- 1
#   igraph::E(g)$lty <- 2
#   igraph::E(g)$color <- "black"
#   igraph::V(g)$label <- vlabels
#   if(is.null(layout)) layout <- igraph::layout.fruchterman.reingold
#   igraph::plot.igraph(g, layout=layout, vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, 
#                       vertex.label.color=vertex.label.color, vertex.size=vertex.size, ...)
# }
# 
# plot.topicCorr(topic_correlation,
#                vlabels = noms_themes_dt$theme_nom)
# 



# Wordcloud:topic 17 with word distribution
labelTopics(Third_STM)
set.seed(837)
library(wordcloud)
stm::cloud(Third_STM, topic=26, scale=c(5,0.5))




# Working with meta-data 

set.seed(837)
predict_topics <- estimateEffect(formula = 1:10 ~ `year`, 
                               stmobj = Third_STM, 
                               metadata = out$meta, 
                               uncertainty = "Global",
                               prior = 1e-5)


# # Effect of Zacks vs . Seeking Alpha publishers
# 
# set.seed(837)
# plot(predict_topics, covariate = "documentType", topics = c(1,4,10),
#      model = Third_STM, method = "difference",
#      cov.value1 = "Dissertation/Thesis", cov.value2 = "Book",
#      xlab = "Dissertation/Thesis ... Book",
#      main = "Dissertation/Thesis VS Book",
#      xlim = c(-.1, .1), labeltype = "custom",
#      custom.labels = c('Topic 1','Topic 4','Topic 10'))
# 


# # Topic proportions
# # 
# plot(Third_STM, type = "hist", topics = sample(1:20, size = 5))
# plot(Third_STM, type="hist")


# The topicQuality() function plots these values 
# and labels each with its topic number:

topicQuality(model=Third_STM, documents=docs)


############ Visualisation dynamique des thèmes et des mots-clés de chacun #######################
vis_topics <- toLDAvis(Third_STM, docs = out$documents)


########################## Évolution de thèmes
dictionnaire_quebecois <- c("qu[ée]b[ée]c|canadi[ae]n|canada?|montr[ée]al?")

# Création d'une colonne texte net
nettoyage_fun<- function(x){
  x <- tolower(x)
  x <- tm::removeNumbers(x)
  x <- tm::removePunctuation(x)
  x <- tm::removeWords(x, lsa::stopwords_en)
  x <- tm::stemDocument(x)
  return(x)
}

corpus_abs_tr_en[, text_net:=nettoyage_fun(text)]

# Création d'un sous-corpus
corp_quebecois <- corpus_abs_tr_en[text_net %like% dictionnaire_quebecois, .(doc_id, year)]

# Distribution annuelle du corpus total
distrib_annuelle_corpus <- corpus_abs_tr_en[, .N, "year"]

# Distribution annuelle de la sélection + ajout pour relatif
distribution_annuelle_selection <- corp_quebecois[, .N, "year"]

distribution_annuelle_selection <- merge(distribution_annuelle_selection, distrib_annuelle_corpus, by="year", all.x = TRUE)
setnames(distribution_annuelle_selection, new = c("annee", "selection", "integral"))

distribution_annuelle_selection <- distribution_annuelle_selection[-c(1,2,50)]

distribution_annuelle_selection_melt <- melt(distribution_annuelle_selection,
     id.vars = "annee",
     variable.name = "corpus",
     value.name = "total")


ggplot(distribution_annuelle_selection_melt, aes(x=annee, y=total, color=corpus))+
  geom_point(stat="identity")+
  geom_smooth()+
  labs(title = "Distribution chronologique des documents traitant du Québec",
       subtitle = "Topique 14")


ggplot(distribution_annuelle_selection_melt, aes(x=annee, y=total, fill=corpus))+
  geom_bar(stat="identity", position="dodge")+
  geom_smooth()+
  labs(title = "Distribution chronologique des documents traitant du Québec",
       subtitle = "Topique 14")







