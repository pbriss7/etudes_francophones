# Importation des données issues de la recherche sur la francophonie dans la littérature savante (ProQuest)

setwd("~/github/PERSONNEL/Francophonie_ProQuest")
library(data.table)
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
library(xlsx)

# # Importation des fichiers xml
# list.files()
liste_fichiers <- list.files("donnees/plein_texte/FR/", pattern = ".xls")
liste_fichiers_long <- paste0("donnees/plein_texte/FR/", liste_fichiers)

# Lecture des fichiers xml
liste_read <- lapply(liste_fichiers_long, read.xlsx, 1)

# assemblage de la liste en df
corpus <- do.call(bind_rows, liste_read)
setDT(corpus)
corpus <- corpus[!Title %in% c("Éditorial", "Comments", "Conclusions",
"Francophony[ies]", "Multiple Francophones", "Opening Address", "Advertisement 11 -- No Title")]

# Réduction de la structure
corpus <- corpus[!documentType %in% c("review", "Book Review") & !Authors == "[Unknown]" & !duplicated(Title)]
corpus <- corpus[, c("Authors", "Title", "Abstract", "StoreId", "ArticleType",
                         "documentType", "isbn", "language", "languageOfSummary",
                         "year", "pubdate", "classificationCodes","identifierKeywords",
                         "majorClassificationCodes", "subjectClassifications",
                         "subjectTerms", "subjects")]

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
data(world)
world_poly <- spData::world %>% as.data.table()


world_poly <- world_poly %>% select(iso_a2, name_long, continent, geom)

setnames(world_poly, new = c("iso_a2", "nom_pays", "continent", "geom"))

world_poly[, nom_pays:=nom_pays %>% str_replace_all(
  c("Democratic Republic of the Congo"="D\\.?(emocratic)?.?R\\.?(epublic)?.([Oo]f.[Tt]he.)?Congo",
    "Côte d'Ivoire"="(Côte d'Ivoire)|(Ivory Coast)",
    "Republic of the Congo"="Rep\\.?(ublic)?\\s(of\\s)?(the\\s)?Congo",
    "Russian Federation"="(Russian Federation)|(Russia)",
    "The Gambia"="Gambia",
    "Dem. Rep. Korea"="(Dem\\.?|DR|D\\.R(ep)?\\.)(ocratic\\s|\\s(Rep\\.?\\s)|\\s(of\\s)?Korea)\\s?(Republic)?\\s?(of)?(\\sKorea)?(Korea)?|North Korea",
    "Republic of Korea"="(?<!North )Korea\\b"))]

country_regex <- paste0(world_poly$nom_pays, collapse = "\\b|\\b")
country_regex_less_Algeria <- paste0(world_poly$nom_pays[-83], collapse = "\\b|\\b")

# Un exemple de texte où France cooccurre avec un autre nom de pays
corpus[Abstract %ilike% "\\bfrance\\b" & Abstract %ilike% "\\bcanada\\b", .(Authors, Title, year, documentType, isbn, Abstract)][6]

# Un deuxième exemple où on signale la France pour mieux dire qu'on étudie un phénomène hors de France
corpus[Abstract %ilike% "recherche francophone en France, Afrique", .(Authors, Title, year, documentType, isbn, Abstract)][1]


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

corpus_abs_tr_en[, StoreId:=(StoreId/1)]

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
       "donnees/20221106_PB_corpus_traduit_georeference.csv")

# Importation de la structure enrichie
corpus_abs_tr_en_sep <-
  fread("donnees/20221106-PB-corpus-traduit-georeference-separe.csv")


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

pays_melt_N <- pays_melt[, .N, "nom_pays"][order(N, decreasing = T)]
tous_pays_N <- paste(pays_melt_N$nom_pays, pays_melt_N$N, sep = "; ", collapse =" ")

# Liste de tous les continents accompagnés des identifiants
continent_melt <- melt(
  corpus_abs_tr_en_sep,
  id.vars = "StoreId",
  measure.vars = patterns("continent.+"),
  value.name = "nom_continent"
)

continent_melt <-
  continent_melt[!nom_continent == "" & !is.na(nom_continent)]

saveRDS(continent_melt, "donnees/20221111_PB_continents_melt.RDS")

continent_melt[, .N, "nom_continent"][order(N, decreasing = TRUE)]

freq_continent_gt <- gt(continent_melt[, .N, "nom_continent"][order(N, decreasing = TRUE)])|>tab_header(
  title = "Fréquences des noms de continents dans les résumés")|>
  cols_label(nom_continent = "Continents")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
freq_continent_gt|>gtsave(filename = "resultats/20221111_PB_freq_continents_gt.png")



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

continent_melt

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

# Données agrégées
pays_melt_geo_N <- pays_melt_geo[, .N, by=list(nom_pays)][order(N, decreasing = T)]


pays_melt_geo_N[nom_pays=="Brunei", nom_pays:="Brunei Darussalam"]
pays_melt_geo_N[nom_pays=="Dominica", nom_pays:="Dominican Republic"]
pays_melt_geo_N[nom_pays=="French Guiana", nom_pays:="Guyana"]
pays_melt_geo_N[nom_pays=="Gambia", nom_pays:="The Gambia"]
pays_melt_geo_N[nom_pays=="North Korea", nom_pays:="Republic of Korea"]
pays_melt_geo_N[nom_pays=="Russia", nom_pays:="Russian Federation"]
pays_melt_geo_N[nom_pays=="South Korea", nom_pays:="(Dem. Rep. )?Korea"]
pays_melt_geo_N[nom_pays=="Mauritius", nom_pays:="Mauritania"]


pays_poly <- merge(world_poly, pays_melt_geo_N, by.x="nom_pays", by.y= "nom_pays", all.x=FALSE, all.y=TRUE)
pays_poly <- pays_poly[!is.na(iso_a2)]


# Transformer les StoreId de integer64 à numeric
pays_melt[, StoreId:=(StoreId/1)]
uniqueN(pays_melt$StoreId)

store_id_pays_melt_france <- pays_melt[nom_pays == "France", .(StoreId)]
cooc_pays_france <- pays_melt[StoreId %in% store_id_pays_melt_france$StoreId & nom_pays != "France"]

store_id_pays_melt_Canada <- pays_melt[nom_pays == "Canada", .(StoreId)]
cooc_pays_Canada <- pays_melt[StoreId %in% store_id_pays_melt_Canada$StoreId & nom_pays != "Canada"]

store_id_pays_melt_Belgium <- pays_melt[nom_pays == "Belgium", .(StoreId)]
cooc_pays_Belgium <- pays_melt[StoreId %in% store_id_pays_melt_Belgium$StoreId & nom_pays != "Belgium"]

store_id_pays_melt_US <- pays_melt[nom_pays == "United States", .(StoreId)]
cooc_pays_US <- pays_melt[StoreId %in% store_id_pays_melt_US$StoreId & nom_pays != "United States"]


store_id_pays_melt_Algerie <- pays_melt[nom_pays == "Algeria", .(StoreId)]
cooc_pay_Algerie <- pays_melt[StoreId %in% store_id_pays_melt_Algerie$StoreId & nom_pays != "Algeria"]


freq_cooc_pays <- data.table(pays = c("France", "Canada", "Belgique", "États-Unis", "Algérie"),
           freq_documentaire = c(nrow(store_id_pays_melt_france),
                                 nrow(store_id_pays_melt_Canada),
                                 nrow(store_id_pays_melt_Belgium),
                                 nrow(store_id_pays_melt_US),
                                 nrow(store_id_pays_melt_Algerie)),
           cooccurrence = c(length(unique(cooc_pays_france$StoreId)),
                            length(unique(cooc_pays_Canada$StoreId)),
                            length(unique(cooc_pays_Belgium$StoreId)),
                            length(unique(cooc_pays_US$StoreId)),
                            length(unique(cooc_pay_Algerie$StoreId))),
           taux_cooccurrence = c(round(length(unique(cooc_pays_france$StoreId))/nrow(store_id_pays_melt_france), 2),
                                 round(length(unique(cooc_pays_Canada$StoreId))/nrow(store_id_pays_melt_Canada), 2),
                                 round(length(unique(cooc_pays_Belgium$StoreId))/nrow(store_id_pays_melt_Belgium), 2),
                                 round(length(unique(cooc_pays_US$StoreId))/nrow(store_id_pays_melt_US), 2),
                                 round(length(unique(cooc_pay_Algerie$StoreId))/nrow(store_id_pays_melt_Algerie), 2))
           )


freq_cooc_pays_gt <- gt(freq_cooc_pays)|>tab_header(
  title = "Tableau comparatif des cooccurrences de pays dans les résumés")|>
  cols_label(pays = "Pays",
             freq_documentaire = "Fréquence documentaire",
             cooccurrence = "Cooccurre avec autre pays",
             taux_cooccurrence = "Taux de cooccurrence")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
freq_cooc_pays_gt|>gtsave(filename = "resultats/20221106_PB_freq_cooc_pays_gt.png")



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
          label = FALSE,
          alpha.regions = 0.4
          )

# # install.packages("leafem")
# # library(leafem)
# addStaticLabels(mapview_pays, pays_poly_sf[, c("nom_pays", "N", "geom")],
#                 label=pays_poly_sf[, c("nom_pays", "N", "geom")]$N)

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
          label = TRUE,
          alpha.regions = 0.6
  )


mapshot(mapview_continent, url = "resultats/20221105_PB_carte_continents_resumes.html")


# Construction d'un concordancier

all_abstracts <- paste(corpus_abs_tr_en_sep$Abs_tr_en_unaccent, collapse = " ")

library(quanteda)

afrique_corp <- corpus(corpus_abs_tr_en_sep,
                       docid_field = "StoreId",
                       text_field = "Abs_tr_en_unaccent")

afrique_toks <- tokens(afrique_corp)

afrique_kwic <- kwic(afrique_toks, 
     pattern = "\\bAfrica\\b",
     valuetype = "regex",
     case_insensitive = FALSE)

setDT(afrique_kwic)

afrique_kwic_unique <- unique(afrique_kwic, by="docname")
africa_kwic_echantillon <- afrique_kwic_unique[1:25, .(pre, keyword, post)]

africa_kwic_echantillon_gt <- gt(africa_kwic_echantillon)|>tab_header(
  title = "Échantillon des collocations du mot «Africa» dans les résumés")|>
  cols_label(pre = "COLLOCATIONS ANTÉRIEURES",
             keyword = "",
             post = "COLLOCATIONS POSTÉRIEURES")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
africa_kwic_echantillon_gt|>gtsave(filename = "resultats/20221106_PB_collocations_africa_echantillon.png")



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

titres_villes_exemple <- data.table(ville = c("Montréal",
                                              "Bruxelles",
                                              "Dakar"),
  titres= c(corpus_abs_tr_en_sep[Title %like% "Montreal", .(Title)][1],
            corpus_abs_tr_en_sep[Title %like% "Brussels", .(Title)][1],
            corpus_abs_tr_en_sep[Title %like% "Dakar", .(Title)][1]
))


# Trois titres de documents choisis au hasard
titres_villes_exemple_gt <- gt(titres_villes_exemple)|>tab_header(
  title = "Trois exemples de titres avec noms de villes")|>
  cols_label(ville = "Ville",
             titres = "Titre")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
titres_villes_exemple_gt|>gtsave(filename = "resultats/20221105_PB_titres_villes_exemple_gt.png")



############################################## Silhouette du corpus - distribution chronologique
ggplot(corpus_abs_tr_en_sep[, .(StoreId, year)][,.N, by="year"], aes(x=year, y=N))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = N),
            hjust = 0.6,
            vjust = -1,
            size = 1.8,
            colour = "black")+
  theme_classic()+
  labs(title = "Distribution annuelle des notices du corpus",
       subtitle = "Mot de la requête: «francophon*»",
       caption = "Données: ProQuest, 2022")+
  ylab("Nombre de notices")+
  xlab("Années")

ggsave("resultats/20221105_PB_DistribChronologique_corpus_trad_en.png", dpi=300)

str(corpus_abs_tr_en_sep)
corpus_abs_tr_en_sep[, StoreId:=(StoreId/1)]

setnames(corpus_abs_tr_en_sep, old = c("StoreId", "Abstract"), new = c("doc_id", "text"))








############### Note: pour contrôler les paramètres de prétraitement, exécuter ceux-ci préalablement à la fonction textPreprocessor de stm

# corpus_abs_tr_en_sep[, text:=ifelse(Abs_tr_en_unaccent!="", Abs_tr_en_unaccent, Ti_tr_en_unaccent)]

corpus_abs_tr_en_sep_reduit <- corpus_abs_tr_en_sep[Abs_tr_en_unaccent !=""]

saveRDS(corpus_abs_tr_en_sep_reduit, "donnees/20221120_PB_donnees_pour_topicmodels.RDS")


# * default parameters
processed_en <- textProcessor(corpus_abs_tr_en_sep_reduit$Abs_tr_en_unaccent, metadata = as.data.frame(corpus_abs_tr_en_sep_reduit[, .(StoreId, Abs_tr_en_unaccent, ArticleType, documentType, year, identifierKeywords, subjects,`villes 1`, `villes 2`,`villes 3`,`pays 1`,`pays 2`,`pays 3`,`continents 1`,`continents 2`,`continents 3`)]),
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
                           customstopwords = c("article", "articles", "research", "researches",
                                               "issue","issues", "work", "works", "field",
                                               "fields", "analysis", "will", "approach",
                                               "present", "presents", "presentation", "discuss", "discussion",
                                               "discussed", "discussions", "discussing",
                                               "focus", "presents", "reflect","reflects", "context",
                                               "propos", "develop", "develops", "highlight","highlights",
                                               "concept", "part","address", "addresses"), #*
                           v1 = FALSE) #*

saveRDS(processed_en, "donnees/20221120_PB_processed_en.RDS")

# filter out terms that don’t appear in more than 10 documents,
out <- prepDocuments(processed_en$documents, processed_en$vocab, processed_en$meta, lower.thresh=10)

# L'analyse porte au final sur 2988 documents, car 4 n'ont aucun mot

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

str(out)

# Construire un premier modèle
set.seed(831)
system.time({
  First_STM <- stm(docs, vocab, 40,
                   # prevalence =~ s(year),
                   data = meta,
                   seed = 15, 
                   max.em.its = 5
  )
})

# Observation du premier modèle
plot(First_STM)

str(Third_STM)
Third_STM$
TwentyNewsgroups$
# Deuxième modèle
set.seed(832)
system.time({
  Second_STM <- stm(documents = out$documents, vocab = out$vocab,
                    K = 18, 
                    prevalence =~ s(year),
                    max.em.its = 75, 
                    data = out$meta,
                    init.type = "Spectral", 
                    verbose = TRUE
  )
})

# Plot second Topic Model
plot(Second_STM)

# Trouver le nombre idéal de topiques (K) 
set.seed(833)
system.time({
  findingk <- searchK(out$documents, out$vocab, K = c(35:47),
                      prevalence =~ s(year), data = meta, verbose=TRUE
  )
})

# Plot
plot(findingk)
findingk$results[which.max(findingk$result$heldout),]
findingk$results[which.min(findingk$result$residual),]

# Find k: Approach 2
set.seed(834)
system.time({
  findingk_ver2 <- searchK(documents = out$documents, 
                           vocab = out$vocab,
                           K = c(30,40,50,60,70,80), #specify K to try
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

# Run final topic model at 36 topics and see how long it takes
set.seed(836)
system.time({
  Third_STM <- stm(documents = out$documents, vocab = out$vocab,
                   K = 36, 
                   prevalence =~ s(year),
                   max.em.its = 75, 
                   data = out$meta,
                   init.type = "Spectral",
                   verbose = TRUE
  )
})

saveRDS(Third_STM, "donnees/20221120_PB_Third_STM.RDS")

#Plot
png("resultats/20221107_PB_Proportions_36themes_mots_cles.png")
plot(Third_STM,
     main = "36 thèmes des études francophones\nNombre de documents analysés: 2988",
     sub = "\nDonnées: ProQuest, 2022",
     xlab = "Proportions des thèmes"
     )
dev.off()


str(Third_STM)
label_topics_matrix <- labelTopics(Third_STM, n=25)[[1]]

noms_themes_dt <- data.table(theme_no = 1:nrow(label_topics_matrix),
                          theme_nom = paste(label_topics_matrix[, 1], label_topics_matrix[, 2], label_topics_matrix[, 3], sep = "; "))

noms_themes <- gt(noms_themes_dt)|>tab_header(
  title = "Numéro des thèmes et principaux mots associés")|>
  cols_label(theme_nom = "Trois premiers mots-clés",
             theme_no = "Numéro du thème")|>
  tab_source_note(
    source_note = md("Données: ProQuest, 2022")
  )
noms_themes|>gtsave(filename = "resultats/20221107_PB_table_themes_mots.png")

# Top Words

labelTopics(Third_STM)


# We can find the top documents associated with a topic with the findThoughts function:
# top 2 paragraps for Topic 1 to 10

findThoughts(Third_STM, texts = meta$year,n = 1, topics = 1:37)



# # # Graphical display of topic correlations
# # 
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
#   igraph::E(g)$color <- "green"
#   igraph::V(g)$label <- vlabels
#   if(is.null(layout)) layout <- igraph::layout.fruchterman.reingold
#   igraph::plot.igraph(g, layout=layout, vertex.color=vertex.color, vertex.label.cex=vertex.label.cex,
#                       vertex.label.color=vertex.label.color, vertex.size=vertex.size, ...)
# }
# 
# plot.topicCorr(topic_correlation,
#                vlabels = noms_themes_dt$theme_nom)



# Wordcloud:topic 17 with word distribution
labelTopics(Third_STM)
set.seed(837)
library(wordcloud)
stm::cloud(Third_STM, topic=1, scale=c(4,0.5))




# Working with meta-data 

set.seed(837)
predict_topics <- estimateEffect(formula = 1:10 ~ `year`, 
                               stmobj = Third_STM, 
                               metadata = out$meta, 
                               uncertainty = "Global",
                               prior = 1e-5)



############ Visualisation dynamique des thèmes et des mots-clés de chacun #######################
str(toLDAvis(Third_STM,
         docs = out$documents,
         reorder.topics = FALSE))
# devtools::install_github("cpsievert/LDAvis")
# library(LDAvis)

str(out$documents)

stm::cloud(Third_STM, topic=10, scale=c(4,0.3))


########################## Évolution de thèmes







