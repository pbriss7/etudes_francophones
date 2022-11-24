---
output: html_document
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Contexte

Ce tableau de bord utilise les données produites par [ProQuest](https://www.proquest.com/). Il vise à accompagner la présentation "Une cartographie des études francophones" faite le 26 novembre 2022 à l'Université de Chonnam, Gwangju, Corée.

### Données utilisées
Les données ont été rassemblées à travers ProQuest le *** octobre 2022. Elles forment un corpus de 2995 documents, rassemblé au moyen d'une requête lancée dans les champs d'indexation "Sujet" de *** bases de données. Le motif de la requête est "francophon*". Le résultat a été limité aux publications savantes, soit (mémoires et thèses, livres, articles, textes de conférences). Les distributions chronologique et géographique du corpus ont été faite à partir des 2995 notices qui le composent. Le modèle des topics a été fait à partir des résumés des 24** résumés disponibles (**% du corpus), après traduction vers l'anglais de *** d'entre eux. La traduction a été faite avec l'API de Deepl.

### Prétraitement
Les mots présents dans plus de 20% du corpus ont été éliminés, de même que les mots dont la fréquence était inférieure à 10 occurrences dans l'ensemble du corpus. Les antidictionnaires anglais et français de l'extension lsa de R ont été utilisés pour supprimer des mots fonctionnels. On a également éliminé d'autres mots fréquents, typiques de ce corpus spécifique: "article", "articles", "research", "researches","issue","issues", "work", "works", "field","fields", "analysis", "will", "approach", "present", "presents", "presentation", "discuss", "discussion", "discussed", "discussions", "discussing", "focus", "presents", "reflect","reflects", "context", "propos", "develop", "develops", "highlight","highlights", "concept", "part","address", "addresses".

### Annotation du corpus
Les noms de villes, de pays et de continents ont été géoréférencés avec l'API de Nominatim. 

### Statistiques, graphiques et application
Les calculs statistiques, les graphiques et l'application ont été réalisés dans Posit, un environnement de développement intégré soutenu par la Société RStudio. L'application est écrite en langage R et le code peut être consulté à l'adresse suivante: github****

### Extensions R utilisées
servr
shiny
LDAvis
wordcloud
data.table
topicmodels
text2vec
stringr
rmarkdown

Pour toute question:
Pascal.brissette@mcgill.ca

Ont contribué au moissonnage des données et à la traduction des résumés: Yu Shen Shi (U. McGill) et Amélie Ducharme (U. McGill


Bibliographie

https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf