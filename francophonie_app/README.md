## Applilcation

### Contexte
Cette application utilise les données produites par ProQuest (https://www.proquest.com/). Elle propose une modélisation topique des données textuelles présentées dans Contexte.Rmd et vise à accompagner la présentation "Une cartographie des études francophones" faite le 26 novembre 2022 à l'Université de Chonnam, Gwangju, Corée.

### Installation de R et des extensions
Avant de lancer l’application, il faut installer R (https://cran.r-project.org/), version 4.2.2, l’IDE RStudio (https://posit.co/products/open-source/rstudio/)  ainsi que les extensions ci-dessous avec la fonction `install.packages`:

servr
shiny
LDAvis
data.table
topicmodels
text2vec
markdown

On peut par exemple exécuter la commande ci-dessous dans la console de RStudio:
install.packages(c("servr", "shiny", "LDAvis", "data.table", "topicmodels", "text2vec", "markdown"))

L’activation des extensions est faite automatiquement lors du lancement de l’application. 

### Lancement de l’application
Une fois R et RStudio installés, ouvrir le projet ../Francophonie_ProQuest.Rproj, de même que le script francophonie_app/app.R. Cliquer ensuite sur `Run App` dans le menu contextuel du script app.R. Le modèle topic par défaut apparaitra dans l’application après une dizaine de secondes.

### Configuration détaillée
R version 4.2.2 (2022-10-31)