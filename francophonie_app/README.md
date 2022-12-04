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
R version 4.2.2 (2022-10-31)Platform: aarch64-apple-darwin20 (64-bit)Running under: macOS Monterey 12.6Matrix products: defaultLAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dyliblocale:[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8attached base packages:[1] stats     graphics [3] grDevices utils    [5] datasets  methods  [7] base     other attached packages:[1] markdown_1.4      [2] text2vec_0.6.2    [3] topicmodels_0.2-12[4] data.table_1.14.4 [5] LDAvis_0.3.5      [6] servr_0.25        [7] shiny_1.7.3       loaded via a namespace (and not attached): [1] mlapi_0.1.1            [2] modeltools_0.2-23      [3] lsa_0.73.3             [4] xfun_0.35              [5] bslib_0.4.1            [6] slam_0.1-50            [7] NLP_0.2-1              [8] lattice_0.20-45        [9] SnowballC_0.7.0       [10] htmltools_0.5.3       [11] stats4_4.2.2          [12] yaml_2.3.6            [13] rlang_1.0.6           [14] later_1.3.0           [15] jquerylib_0.1.4       [16] withr_2.5.0           [17] lifecycle_1.0.3       [18] commonmark_1.8.1      [19] fontawesome_0.4.0     [20] proxyC_0.3.3          [21] htmlwidgets_1.5.4     [22] evaluate_0.18         [23] memoise_2.0.1         [24] knitr_1.40            [25] fastmap_1.1.0         [26] httpuv_1.6.6          [27] tm_0.7-9              [28] parallel_4.2.2        [29] Rcpp_1.0.9            [30] xtable_1.8-4          [31] promises_1.2.0.1      [32] DT_0.26               [33] cachem_1.0.6          [34] float_0.3-0           [35] RcppParallel_5.1.5    [36] jsonlite_1.8.3        [37] mime_0.12             [38] rsparse_0.5.1         [39] RhpcBLASctl_0.21-247.1[40] digest_0.6.30         [41] stringi_1.7.8         [42] tidygeocoder_1.0.5    [43] RJSONIO_1.3-1.6       [44] grid_4.2.2            [45] cli_3.4.1             [46] tools_4.2.2           [47] magrittr_2.0.3        [48] sass_0.4.2            [49] proxy_0.4-27          [50] crayon_1.5.2          [51] ellipsis_0.3.2        [52] Matrix_1.5-1          [53] rsconnect_0.8.28      [54] xml2_1.3.3            [55] rmarkdown_2.18        [56] rstudioapi_0.14       [57] stm_1.3.6             [58] lgr_0.4.4             [59] R6_2.5.1              [60] compiler_4.2.2        