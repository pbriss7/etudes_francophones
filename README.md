## Études francophones

### Contexte
Ce projet R utilise les données produites par ProQuest (https://www.proquest.com/). Il accompagne la présentation "Une cartographie des études francophones" faite le 26 novembre 2022 à l'Université de Chonnam, Gwangju, Corée.

### Données utilisées
Les données ont été rassemblées à travers ProQuest le 26 octobre 2022. Elles forment un corpus de 2992 documents, composé au moyen d'une requête lancée dans les champs d'indexation "Sujet" de 174 bases de données. Le motif de la requête est "francophon\*". Le résultat a été limité aux publications savantes (mémoires et thèses, livres, articles de fond, textes de conférences). Les distributions chronologique et géographique du corpus ont été faites à partir des 2992 notices qui le composent. Le modèle des topics a été fait à partir des 2545 résumés disponibles (85% du corpus), après traduction vers l'anglais de 607 d'entre eux. La traduction a été faite avec l'API de Deepl ("https://www.deepl.com/docs-api").

### Langage et IDE
Les scripts sont écrits en langage R, version 4.2.2. Pour l’exécuter, il faut au préalable installer R (https://cran.r-project.org/). L’environnement de développement intégré (IDE)  POSIT RStudio (https://posit.co/) a été utilisé.

### Prétraitement
Le script code/20221023_PB_franco_ProQuest_en_lieux_topic.R expose le prétraitement du corpus. La majeure partie du nettoyage des données (corrections et uniformisation de titres) et quelques opérations de prétraitement indiqués dans le script ont été faites dans OpenRefine (https://openrefine.org/).

### Annotation du corpus
Les noms de villes, de pays et de continents ont été géoréférencés avec l'API de Nominatim ("https://nominatim.org/"), à l'aide de l'extension tidygeocoder. 

### Pour toute question
pascal.brissette@mcgill.ca

### Contributions
Ont contribué au moissonnage des données et à la traduction des résumés: Yu Shen Shi (U. McGill) et Amélie Ducharme (U. McGill)

### Informations détailléeés de la session
R version 4.2.2 (2022-10-31)Platform: aarch64-apple-darwin20 (64-bit)Running under: macOS Monterey 12.6Matrix products: defaultLAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dyliblocale:[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8attached base packages:[1] stats     graphics [3] grDevices utils    [5] datasets  methods  [7] base     other attached packages:[1] markdown_1.4      [2] text2vec_0.6.2    [3] topicmodels_0.2-12[4] data.table_1.14.4 [5] LDAvis_0.3.5      [6] servr_0.25        [7] shiny_1.7.3       loaded via a namespace (and not attached): [1] sass_0.4.2             [2] tidyr_1.2.1            [3] jsonlite_1.8.3         [4] RhpcBLASctl_0.21-247.1 [5] bslib_0.4.1            [6] RcppParallel_5.1.5     [7] assertthat_0.2.1       [8] lgr_0.4.4              [9] stats4_4.2.2          [10] yaml_2.3.6            [11] slam_0.1-50           [12] pillar_1.8.1          [13] lattice_0.20-45       [14] glue_1.6.2            [15] digest_0.6.30         [16] promises_1.2.0.1      [17] htmltools_0.5.3       [18] httpuv_1.6.6          [19] Matrix_1.5-1          [20] lsa_0.73.3            [21] tm_0.7-9              [22] rsparse_0.5.1         [23] pkgconfig_2.0.3       [24] purrr_0.3.5           [25] xtable_1.8-4          [26] fontawesome_0.4.0     [27] later_1.3.0           [28] tibble_3.1.8          [29] proxy_0.4-27          [30] generics_0.1.3        [31] ellipsis_0.3.2        [32] DT_0.26               [33] cachem_1.0.6          [34] withr_2.5.0           [35] NLP_0.2-1             [36] cli_3.4.1             [37] RJSONIO_1.3-1.6       [38] magrittr_2.0.3        [39] crayon_1.5.2          [40] mime_0.12             [41] memoise_2.0.1         [42] evaluate_0.18         [43] tokenizers_0.2.3      [44] float_0.3-0           [45] fansi_1.0.3           [46] SnowballC_0.7.0       [47] xml2_1.3.3            [48] rsconnect_0.8.28      [49] tools_4.2.2           [50] proustr_0.4.0         [51] lifecycle_1.0.3       [52] stringr_1.4.1         [53] stm_1.3.6             [54] compiler_4.2.2        [55] jquerylib_0.1.4       [56] proxyC_0.3.3          [57] mlapi_0.1.1           [58] rlang_1.0.6           [59] grid_4.2.2            [60] attempt_0.3.1         [61] rstudioapi_0.14       [62] htmlwidgets_1.5.4     [63] rmarkdown_2.18        [64] DBI_1.1.3             [65] R6_2.5.1              [66] knitr_1.40            [67] dplyr_1.0.10          [68] fastmap_1.1.0         [69] utf8_1.2.2            [70] commonmark_1.8.1      [71] tidygeocoder_1.0.5    [72] modeltools_0.2-23     [73] stringi_1.7.8         [74] parallel_4.2.2        [75] Rcpp_1.0.9            [76] vctrs_0.5.0           [77] tidyselect_1.2.0      [78] xfun_0.35             
