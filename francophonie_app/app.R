# Application Shiny proposant un modèle de topics à partir d'un corpus constitué de 2545 publications savantes sur la francophonie

if(!"shiny" %in% rownames(installed.packages())){install.packages("shiny")}
if(!"servr" %in% rownames(installed.packages())){install.packages("servr")}
# install.packages("xfun", type="binary")
library(servr)
library(shiny)
library(LDAvis)
library(wordcloud)
library(data.table)
library(topicmodels)
library(text2vec)
library(stringr)
library(rmarkdown)

# - Importation des données ----
# Note: les chemins doivent renvoyer à ceux contenus à l'intérieur de l'application.
dmt <- readRDS("donnees/20221121_PB_dtm.RDS")
corpus_abs_tr_en_sep_reduit <- readRDS("donnees/20221121_PB_corpus_abs_tr_en_sep_reduit.RDS")

# - Traces du processus ----

# corpus_abs_tr_en_sep_reduit <- readRDS("donnees/20221120_PB_donnees_pour_topicmodels.RDS")

# prep_fun = function(x) {
#   # make text lower case
#   x = str_to_lower(x)
#   # remove non-alphanumeric symbols
#   x = str_replace_all(x, "[^[:alpha:]]", " ")
#   # collapse multiple spaces
#   x = str_replace_all(x, "\\s+", " ")
# }

# corpus_abs_tr_en_sep_reduit$text <- prep_fun(corpus_abs_tr_en_sep_reduit$Abs_tr_en_unaccent)


# saveRDS(corpus_abs_tr_en_sep_reduit, "~/github/PERSONNEL/Francophonie_ProQuest/francophonie_app/donnees/20221121_PB_corpus_abs_tr_en_sep_reduit.RDS")



# - Application ----
ui <- navbarPage(
  title = "Topics des études francophones",
  
  tabPanel("Modèle", 
           fluidPage(
             
             headerPanel(""),
             # titlePanel(p(h2("Topics des études francophones",style = "color:#4d3a7d"))),
             
             #sidebarPanel(
             wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), style = "background: lightgrey",
                       id = "leftPanel",
                       sliderInput("nTopics", "Nbre de topics à afficher", min = 6, max = 48, value = 36, step=6),
                       sliderInput("nTerms", "Nbre de mots par topic", min = 10, max = 50, value = 20, step=5),
                       tags$hr(),
                       actionButton(inputId = "GoButton", label = "Rafraîchir",  icon("refresh"))
             ),
             mainPanel( 
               tabPanel("", hr(),helpText(h2("Choisissez un topic")),  visOutput('visChart')))
           )
  ),
  tabPanel("Graphiques"
           # ,
           # includeMarkdown("Graphiques.Rmd")
           ),
  tabPanel("Documentation",
           includeMarkdown("Readme.Rmd")
           )
)


server <- shinyServer(function(input, output, session) {
  Topic_Subset <- reactive({
    
    it = itoken(corpus_abs_tr_en_sep_reduit$text, progressbar = FALSE)
    v = create_vocabulary(it)
    v = prune_vocabulary(v, doc_proportion_max = 0.2, term_count_min = 10)
    setDT(v)
    v <- v[!term %in% lsa::stopwords_en & !term %in% lsa::stopwords_fr & !term %in% c("article", "articles", "research", "researches",
                                                                                      "issue","issues", "work", "works", "field",
                                                                                      "fields", "analysis", "will", "approach",
                                                                                      "present", "presents", "presentation", "discuss", "discussion",
                                                                                      "discussed", "discussions", "discussing",
                                                                                      "focus", "presents", "reflect","reflects", "context",
                                                                                      "propos", "develop", "develops", "highlight","highlights",
                                                                                      "concept", "part","address", "addresses")]
    vectorizer = vocab_vectorizer(v)
    dtm = create_dtm(it, vectorizer, type = "dgTMatrix")
    
    docs <- corpus_abs_tr_en_sep_reduit$text
    nTopics <- input$nTopics
    
    lda_model = text2vec::LDA$new(n_topics = nTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)
    
    return(lda_model) # 
  })
  
  output$visChart <- renderVis({
    
    input$GoButton
    
    isolate({
      nterms    <- input$nTerms
      lda_model <- Topic_Subset()
    })
    
    lda_model$plot(out.dir = "./results", R = nterms, open.browser = FALSE)
    
    readLines("./results/lda.json")
    
  })
})

shinyApp(ui = ui, server = server)
