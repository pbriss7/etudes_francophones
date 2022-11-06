#### Composition d'un corpus JSTOR (Constellate) sur les études françaises/French Studies
setwd("~/github/PERSONNEL/Francophonie_ProQuest")
library(data.table)
library(xlsx)
library(dplyr)
library(stringr)
library(ggplot2)


liste_fichiers <- list.files("zone_index")
liste_fichiers_long <- paste0("zone_index/", liste_fichiers)

liste_read <- lapply(liste_fichiers_long, read.xlsx, 1)


notices <- do.call(bind_rows, liste_read)

setDT(notices)
notices_red <- notices[subjects %ilike% "francophon" | subjectTerms %ilike% "francophon"]

table(notices_red$language)

notices_red[, year:=as.integer(year)]


slicing <- function(x){
  decennies <- paste0(str_sub(x, 1,3),0)
  return(as.integer(decennies))
}

ggplot(notices_red[, .(StoreId, year)][, decennies:=slicing(year)][,.N, by="decennies"], aes(x=decennies, y=N))+
  geom_col()

ggplot(notices_red[, .(StoreId, year)][,.N, by="year"], aes(x=year, y=N))+
  geom_point()+
  geom_smooth()
