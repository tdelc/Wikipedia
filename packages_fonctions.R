################################################
#   WIKIPEDIA : D'un sujet d'actualité à un    #
#   une entrée encyclopédique                  #
#   Introduction à l'usage du Webscraping      # 
#   Thomas Delclite, Statbel                   #
################################################

############
# Packages #
############

options(scipen=10000)

###
# Installation des packages
###

# install.packages('tidyverse')
# install.packages('httr')
# install.packages('rvest')
# install.packages('rstudioapi')
# install.packages("progress")

###
# Chargements des packages
###

library(tidyverse)
library(httr)
library(rvest)
library(rstudioapi)
library(scales)
library(lubridate)
library(R.temis)
library(progress)

#############
# Fonctions #
#############

###
# Récupérer les pages html sauvegardés sur PC
###

recup_pages_html <- function(DB_SUJETS){
  LISTE_PAGES_WEB <- list(
    BASE = map(DB_SUJETS$TX_URL, read_html),
    HISTO =  map(DB_SUJETS$TX_URL_HIST, read_html),
    STATS = map(DB_SUJETS$TX_URL_STATS, read_html)
  )
  
  PAGES_BASE <- DB_SUJETS$TX_TITLE %>% map(~{
    read_html(paste0("pages_html/articles/",.x))
  })
  
  PAGES_HISTO <- DB_SUJETS$TX_TITLE %>% map(~{
    read_html(paste0("pages_html/historiques/",.x))
  })
  
  PAGES_STATS <- DB_SUJETS$TX_TITLE %>% map(~{
    read_html(paste0("pages_html/stats/",.x))
  })
  
  
  PAGES_HISTO_DETAILS <- DB_SUJETS$TX_TITLE %>% map(~{
    liste_histo <- list.files("pages_html/details_historiques",pattern=.x)
    liste_histo %>% map(~{
      read_html(paste0("pages_html/details_historiques/",.x))
    })
  })
  
  LISTE_PAGES_WEB <- list(BASE=PAGES_BASE,
                          HISTO=PAGES_HISTO,
                          STATS = PAGES_STATS,
                          DETAILS_HISTO = PAGES_HISTO_DETAILS)
  
  LISTE_PAGES_WEB
}


###
# Découpage d'une page wiki de base
###

# Objectifs :
#   - Récupération du titre, du texte complet, du texte découpé par H2

extract_page_base <- function(PAGE_HTML){
  
  TX_TITLE <- PAGE_HTML %>% html_element("span.mw-page-title-main") %>% html_text()
  
  TX_TEXT <- paste(PAGE_HTML %>% html_elements("div#mw-content-text > div > p") %>% html_text(),collapse = " ")
  
  TX_TEXT <- str_remove_all(TX_TEXT,"\\[[0-9]+\\]")
  TX_TEXT <- str_squish(TX_TEXT)
  
  tibble(TX_TITLE,TX_TEXT)
}

decoupage_page_base <- function(PAGE_HTML){

  DB_ARTICLE <- tibble(
    TX_H2 = character(),
    TX_P = character()
  )
  
  article <- PAGE_HTML %>% 
    html_elements("div#mw-content-text > div > *")
  
  P <- NULL
  H2 <- "Introduction"
  for (i in 1:length(article)){
    if (html_name(article[[i]]) == "p")
      P <- paste(P,html_text(article[[i]])," ")
    
    if (html_name(article[[i]]) == "h2"){
      DB_ARTICLE <- DB_ARTICLE %>% add_row(
        TX_H2 = H2,
        TX_P = P
      )
      H2 <- html_text(article[[i]] %>% html_element(".mw-headline"))
      P <- NULL
    }
  }
  # Dernière entrée
  DB_ARTICLE <- DB_ARTICLE %>% add_row(TX_H2 = H2,TX_P = P)
  
  # Supprimer les notes de bas de pages
  DB_ARTICLE$TX_P <- str_remove_all(DB_ARTICLE$TX_P,"\\[[0-9]+\\]")
  DB_ARTICLE$TX_P <- str_squish(DB_ARTICLE$TX_P)
  
  DB_ARTICLE
}

encart_page_base <- function(PAGE_HTML){

  tables <- NULL
  
  encart <- PAGE_HTML %>% 
    html_element(xpath = "//*[contains(@class,'infobox')]")
  
  if (length(encart) > 0){
    tables <- encart %>% html_table()
    colnames(tables) <- c("TX_CATEGORIE","TX_VALEUR")
  }
  tables
}

###
# Découpages des informations de l'historique
###

decoupage_page_histo <- function(PAGE_HTML){
  TX_TITLE = PAGE_HTML %>% 
    html_elements("h1#firstHeading") %>% 
    html_text() %>% 
    str_extract("«.*»") %>% 
    str_remove_all("«|»") %>% 
    str_squish()
  
  ID = PAGE_HTML %>% html_elements("li[data-mw-revid]") %>% html_attr("data-mw-revid")
  TX_DATE = PAGE_HTML %>% html_elements(".mw-changeslist-date") %>% html_text()
  TX_AUTEUR = PAGE_HTML %>% html_elements(".mw-userlink") %>% html_text()
  # TX_TAILLE = PAGE_HTML %>% html_elements(".mw-diff-bytes.history-size") %>% html_attr("data-mw-bytes")
  TX_TAILLE = PAGE_HTML %>% html_elements(".mw-diff-bytes[data-mw-bytes]") %>% html_attr("data-mw-bytes")
  TX_MODIF = PAGE_HTML %>% html_elements(".mw-diff-bytes[dir]") %>% html_text()
  TX_COMMENTAIRES = (PAGE_HTML %>% html_elements("span.comment") %>% html_text())[
    PAGE_HTML %>% 
      html_elements("span.comment") %>% 
      html_attr("class") != "history-deleted comment"]
  
  TX_TAGS <- PAGE_HTML %>% html_elements("li[data-mw-revid]") %>% 
    map_chr(~{
      .x %>% html_element("span.mw-tag-markers") %>% html_text()
    })
  
  tibble(TX_TITLE,ID,TX_DATE,TX_AUTEUR,TX_TAILLE,TX_MODIF,TX_COMMENTAIRES,TX_TAGS)
}


ajout_details_histo <- function(PAGE_HTML){
  
  URL <- PAGE_HTML %>% html_element("div#mw-diff-ntitle1 >*> a") %>% html_attr("href")
  ID <- str_extract(URL,"\\d+$")
  
  TX_INS <- paste(PAGE_HTML %>% html_elements("ins") %>% html_text() %>% unlist(),collapse = " ; ")
  
  TX_DEL <- paste(PAGE_HTML %>% html_elements("del") %>% html_text() %>% unlist(),collapse = " ; ")
  
  tibble(ID,TX_INS,TX_DEL)
  
}


