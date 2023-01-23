################################################
#   WIKIPEDIA : D'un sujet d'actualité à un    #
#   une entrée encyclopédique                  #
#   Introduction à l'usage du Webscraping      # 
#   Thomas Delclite, Statbel                   #
################################################

###########
# Sources #
###########

source("packages_fonctions.R")

############################
# Liste des sujets de base #
############################

###
# Création de la DB des sujets
###

DB_SUJETS <- tibble(
  TX_TITLE = character(),
  TX_URL = character()
)

###
# Exemples bidons, mais pour avoir une liste
###

# DB_SUJETS <- DB_SUJETS %>% add_row(
#   TX_TITLE = "Casque de Sutton Hoo",
#   TX_URL = "https://fr.wikipedia.org/w/index.php?title=Casque_de_Sutton_Hoo"
# )

# DB_SUJETS <- DB_SUJETS %>% add_row(
#   TX_TITLE = "Curgies",
#   TX_URL = "https://fr.wikipedia.org/w/index.php?title=Curgies"
# )

DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "France")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Belgique")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Allemagne")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Espagne")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Italie")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Russie")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Autriche")

DB_SUJETS <- DB_SUJETS[0,]
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Hydroxychloroquine")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Didier_Raoult")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Réchauffement_climatique")

DB_SUJETS$TX_URL <- paste0("https://fr.wikipedia.org/w/index.php?title=",DB_SUJETS$TX_TITLE)

DB_SUJETS$TX_URL <- URLencode(DB_SUJETS$TX_URL)

###
# Génération des variables 
###

# Langue
TEMP_URL <- str_remove(DB_SUJETS$TX_URL,"https://")
DB_SUJETS$TX_LANGUE <- str_remove(TEMP_URL,".wikipedia.*")

# URL de base
DB_SUJETS$TX_URL_BASE <- str_remove(DB_SUJETS$TX_URL,"/w/.*")

# URL historique
DB_SUJETS$TX_URL_HIST <- paste0(DB_SUJETS$TX_URL,"&action=history&limit=5000")

# URL de stats
TEMP_URL <- str_extract(DB_SUJETS$TX_URL,"title=.*")
TEMP_URL <- str_remove(TEMP_URL,"title=")

DB_SUJETS$TX_URL_STATS <-paste0("https://xtools.wmflabs.org/articleinfo/fr.wikipedia.org/",TEMP_URL)

####################
# Scraping initial #
####################

# Articles de base
PAGES_BASE <- map(DB_SUJETS$TX_URL, read_html)

lapply(1:length(PAGES_BASE), function(i)
  xml2::write_html(PAGES_BASE[[i]],file=paste0("pages_html/articles/",DB_SUJETS$TX_TITLE[i]))
)

# Historique des articles
PAGES_HISTO <- map(DB_SUJETS$TX_URL_HIST, read_html)

lapply(1:length(PAGES_HISTO), function(i)
  xml2::write_html(PAGES_HISTO[[i]],file=paste0("pages_html/historiques/",DB_SUJETS$TX_TITLE[i]))
)

# Statistiques des articles
PAGES_STATS <- map(DB_SUJETS$TX_URL_STATS, read_html)

lapply(1:length(PAGES_STATS), function(i)
  xml2::write_html(PAGES_STATS[[i]],file=paste0("pages_html/stats/",DB_SUJETS$TX_TITLE[i]))
)

#####################################
# Scraping avancé pour l'historique #
#####################################

URL_HISTO <- map(PAGES_HISTO,~{
  URL <- .x %>% 
        html_elements("li[data-mw-revid] > span.mw-history-histlinks") %>% 
        html_element(xpath = "./span[2]") %>% 
        html_element(xpath = "a") %>% 
        html_attr("href")
      
      URL <- URL[!is.na(URL)]
      
      TEMP_LANGUE <- .x %>% html_attr("lang")
      
      URL <- paste0("https://",TEMP_LANGUE,".wikipedia.org",URL)
      
      #A SUPPRIMER !
      URL <- URL[1:10]
      
      TX_TITLE <- URL %>% 
        str_extract("title=.*?&diff") %>% 
        str_remove("title=") %>% 
        str_remove("&diff")

      ID <- str_extract(URL,"\\d+$")
      
      tibble(TX_TITLE,URL,ID)
    }
)

pb <- progress_bar$new(
  format = "Downloading :what [:bar] :percent eta: :eta",
  total = sum(unlist(lapply(URL_HISTO,function(x) nrow(x)))), 
  clear = FALSE, width= 60)

PAGES_HISTO_DETAILS <- map(URL_HISTO,~{
  etape <- unique(.x$TX_TITLE)
  WEB_HISTO <- map(.x$URL, ~{
    pb$tick(tokens = list(what = etape))
    read_html(.x)
  })
  names(WEB_HISTO) <- .x$ID
  WEB_HISTO
})

lapply(1:length(PAGES_HISTO_DETAILS), function(i){
  name <- DB_SUJETS$TX_TITLE[i]
  details <- names(PAGES_HISTO_DETAILS[[i]])
  lapply(1:length(PAGES_HISTO_DETAILS[[i]]), function(k){
    name_file <- paste0("pages_html/details_historiques/",name,"_",details[k])
    xml2::write_html(PAGES_HISTO_DETAILS[[i]][[k]],file=name_file)
  })
})

###########################
# Sauvegarde du scrapping #
###########################

save(DB_SUJETS,file="DB_SUJETS")

