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
# Exemples
###

DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "France")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Belgique")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Allemagne")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Espagne")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Italie")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Russie")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Autriche")

DB_SUJETS <- DB_SUJETS[0,]
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Elon_Musk")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Donald_Trump")
DB_SUJETS <- DB_SUJETS %>% add_row(TX_TITLE = "Mark_Zuckerberg")

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

# save(DB_SUJETS,file="objets_R/DB_SUJETS2")

#####################
# Création de la DB #
#####################

source("packages_fonctions.R")

load("objets_R/DB_SUJETS")
DB_SUJETS_PAYS <- DB_SUJETS
load("objets_R/DB_SUJETS2")
DB_SUJETS_HOMME <- DB_SUJETS

DB_SUJETS <- DB_SUJETS_PAYS %>% add_row(DB_SUJETS_HOMME)

LISTE_PAGES_WEB <- recup_pages_html(DB_SUJETS)

###########################
# Pages de chaque article #
###########################

# DB simple Titre + Texte
DB_PAGES <- map_df(LISTE_PAGES_WEB$BASE,extract_page_base)

# Liste de DB avec le détails de chaque article par H2
LISTE_DB_DETAILS_PAGES <- map(LISTE_PAGES_WEB$BASE,decoupage_page_base)

# Liste de DB avec l'encart synthétique de chaque article
LISTE_DB_ENCART_PAGES <- map(LISTE_PAGES_WEB$BASE,encart_page_base)

######################
# Pages d'historique #
######################

LISTE_DB_HISTO <- map(LISTE_PAGES_WEB$HISTO,decoupage_page_histo)

# Ajouts des textes modifiés
TEMP <- map(LISTE_PAGES_WEB$DETAILS_HISTO,~{
  map_df(.x,ajout_details_histo)
})

LISTE_DB_HISTO <- lapply(1:length(LISTE_DB_HISTO),function(i){
  left_join(LISTE_DB_HISTO[[i]],TEMP[[i]],by="ID")
})

###
# Ajouter des variables
###

# Flag de révocation
LISTE_DB_HISTO <- LISTE_DB_HISTO %>% map(~{
  .x %>% 
    mutate(FL_REVOQUE = 
             str_detect(str_to_lower(TX_TAGS),"révocation|annulation") | 
             str_detect(str_to_lower(TX_COMMENTAIRES),"révocation|annulation"
             )
    )
})

###
# Formatage
###

# Taille de la modification
LISTE_DB_HISTO <- LISTE_DB_HISTO %>% map(~{
  .x %>% 
    mutate(MS_MODIF = str_remove_all(TX_MODIF,"\u00A0")) %>% 
    mutate(MS_MODIF = str_replace(MS_MODIF,"−","-")) %>% 
    mutate(MS_MODIF = as.numeric(MS_MODIF)) %>% 
    mutate(MS_TAILLE = as.numeric(TX_TAILLE)) 
  
})

# Jour
LISTE_DB_HISTO <- LISTE_DB_HISTO %>% map(~{
  .x %>% 
    mutate(TX_JOUR = unlist(lapply(str_split(TX_DATE," "),
                                   function(x) paste(x[1:3],collapse = " ")))) %>% 
    mutate(DT_JOUR = dmy(TX_JOUR))
})


##############
# Sauvegarde #
##############

save(DB_SUJETS,DB_PAGES,
     LISTE_DB_DETAILS_PAGES,
     LISTE_DB_HISTO,
     file = "objets_R/DB_WIKIPEDIA")


###########
# Sources #
###########

COUNT_DATE <- LISTE_DB_HISTO %>% map(~{
  .x %>% 
    filter(!is.na(DT_JOUR)) %>%
    mutate(MS_YEAR = year(DT_JOUR)) %>% 
    # filter(!is.na(TX_TAILLE)) %>% 
    group_by(TX_TITLE,DT_JOUR) %>% 
    summarise(n=n(),
              mean_taille = mean(MS_TAILLE),
              sd_taille = sd(MS_TAILLE),
              sum_diff = sum(MS_MODIF),
              sum_abs_diff = sum(abs(MS_MODIF))
    ) %>% 
    ungroup()
  # %>% 
  #   mutate(sum_diff = ifelse(is.na(sum_diff),0,sum_diff)) %>% 
  #   mutate(cum_sum_diff = cumsum(sum_diff))
})

lapply(1:length(COUNT_DATE),function(i) {
  
  scale <- 10
  
  max_graph <- max(COUNT_DATE[[i]]$mean_taille)
  
  graph_title <- paste0("Évolution de l'article wikipédia '",DB_SUJETS$TX_TITLE[i],"' en ",DB_SUJETS$TX_LANGUE[i])
  
  COUNT_DATE[[i]] %>%
    mutate(sum_abs_diff = pmin(sum_abs_diff,max_graph)) %>% 
    ggplot(aes(x = DT_JOUR)) +
    # geom_line(aes(y = cumsum(sum_diff)),color ='black') +
    geom_line(aes(y=mean_taille),color='black',lwd=1) +
    geom_bar( aes(y=sum_abs_diff), stat="identity", size=.1, color="orange", alpha=.8) +
    # geom_line(aes(y = cumsum(n)*scale),color ='black',lty=2) +
    # ggtitle(graph_title) +
    scale_x_date(name = "Date de l'article",
                 date_breaks = "2 years",
                 date_labels = "%Y"
    ) +
    scale_y_continuous(
      labels=label_number(big.mark = ".",decimal.mark = ","),
      limits = c(0,max_graph),
      name = "Taille de l'article (octets)",
      # sec.axis = sec_axis(~./scale,name="Nombre cumulé de modifications")
    ) +
    theme_minimal() +
    theme(axis.text.y.right = element_text(color="black")) +
    # theme(axis.title.x.bottom = element_text(hjust=0)) +
    theme(axis.title.y.left = element_text(vjust = +3)) +
    # theme(axis.title.y.right = element_text(vjust = +3)) +
    labs(
      title = graph_title,
      subtitle = "(en orange la somme des modifications par jour)",
      caption = "(Source : Historique Wikipédia, 26/01/2025)"
    )
  
  # theme(axis.title.y.right = element_text(angle = 90))
})

