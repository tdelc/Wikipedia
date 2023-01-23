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

load("objets_R/DB_WIKIPEDIA")

LISTE_PAGES_WEB <- recup_pages_html(DB_SUJETS)

###########################
# Pages de chaque article #
###########################

###
# DB simple Titre + Texte
###

DB_PAGES <- map_df(LISTE_PAGES_WEB$BASE,extract_page_base)

###
# Liste de DB avec le détails de chaque article par H2
###

LISTE_DB_DETAILS_PAGES <- map(LISTE_PAGES_WEB$BASE,decoupage_page_base)

###
# Liste de DB avec l'encart synthétique de chaque article
###

LISTE_DB_ENCART_PAGES <- map(LISTE_PAGES_WEB$BASE,encart_page_base)


######################
# Pages d'historique #
######################

LISTE_DB_HISTO <- map(LISTE_PAGES_WEB$HISTO,decoupage_page_histo)

###
# Ajouts des textes modifiés
###

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
