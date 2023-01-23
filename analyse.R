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

load("DB_WIKIPEDIA")

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
      caption = "(Source : Historique Wikipédia, 22/01/2023)"
    )
  
    # theme(axis.title.y.right = element_text(angle = 90))
})

###
# Pourcentage de modifications annulées
###

DB_COUNTS <- do.call("rbind", COUNT_DATE)

DB_COUNTS %>% 
  mutate(FL_ANNULATION = sum_abs_diff > mean_taille) %>% 
  group_by(TX_TITLE) %>% 
  summarise(n(),sum(FL_ANNULATION),mean(FL_ANNULATION))


#############
# Questions #
#############

extract_count <- function(sujet,deb,fin){
  COUNT_DATE[[which(DB_SUJETS$TX_TITLE == sujet)]] %>% 
    filter(DT_JOUR >= ymd(deb) & DT_JOUR <= ymd(fin)) %>% 
    print(n=1000)
}
  
extract_histo <- function(sujet,deb,fin){
  LISTE_DB_HISTO[[which(DB_SUJETS$TX_TITLE == sujet)]] %>% 
      filter(DT_JOUR >= ymd(deb) & DT_JOUR <= ymd(fin)) %>% 
      print(n=1000)
  }

extract_count_2 <- function(sujet,abs_diff){
  COUNT_DATE[[which(DB_SUJETS$TX_TITLE == sujet)]] %>% 
    filter(sum_abs_diff >= abs_diff) %>% 
    print(n=1000)
}

graph_specific <- function(sujet,deb,fin){
  
  scale <- 10
  DB <- COUNT_DATE[[which(DB_SUJETS$TX_TITLE == sujet)]] %>% 
    filter(DT_JOUR > ymd(deb) & DT_JOUR < ymd(fin))
  
  max_graph <- max(DB$mean_taille)
  min_graph <- min(DB$mean_taille)
  
  graph_title <- paste0("Évolution de l'article wikipédia '",unique(DB$TX_TITLE),"'")
  
  DB %>%
    mutate(sum_abs_diff = pmin(sum_abs_diff,max_graph)) %>% 
    ggplot(aes(x = DT_JOUR)) +
    # geom_line(aes(y = cumsum(sum_diff)),color ='black') +
    geom_line(aes(y = mean_taille),color ='black') +
    geom_bar( aes(y=sum_abs_diff), stat="identity", size=.1, fill="red", color="red", alpha=.4) +
    geom_line(aes(y = cumsum(n)*scale),color ='black',lty=2) +
    ggtitle(graph_title) +
    scale_x_date(name = "Date de l'article") +
    scale_y_continuous(
      limits = c(0,max_graph),
      name = "Taille de l'article",
      sec.axis = sec_axis(~./scale,name="Nombre cumulé de modifications")) +
    theme_minimal() +
    theme(axis.text.y.right = element_text(color="black")) +
    theme(axis.title.x.bottom = element_text(hjust=0)) +
    theme(axis.title.y.left = element_text(vjust = +3)) +
    theme(axis.title.y.right = element_text(vjust = +3)) +
    labs(caption = "(en rouge la somme des modifications par jour)")
  
}

###
# Pourquoi la France a bondi d'un coup niveau modification cumulée
###

id_sujet <- which(DB_SUJETS$TX_TITLE == "France")

CUM_FRANCE <- COUNT_DATE[[id_sujet]] %>% 
  filter(DT_JOUR > ymd("2015-12-01") & DT_JOUR < ymd("2016-01-31"))

print(CUM_FRANCE,n=50)

# Rien de choquant, bcp d'ajouts dans l'article

CUM_FRANCE <- COUNT_DATE[[id_sujet]] %>% 
  filter(DT_JOUR > ymd("2019-06-01") & DT_JOUR < ymd("2019-11-31"))

print(CUM_FRANCE,n=50)

# Pareil, rien de très choquant

###
# Pourquoi la Belgique a grimpé puis redescendu
###

id_sujet <- which(DB_SUJETS$TX_TITLE == "Belgique")

CUM_BELGIQUE <- COUNT_DATE[[id_sujet]] %>% 
  filter(DT_JOUR > ymd("2019-01-01") & DT_JOUR < ymd("2019-12-31"))

print(CUM_BELGIQUE,n=100)

