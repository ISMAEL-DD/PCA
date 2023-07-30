# Importation d'un fichier atp_matches_* du dépôt tennis_atp ----

## Important : le dépôt tennis_atp est supposé être au même niveau de l'aroboresence que le dépôt dashboard_atp_tour

#### Configuration préliminaire ####
path <- "../tennis_atp/" # Chemin relatif vers le dépôt tennis_atp depuis le dépôt dashboard_atp_tour
lst_files <- list.files(path = path) # Liste des fichiers présents
lst_atp_matches <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst_files, value = TRUE) # Liste des noms de fichiers de la forme atp_matches_xyzt

# Choix arbitraire d'un fichier à importer (exemple)
set.seed(654) # On fixe une graine
fichier <- sample(lst_atp_matches, size = 1L) # Exemple arbitraire : on choisit un fichier au hasard

#### Importation unique fichier ####

library("readr")
tmp <- read_csv(file = paste0(path, fichier))
# Attention : avant ...., les colonnes minutes, w_ace, etc, ne sont pas renseignées et sont importées en tant que logical.
rm(fichier) # Suppression variable fichier pour éviter tout risque de confusion par la suite

# Importation et fusion de tous les fichiers ----

#### Importation de tous les fichiers sous forme d'une liste de tibbles

library("purrr") # Pour les fonctions map et reduce
lst_tibs <- map(.x = lst_atp_matches,
                .f = function(fichier) read_csv(file = paste0(path, fichier),
                                                show_col_types = FALSE))
names(lst_tibs) <- stringr::str_sub(lst_atp_matches, start = 1L, end = -5L)

#### Visualisation du type de chaque colonne dans les fichiers

# COnstruction matrice des types de colonnes
types <- t(sapply(lst_tibs, \(x) map_chr(x, typeof)))

# Quels sont les colonnes présentant plusieurs types au fil des ans ?
library("dplyr") # Pour transofmation des données
# types %>%
#   as_tibble() %>%
#   select(where(~ length(unique(.x)) >=2)) %>%
#   View()

## Attention : 
## Dans certains matchs de 2022, les champs seed et entry ont été fusionné.
## Les champs minutes et stats de jeu des matches de 1968 à 1990 ne sont pas renseignés et importés en tant que logical

#### Correction de la fusion des seed et entry en 2022 ####

# Attention : ce qui suit semble induire la suppression de certaines entrées pour winner_entry. A vérifier.
lst_tibs[[55]] %>%
  rowwise() %>%
  mutate(winner_entry = case_when(is.na(winner_seed) ~ NA_character_,
                                  !grepl(pattern = "[[:digit:]]+", x = winner_seed) ~ winner_seed,
                                  TRUE ~ winner_entry),
         winner_seed = case_when(is.na(winner_seed) ~ NA_real_,
                                 !grepl(pattern = "[[:digit:]]+", x = winner_seed) ~ NA_real_,
                                 TRUE ~ as.numeric(winner_seed)),
         loser_entry = case_when(is.na(loser_seed) ~ NA_character_,
                                  !grepl(pattern = "[[:digit:]]+", x = loser_seed) ~ loser_seed,
                                  TRUE ~ loser_entry),
         loser_seed = case_when(is.na(loser_seed) ~ NA_real_,
                                 !grepl(pattern = "[[:digit:]]+", x = loser_seed) ~ NA_real_,
                                 TRUE ~ as.numeric(loser_seed))) %>%
  ungroup() %>%
  identity() -> lst_tibs[[55]]


#### Uniformisation des types des colonnes ####

lst_libs <- map(.x = lst_tibs,
                .f = function(tib) mutate(tib, across(.cols = where(~ is.logical(.x)),
                                                      .fns = ~ as.double(.x))))

#### Fusion des fichiers en une seule base ####

atp <- reduce(.x = lst_tibs,
              .f = bind_rows)

# Ajout de l'année ----

library("magrittr")
atp %<>% 
  mutate(year = stringr::str_sub(tourney_id, start=1L, end=4L)) %>%
  # select(year) %>%
  # pull() %>%
  # unique() %>%
  identity()

# Ajout d'un flag pour la catégorie de tournoi
atp %<>%
  mutate(tourney_type = case_when(grepl(pattern = "^Davis", x = tourney_name) ~ "Davis",
                                  grepl(x = tourney_name, pattern = "Olympics") ~ "Olympics",
                                  tourney_name %in% c("Masters Cup", "Tour Finals") ~ "Masters",
                                  tourney_name == "Laver Cup" ~ "Laver",
                                  TRUE ~ "ATP"))
# Transformation de la variable round en facteur ordonnée
atp %<>% mutate(round = factor(round, levels = c("RR", "R128", "R64", "R32", "R16", "QF", "SF", "BR", "F"),
                               ordered = TRUE))

# Extraction des informations des tournois ----

atp %>% 
  mutate(year = stringr::str_sub(tourney_id, start=1L, end=4L)) %>%
  select(year, tourney_id, tourney_name, tourney_level, tourney_date, tourney_type, surface, draw_size) %>%
  distinct() -> atp_tourneys
atp_aliz<-atp %>% 
  group_by(Année = substr(tourney_date,1,4))
# Removing all temporary variables ----

rm("lst_libs", "lst_tibs", "tmp", "types")
##### Preparation des données pour la carte #####
atp_global<-atp
group_tournoi<- atp_global %>% 
  select(everything())%>% 
  group_by(tourney_name) %>% dplyr::summarize(RENCONTRES = n()) #n() pour nombre d'observations

#On verifie que le nombre de rencontres vaut la length de atp
#length(row.names(atp))==sum(group_tournoi$RENCONTRES)

# Nous excluons les matches de la Davis et masters Cup, tour finals et WCT Challenge Cup
group_tournoi<-group_tournoi %>% filter(!grepl('Davis Cup', tourney_name))
group_tournoi<-group_tournoi %>% filter(!grepl('Masters Cup', tourney_name))
group_tournoi<-group_tournoi %>% filter(!grepl('Tour Finals', tourney_name))
group_tournoi<-group_tournoi %>% filter(!grepl('WCT Challenge Cup', tourney_name))
group_tournoi<-group_tournoi %>% filter(!grepl('Masters', tourney_name))

library(DescTools)
# Changement du nom de tournoi par la ville ou il a eu lieu
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="US Open","New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Roland Garros","Paris",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Australian Open","Melbourne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Miami Masters","Miami",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Cincinnati Masters","Cincinnati",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Rome Masters","Rome",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Canada Masters","Montreal",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Gstaad","Saanen",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Bastad",tourney_name, value = T),"Boo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Beckenham","London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Bermuda","Hamilton",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Paris Masters","Paris",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Sydney Outdoor","Sydney",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Hamburg Masters","Hamburg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Tokyo Outdoor","Tokyo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Madrid Masters","Madrid",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Stuttgart Outdoor","Stuttgart",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Hilversum","Amsterdam",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Montreal / Toronto","Toronto",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Sydney ",tourney_name,value=TRUE),"Sydney",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Tehran ",tourney_name,value=TRUE),"Tehran",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Tokyo ",tourney_name,value=TRUE),"Tokyo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Paris Indoor","Paris",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="s Hertogenbosch","Amsterdam",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Shanghai Masters","Shanghai",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Philadelphia WCT","Philadelphia",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Costa Do Sauipe","Sao Paulo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Aptos","Sacramento",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Colombus","Columbus",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Curacao","Willemstad",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Djkarta","Jakarta",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Boca West","Tallahassee",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Buzios","Armacao dos Buzios",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Bahamas","Spanish Wells",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Bahia","Salvador",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Mumbai","Bombay",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="New Delhi","Delhi",tourney_name))


group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Stuttgart",tourney_name,value=TRUE),"Stuttgart",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Richmond ",tourney_name,value=TRUE),"Richmond",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Forest Hills",tourney_name,value=TRUE),"New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Haverford","Harrisburg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Hong Kong","Shenzhen",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Itaparica","Barra Bonita",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Key Biscayne","Tallahassee",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Kiawah Island","Columbia",tourney_name))
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="La Jolla","Sacramento",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Lakeway","Austin",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Luxembourg","Steinfort",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Mar Del Plata","Azul",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Marrakech","Marrakesh",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Maui","Honolulu",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Merion","Harrisburg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Ocean City","Baltimore",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Oporto","Porto",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Paramus","New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Parioli","Rome",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Pinehurst","Raleigh",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Sardinia","Rome",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% c("St Petersburg","St. Petersburg"),"Saint Petersburg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Stowe","Concord",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Surbiton","London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Tanglewood","Raleigh",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Tel Aviv","Tel Aviv-Yafo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Tuscon","Phoenix",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name== "Wimbledon","London",tourney_name))


group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Hoylake","Hoylake-West Kirby",tourney_name))


group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Stockholm Masters","Stockholm",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Melbourne Indoor","Melbourne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Dallas WCT","Dallas",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^St. Louis",tourney_name, value = T),"Kansas City",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Denver WCT","Denver",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Houston",tourney_name,value = T),"Houston",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Miami WCT","Miami",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Barcelona",tourney_name,value = TRUE),"Barcelona",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Beijing",tourney_name,value = TRUE),"Beijing",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Munich WCT","Munich",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Monte Carlo  WCT","Monte Carlo ",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Washington WCT","Washington",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="U.S. National Chps.","New York",tourney_name))

#grep pour regex
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Johannesburg",tourney_name,value=TRUE),"Johannesburg",tourney_name))
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Fort Worth",tourney_name,value=TRUE),"Fort Worth",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Tulsa ",tourney_name,value=TRUE),"Tulsa",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Atp Cup","Brisbane",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Stockholm ",tourney_name, value=TRUE),"Stockholm",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Bologna WCT","Bologna",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Us Open","New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Boston",tourney_name, value=TRUE),"Boston",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Strasbourg WCT","Strasbourg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Laver Cup","Quincy",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="WCT World Cup","New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Stratton Mountain","Strasbourg",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Washington ",tourney_name,value=TRUE),"Washington",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Washington-",tourney_name,value=TRUE),"Washington",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Chicago","Chicago",tourney_name))
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Chicago-2","Chicago",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="WCT Invitational","New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^London",tourney_name, value=TRUE),"London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Toronto ",tourney_name, value=TRUE),"Toronto",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Toronto WCT","Toronto",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Charlotte WCT","Charlotte",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Vancouver WCT","Vancouver",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Baltimore WCT","Baltimore",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name=="Queen's Club","London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Indian Wells",tourney_name, value = T),"Sacramento", tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Bost","Boston",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Aix","Aix-en-Provence",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Alamo ","Alamo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Atlanta ","Atlanta",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "ATP Rio de Janeiro","Rio de Janeiro",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Austral","Melbourne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Austral", tourney_name,value = TRUE),"Melbourne", tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Bakersfield",tourney_name, value = TRUE),"Lompoc",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Barcelona","Barcelona",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Los Altos Hills WCT","Palo Alto",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Batad","Stockholm",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Binghamt","Binghamton",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Birmingham",tourney_name,value = TRUE),"Birmingham",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Bogota",tourney_name,value = TRUE),"Bogota",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Bretton ","Nashua",tourney_name)) #NH USA

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "North Conway","Nashua",tourney_name)) #NH USA

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Brussels",tourney_name,value = TRUE),"Brussels",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Buenos Aires",tourney_name, value = TRUE),"Bahia Blanca",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cannes",tourney_name, value = TRUE),"Cannes",tourney_name)) 


group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Buffalo WCT","Buffalo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cannes",tourney_name,value = TRUE), "Cannes",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Caracas",tourney_name,value = TRUE), "Caracas",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Casablanca",tourney_name,value = TRUE), "Casablanca",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cap D'Adge",tourney_name,value = TRUE), "Agde",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cape Town",tourney_name,value = TRUE),"Cape Town",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Caracas ","Caracas",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Casablanca ","Casablanca",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Champions ","Philadelphia",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Chicago",tourney_name,value = TRUE),"Chicago",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cleveland",tourney_name, value = TRUE),"Cleveland",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Cologne ",tourney_name, value = T),"Cologne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Copenhagen",tourney_name, value = TRUE),"Copenhagen",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Coral Springs","Tallahassee",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Corpus Christi NTL","Austin",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Dayton",tourney_name,value = T),"Dayton",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Delray Beach WCT","Delray Beach",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Detroit WCT","Detroit",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Dortmund WCT","Dortmund",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Durban",tourney_name,value = T),"Durban",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Dortmund WCT","Dortmund",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "East London WCT","East London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Essen",tourney_name,value = T),"Essen",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Evansville WCT","Evansville",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Fort Worth NTL","Fort Worth",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Fresno WCT","Fresno",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Genova",tourney_name,value = T),"Genoa",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Gothenberg WCT","Stockholm",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Grand Slam Cup","Munich",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Great Ocean Road Open","Melbourne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Hartford WCT","Hartford",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Hempstead",tourney_name,value = TRUE),"Hempstead",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Belgrade",tourney_name,value = TRUE),"Belgrade",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Binghamton",tourney_name,value = TRUE),"Binghamton",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Hilton Head",tourney_name,value = T),"Columbia",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Hollywood NTL","Hollywood",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Indianapolis",tourney_name,value = T),"Indianapolis",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Jackson",tourney_name,value = T),"Jackson",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Japanese Championships","Tokyo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Kimberley WCT","Cape Town",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Kuala Lumpur",tourney_name,value=TRUE),"Kuala Lumpur",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^La Costa",tourney_name,value = T),"Long Beach",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Long Island",tourney_name,value = T),"New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "La Paz NTL","Abapo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Lagos",tourney_name,value=TRUE),"Abuja",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Lacosta WCT","Long Beach",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("Las Vegas ",tourney_name,value=TRUE),"Las Vegas",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Lima NTL","Zorritos",tourney_name)) #Zorritos Peru

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Los Angeles",tourney_name,value=TRUE),"Los Angeles",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Louisville",tourney_name,value=TRUE),"Louisville",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name == "Masters Dec","New York",tourney_name)) #Masters Dec: aux USA en dec 1986 on ne retient que ce seul Master

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Minneapolis",tourney_name,value = T),"Minneapolis",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Memphis",tourney_name,value=TRUE),"Memphis",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Midland",tourney_name,value=TRUE),"Midland",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Mexico City",tourney_name,value=TRUE),"Mexico City",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Milan",tourney_name,value=TRUE),"Milan",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Monte Carlo",tourney_name, value=TRUE),"Monte Carlo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Monterrey",tourney_name, value=TRUE),"Mexico City",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Montreal WCT","Montreal",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Munich",tourney_name, value=TRUE),"Munich",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Athens",tourney_name, value=TRUE),"Athens",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Murray River Open","Melbourne",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Reggio Calabria","Rome",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% c("Poertschach","Portschach"),"Vienna",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Naples",tourney_name, value=TRUE),"Naples",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Naples",tourney_name, value=TRUE),"Naples",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^New Orleans",tourney_name, value=TRUE),"New Orleans",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Nations Cup","Munich",tourney_name)) #itftennis.com

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^New York ",tourney_name, value=TRUE),"New York",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Newport",tourney_name, value=TRUE),"Providence",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "NextGen Finals","Milan",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Nur-Sultan","Astana",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Oahu","Honolulu",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Orlando ",tourney_name, value=TRUE),"Orlando",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Atlanta ",tourney_name, value=TRUE),"Atlanta",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Palm Desert WCT","Palm Desert",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Paris ",tourney_name, value=TRUE),"Paris",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Pepsi Grand Slam","Boca Raton",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Port Elizabeth WCT","Cape Town",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Pretoria WCT","Pretoria",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Quebec ",tourney_name, value=TRUE),"Quebec",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Rancho Mirage","Palm Springs",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Republic Of China","Beijing",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Quebec ",tourney_name, value=TRUE),"Quebec",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Richmond-",tourney_name, value=TRUE),"Norfolk",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Adelaide",tourney_name,value = TRUE),"Adelaide",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Alamo",tourney_name,value = TRUE),"Sacramento",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Rio Olympics","Rio de Janeiro",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Rosmalen","Amsterdam",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Rome ",tourney_name, value=TRUE),"Rome",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Rotterdam ",tourney_name, value=TRUE),"Rotterdam",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Rye Brook","New York",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Salisbury ",tourney_name, value=TRUE),"Salisbury",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "San Antonio WCT","San Antonio",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^San Diego ",tourney_name, value=TRUE),"Long Beach",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "San Juan","Ponce",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Santiago","Chillan",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Sao Paulo ",tourney_name, value=TRUE),"Sao Paulo",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Seoul ",tourney_name, value=TRUE),"Seoul",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Shreveport ",tourney_name, value=TRUE),"Shreveport",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "South Orange","Trenton",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Southampton","New York",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^St. Petersburg ",tourney_name, value=TRUE),"Moscow",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^St Petersburg ",tourney_name, value=TRUE),"Moscow",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Vienna ",tourney_name, value=TRUE),"Vienna",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Zell Am See WCT","Vienna",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Zurich ",tourney_name, value = T),"Zurich",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "St. Poelten","Vienna",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "St. Vincent","Rome",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Stratton Mountain","Augusta-Richmond",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Dorado Beach","Ponce",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Mallorca","Palma",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Wembley ",tourney_name, value=TRUE),"London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("Wembley",tourney_name, value=TRUE),"London",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Amsterdam",tourney_name, value=TRUE),"Amsterdam",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %in% grep("^Columbus",tourney_name, value=TRUE),"Columbus",tourney_name))

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Winston-Salem","New York",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "World Invitational Classic","New York",tourney_name)) 

group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name %like% "Salisbury","Baltimore",tourney_name)) 
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name == "Aix en Provence","Aix-en-Provence",tourney_name)) 
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name == "Bretton Woods","Concord",tourney_name)) 
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name == "Tournament of Champions WCT","Austin",tourney_name)) 
group_tournoi<-group_tournoi %>%
  mutate(tourney_name=ifelse(tourney_name == "Champions Classic","Louisville",tourney_name)) 

#Il y a des doublons, on fait un group_by pour eviter les doublons dans tourney_name
group_tournoi <- group_tournoi %>% group_by(tourney_name)%>% 
  dplyr::summarise( RENCONTRES= sum(RENCONTRES),
                    .groups = 'drop')
#Utilisation du dataset world.cities pour faire la jointure ville-pays
library(maps)
data(world.cities)
villesMonde<-as.data.frame(world.cities)
#On renomme la seconde colonne  PAYS
colnames(villesMonde)[2]="PAYS"

#Quelques villes sont presentent dans 2 pays differents
#Dans ce cas de figure, on les supprime pour eviter que des matchs soient affectes au mauvais pays
#LA dans plusieurs pays alors qu'il s'agit de LA des USA
#la cle de jointure est tourney_name (ville de la rencontre)
#il est possible que les noms des villes soient redondants

#Les matches joues sur  Paris (France) seront aussi affectes a  Paris (Ontario) Canada lors de la jointure
#On supprime l'enregistrement correspondant C  Paris (Ontario) Canada
villesMonde <- villesMonde[!(villesMonde$name=="Paris" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Brussels" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Melbourne" & villesMonde$PAYS =="USA"),]
#villesMonde <- villesMonde[!(villesMonde$name=="Ontario" & villesMonde$PAYS =="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Doha" & villesMonde$PAYS =="Kuwait"),]
villesMonde <- villesMonde[!(villesMonde$name=="Manchester" & villesMonde$PAYS =="UK"),] #https://en.wikipedia.org/wiki/Manchester_Open
villesMonde <- villesMonde[!(villesMonde$name=="Albany" & villesMonde$PAYS =="Australia"),]
villesMonde <- villesMonde[!(villesMonde$name=="Bari" & villesMonde$PAYS =="India"),] #https://www.itftennis.com/en/tournament/bari/ita/1987/m-gp-ita-02a-1987/draws-and-results/
villesMonde <- villesMonde[!(villesMonde$name=="Birmingham" & villesMonde$PAYS =="UK"),] #https://www.itftennis.com/en/tournament/birmingham/gbr/1994/w-wt-gbr-01a-1994/draws-and-results/
villesMonde <- villesMonde[!(villesMonde$name=="Bombay" & villesMonde$PAYS =="New Zealand"),]
villesMonde <- villesMonde[!(villesMonde$name=="Brighton" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Bristol" & villesMonde$PAYS =="USA"),] #https://en.wikipedia.org/wiki/1989_Bristol_Open#:~:text=The%201989%20Bristol%20Open%20was,Jelen%20won%20the%20singles%20title.
villesMonde <- villesMonde[!(villesMonde$name=="Hampton" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Kingston" & villesMonde$PAYS !="Jamaica"),]
villesMonde <- villesMonde[!(villesMonde$name=="Macon" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Midland" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Murcia" & villesMonde$PAYS !="Spain"),]
villesMonde <- villesMonde[!(villesMonde$name=="Oviedo" & villesMonde$PAYS !="Spain"),]
villesMonde <- villesMonde[!(villesMonde$name=="Parma" & villesMonde$PAYS =="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="San Diego" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Springfield" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Vancouver" & villesMonde$PAYS =="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Wellington" & villesMonde$PAYS !="New Zealand"),]
villesMonde <- villesMonde[!(villesMonde$name=="Hamilton" & villesMonde$PAYS !="Bermuda"),]
villesMonde <- villesMonde[!(villesMonde$name=="Saint Petersburg" & villesMonde$PAYS =="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Sydney" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Houston" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Delhi" & villesMonde$PAYS =="Canada"),]
villesMonde <- villesMonde[!(villesMonde$name=="Sopot" & villesMonde$PAYS !="Poland"),]
villesMonde <- villesMonde[!(villesMonde$name=="Livingston" & villesMonde$PAYS =="Guatemala"),]
#On supprime les enregistrements des villes des pays d'amerique latine qui portent un nom d'une grande ville des USA
villesMonde <- villesMonde[!(villesMonde$name=="San Francisco" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="San Jose" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Richmond" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="San Antonio" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Newport" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="London" & villesMonde$PAYS !="UK"),]
villesMonde <- villesMonde[!(villesMonde$name=="Berlin" & villesMonde$PAYS !="Germany"),]
villesMonde <- villesMonde[!(villesMonde$name=="Cambridge" & villesMonde$PAYS !="UK"),]
villesMonde <- villesMonde[!(villesMonde$name=="Christchurch" & villesMonde$PAYS =="UK"),]#https://www.itftennis.com/en/tournament/christchurch/nzl/1973/m-gp-nzl-01a-1973/draws-and-results/
villesMonde <- villesMonde[!(villesMonde$name=="Cordoba" & villesMonde$PAYS !="Argentina"),]
villesMonde <- villesMonde[!(villesMonde$name=="Corpus Christi" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Dallas" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Dublin" & villesMonde$PAYS =="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Essen" & villesMonde$PAYS =="Belgium"),]
villesMonde <- villesMonde[!(villesMonde$name=="Fresno" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Valencia" & villesMonde$PAYS !="Spain"),]
villesMonde <- villesMonde[!(villesMonde$name=="Zaragoza" & villesMonde$PAYS !="Spain"),]
villesMonde <- villesMonde[!(villesMonde$name=="Perth" & villesMonde$PAYS !="Australia"),]
villesMonde <- villesMonde[!(villesMonde$name=="Halle" & villesMonde$PAYS !="Germany"),]
villesMonde <- villesMonde[!(villesMonde$name=="Las Vegas" & villesMonde$PAYS !="USA"),]
villesMonde <- villesMonde[!(villesMonde$name=="Milan" & villesMonde$PAYS =="Colombia"),]
villesMonde <- villesMonde[!(villesMonde$name=="Barcelona" & villesMonde$PAYS =="Venezuela"),]
villesMonde <- villesMonde[!(villesMonde$name=="Guadalajara" & villesMonde$PAYS =="Spain"),]
villesMonde <- villesMonde[!(villesMonde$name=="Los Angeles" & villesMonde$PAYS =="Chile"),]
villesMonde <- villesMonde[!(villesMonde$name=="Madrid" & villesMonde$PAYS =="Colombia"),]
villesMonde <- villesMonde[!(villesMonde$name=="Palermo" & villesMonde$PAYS =="Colombia"),]
villesMonde <- villesMonde[!(villesMonde$name=="Providence" & villesMonde$PAYS =="Mauritius"),]
villesMonde <- villesMonde[!(villesMonde$name=="Adelaide" & villesMonde$PAYS =="South Africa"),]
villesMonde <- villesMonde[!(villesMonde$name=="Boston" & villesMonde$PAYS =="UK"),]
villesMonde <- villesMonde[!(villesMonde$name=="Washington" & villesMonde$PAYS %in% c("Kiribati","UK")),]
#On supprime les villes redondantes dans un mm pays
#L"important c'est de connaitre le pays pour tout match du dataset 

villesMonde<-villesMonde %>% distinct(name,PAYS, .keep_all = TRUE)
attach(villesMonde)
# Fusion villes du monde pays avec lieu  du tournoi du tennis
#lieuTournoiS <-  villesMonde %>% dplyr::left_join(group_tournoi, by=c("name" ="tourney_name"), keep=TRUE)
fullJoin_tournoi_ville <- full_join(villesMonde,group_tournoi, by=c("name" ="tourney_name"), keep=T)
fullJoin_tournoi_villes<-fullJoin_tournoi_ville[complete.cases(fullJoin_tournoi_ville),]

# #Les grands chelems et date de creation pour les pays concernes et pas de grand chelem pour les autres

fullJoin_tournoi_villes<-fullJoin_tournoi_villes %>% mutate("Grand_Chelem" = case_when(PAYS == "France" ~ "France, Roland Garros depuis 1891.",
                                                                                       PAYS == "UK" ~ "UK,  Wimbledon depuis 1877.",
                                                                                       PAYS=="USA" ~ "USA,  US Open depuis 1881.",
                                                                                       PAYS== "Australia" ~ "Australia,  Australia Open depuis 1905.",
                                                                                       TRUE ~ "Pas de Grand Chelem."))

fullJoin_tournoi_villes<-fullJoin_tournoi_villes %>%
  mutate(PAYS=ifelse(PAYS=="USA","United States",PAYS))%>%
  mutate(PAYS=ifelse(PAYS=="UK","United Kingdom",PAYS))
#On rajoute des colonnes pour les tournois ATP (250, 500,...) qui se jouent dans chaque pays

location_atp250<-c("Antalya","Antwerp","Atlanta","Auckland",
                   "Boo","Brisbane","Budapest","Bahia Blanca",
                   "Chengdu","Cordoba","Delray Beach", "Doha",
                   "Eastbourne","Estoril","Geneva","Houston","Kitzbuhel",
                   "Los Cabos", "Lyon","Marrakesh","Marseille", "Metz", "Montpellier",
                   "Moscow","Munich","New York","Pune","Sao Paulo", "Sofia","Saint Petersburg",
                   "Stockholm","Stuttgart","Sydney","Umag","Zhuhai") 

location_atp500<-c("Acapulto","Barcelona","Basel","Beijing","Dubai","Halle",
                   "Hamburg","London","Rio de Janeiro","Rotterdam","Tokyo",
                   "Vienna","Washington") 

location_atp1000<-c("Cincinnati","Sacramento","Madrid", "Miami","Monte Carlo",
                    "Toronto","Montreal","Paris","Rome","Shanghai")
fullJoin_tournoi_villes<- fullJoin_tournoi_villes %>% mutate("ATP_250" = case_when(name %in% location_atp250 ~ "OUI",
                                                                                   TRUE ~ "Non")) %>%
  mutate("ATP_500"=case_when(name %in% location_atp500 ~ "OUI",
                             TRUE ~ "Non")) %>%
  mutate("ATP_1000"=case_when(name %in% location_atp1000 ~ "OUI",
                              TRUE ~ "Non"))

# Preparation du contenu des infos bulles sur la carte. !!! "<br/>" pour saut de lignes sous html

#Iformations sur les infos bulles par la suite
library(htmltools)
mytext <- paste(
  "Ville : ",  fullJoin_tournoi_villes$name,"<br/>", 
  "Grand Chelem : ",  fullJoin_tournoi_villes$Grand_Chelem, "<br/>", 
  "ATP 1000 : ",  fullJoin_tournoi_villes$ATP_1000, "<br/>",
  "ATP 500 : ",  fullJoin_tournoi_villes$ATP_500, "<br/>",
  "ATP 250 : ",  fullJoin_tournoi_villes$ATP_250, "<br/>",
  "Nombre de rencontres ATP : ",  fullJoin_tournoi_villes$RENCONTRES, 
  sep="") %>%
  lapply(htmltools::HTML)

donnees_carte<-fullJoin_tournoi_villes
##### CARTE DONE ####
library(rbokeh)
library(leaflet)
note<- " Toutes les rencontres ATP simples entre 1968-2022 sauf : Davis Cup, Masters Cup, Tour Finals, WCT challenge Cup et Masters."
carte<-donnees_carte %>%  leaflet() %>% 
  addTiles()  %>% 
  setView( lat=48.86, lng=2.34 , zoom=2) %>%
  addTiles() %>%
  addCircles(label = ~mytext) %>%
  addControl(note, position = "bottomleft")

