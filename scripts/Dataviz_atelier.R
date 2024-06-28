## Données du projet Oraccle - Université Sorbonne Paris Nord

# Librairies
library(tidyverse)
library(plotly)
# library(flexdashboard)
# library(icons)
# library(gt)
# library(rvest)
# library(summarytools)
# library(htmlwidgets)
# library(ggiraph)
# library(tools)


      ###### Data-visualisation issues de l'atelier



# Import données Oraccle
cohortes_age_premiereins <- read_delim("./data/Oraccle/cohorte_age_premiereins.csv", ",")
cohorte_anbac <- read_delim("./data/Oraccle/cohorte_anbac.csv", ",")
cohorte_bac <- read_delim("./data/Oraccle/cohorte_bac.csv", ",")
cohorte_derniereins <- read_delim("./data/Oraccle/cohorte_derniereins.csv", ",")
cohorte_premiereins <- read_delim("./data/Oraccle/cohorte_premiereins.csv", ",")
cohorte_sexe <- read_delim("./data/Oraccle/cohorte_sexe.csv", ",")
cohorte_spe <- read_delim("./data/Oraccle/cohorte_spe.csv", ",")
cohortes <- read_delim("./data/Oraccle/cohortes.csv", ",")
formations <- read_delim("./data/Oraccle/formations.csv", ",") #Année-Diplome-Etablissement

# Données transformées
formations_enrichies <- read_csv("data/Export/formations_enrichies.csv")
cohortes_unnest <- cohortes |> 
    select(-reussites) |> 
    mutate(parcours_annee = strsplit(as.character(trace), "\\+")) |> 
    unnest(parcours_annee) |> 
    mutate(parcours_annee = strsplit(as.character(parcours_annee), "&")) |> 
    unnest(parcours_annee) |> 
    mutate(formation = str_extract_all(parcours_annee, "(?<=-)([^-&]+)(?=-)"), #extract characters between - and -
           etablissement = str_extract_all(parcours_annee, "([^\\-]+)$")) #extract characters after last -
    #left_join(formations, join_by("parcours_annee" == "cohorte"))



    ## Dataviz n°2 : Parcours et pourcentage de réussites des spécialités SVT et Maths au bac


#--- Préparation des données

#  Liste des cohortes avec les spécialités Maths et SVT au bac
table <- cohorte_spe |> 
    mutate(couple_spe = paste(bac_spe1, "-", bac_spe2)) |> 
    filter(couple_spe == "MATHS - SVT")

# Filtre des cohortes appartenant à la liste
table <- cohortes_unnest |> 
    filter(cohorteid %in% table$cohorteid) |> 
    mutate(annee_formation = substr(parcours_annee, 1, 1),
           formation = as.character(formation)) |> 
    left_join(diplome |> select(DIPLOME_SISE, LIBELLE_INTITULE_1), by = c("formation" = "DIPLOME_SISE")) |> 
    mutate(nom_formation = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", tolower(LIBELLE_INTITULE_1), perl = TRUE)) |> 
    select(-LIBELLE_INTITULE_1)
table_annee1 <- table |> 
    summarise(nb_etudiants = sum(effectif), 
              .by = c(annee_formation, nom_formation))




    ## Dataviz n°4, 2/4 : Tableau de bord de l’orientation en SHS


# Préparation des données
