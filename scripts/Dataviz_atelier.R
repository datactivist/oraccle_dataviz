## Données du projet Oraccle - Université Sorbonne Paris Nord

# Librairies
library(tidyverse)
library(plotly)
library(treemap)
library(treemapify)
library(cowplot)

# Import données Oraccle : https://data.unif.app/data/avril2024/Ile-de-France/
cohortes_age_premiereins <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_age_premiereins.csv", ",")
cohorte_anbac <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_anbac.csv", ",")
cohorte_bac <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_bac.csv", ",")
cohorte_derniereins <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_derniereins.csv", ",")
cohorte_premiereins <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_premiereins.csv", ",")
cohorte_sexe <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_sexe.csv", ",")
cohorte_spe <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohorte_spe.csv", ",")
cohortes <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/cohortes.csv", ",")
formations <- read_delim("https://data.unif.app/data/avril2024/Ile-de-France/formations.csv", ",") #Année-Diplome-Etablissement

# Import données référentiels
academie <- read_csv("../data/Referentiels/n_academie_et_assimile_.csv", locale = locale(encoding = "ISO-8859-1"))
groupe_discipline <- read_csv("../data/Referentiels/n_groupe_discipline_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
secteur_discipline <- read_csv("../data/Referentiels/n_secteur_disciplinaire_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
discipline <- read_csv("../data/Referentiels/n_discipline_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
diplome <- read_csv("../data/Referentiels/n_diplome_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
type_diplome <- read_csv("../data/Referentiels/n_type_diplome_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
etablissement <- read_delim("../data/Referentiels/organismes-2024-04-22.csv", ";")

# Import référentiels ortho-typographiés
diplome_OT <- read_delim("https://data.pages.unif.app/html/ortho/ortho_diplomes.csv", "|")
discipline_OT <- read_delim("https://data.pages.unif.app/html/ortho/ortho_discipline.csv", "|")
secteur_discipline_OT <- read_delim("https://data.pages.unif.app/html/ortho/ortho_secteur_disciplinaire.csv", "|")
specialites_bac_OT <- read_delim("https://data.pages.unif.app/html/ortho/ortho_specialites.csv", "|")


# Custom_theme pour ggplot
custom_theme <- function (){
    font <- "Helvetica"
    ggplot2::theme(plot.title = ggplot2::element_text(family = font,size = 19, face = "bold", color = "#222222"), 
        plot.subtitle = ggplot2::element_text(family = font,size = 18, face = "italic", margin = ggplot2::margin(0, 0, 9, 0)), 
        plot.caption = ggplot2::element_text(family = font,size = 15, face = "italic", color = "#666666", margin = ggplot2::margin(9, 0, 9, 0)), 
        plot.tag = ggplot2::element_text(size = 15),
        #plot.tag.position = "topright",
        plot.caption.position = "plot",
        legend.title = ggplot2::element_text(family = font, size = 18, color = "#222222"), 
        legend.position = "top", 
        legend.text.align = 0, 
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(family = font, size = 18,color = "#222222"), 
        axis.text = ggplot2::element_text(family = font, size = 15,color = "#222222"), 
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,b = 10), size = 12), 
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(5,b = 20), size = 12), 
        axis.title = ggplot2::element_text(family = font, size = 18,color = "#222222"),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank(), 
        panel.background = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white"),
        strip.text = ggplot2::element_text(size = 22, hjust = 0, face = "bold"))
}



# Données transformées
formations_enrichies <- formations |> 
    left_join(academie |> select(ACADEMIE_ET_ASSIMILE, LIBELLE_70), by = c("acaeta" = "ACADEMIE_ET_ASSIMILE")) |> 
    left_join(etablissement |> 
                  distinct(`UAI validée`, `Raison sociale`, `Adresse`, `Nature`) |> 
                  group_by(`UAI validée`) |> 
                  arrange(Nature) |> 
                  filter(row_number() == 1, !is.na(`UAI validée`)), 
              by = c("compos" = "UAI validée")) |> 
    mutate(cursus_lmd = case_when(cursus_lmd == "L" ~ "Licence",
                                  cursus_lmd == "M" ~ "Master",
                                  cursus_lmd == "D" ~ "Doctorat", 
                                  .default = NA_character_)) |> 
    left_join(diplome |> select(DIPLOME_SISE, LIBELLE_INTITULE_1, TYPE_DIPLOME_SISE, SECTEUR_DISCIPLINAIRE_SISE), by = c("diplom" = "DIPLOME_SISE")) |> 
    left_join(discipline |> select(DISCIPLINE_SISE, LIBELLE_COURT, LIBELLE_DISCIPLINE_60), by = c("discipli" = "DISCIPLINE_SISE")) |> 
    left_join(groupe_discipline |> select(GROUPE_DISCIPLINE_SISE, LIBELLE_LONG), by = c("groupe" = "GROUPE_DISCIPLINE_SISE")) |> 
    left_join(secteur_discipline |> select(SECTEUR_DISCIPLINAIRE_SISE, LIBELLE_SECTEUR_DISCIPLINAIRE), by = c("sectdis" = "SECTEUR_DISCIPLINAIRE_SISE")) |> 
    left_join(type_diplome |> select(TYPE_DIPLOME_SISE, LIBELLE_LONG), by = c("typ_dipl" = "TYPE_DIPLOME_SISE")) |> 
    rename(cohorte = formation,
           academie = LIBELLE_70,
           id_etablissement = etabli, #pas d'erreur sur cette colonne
           nom_etablissement = `Raison sociale`,
           adresse_etablissement = Adresse,
           degre_etude = degetu, #pas d'erreur sur cette colonne (extraction du premier caractère et correspond bien à la colonne `degetu`)
           id_diplome = diplom, #pas d'erreur sur cette colonne
           nom_diplome = LIBELLE_INTITULE_1,
           id_discipline = discipli,
           nom_discipline = LIBELLE_COURT,
           nom_long_discipline = LIBELLE_DISCIPLINE_60,
           id_groupe_discipline = groupe,
           nom_groupe_discipline = LIBELLE_LONG.x,
           id_secteur_discipline = SECTEUR_DISCIPLINAIRE_SISE,
           nom_secteur_discipline = LIBELLE_SECTEUR_DISCIPLINAIRE,
           id_type_diplome = TYPE_DIPLOME_SISE,
           nom_type_diplome = LIBELLE_LONG.y) |> 
    select(cohorte, degre_etude, cycle, cursus_lmd, id_diplome, nom_diplome, id_type_diplome, nom_type_diplome, id_discipline, nom_discipline, 
           nom_long_discipline, id_groupe_discipline, nom_groupe_discipline, id_secteur_discipline, nom_secteur_discipline, #id_etablissement, 
           nom_etablissement, adresse_etablissement, academie) |> 
    distinct()

# Nettoyage
#formations_enrichies <- read_csv("data/Export/formations_enrichies.csv")
cohortes_unnest <- cohortes |> 
    select(-reussites) |> 
    mutate(parcours_annee = strsplit(as.character(trace), "\\+")) |> 
    unnest(parcours_annee) |> 
    mutate(parcours_annee = strsplit(as.character(parcours_annee), "&")) |> 
    unnest(parcours_annee) |> 
    mutate(formation = str_extract_all(parcours_annee, "(?<=-)([^-&]+)(?=-)"), #extract characters between - and -
           etablissement = str_extract_all(parcours_annee, "([^\\-]+)$"))   #extract characters after last -
cohortes_unnest_enrichies <- cohortes_unnest |> 
    left_join(formations_enrichies, by = c("parcours_annee" = "cohorte"))







      ###### Data-visualisation issues de l'atelier

# Custom_theme pour ggplot
custom_theme <- function (){
    font <- "Helvetica"
    ggplot2::theme(plot.title = ggplot2::element_text(family = font,size = 19, face = "bold", color = "#222222"), 
        plot.subtitle = ggplot2::element_text(family = font,size = 18, face = "italic", margin = ggplot2::margin(0, 0, 9, 0)), 
        plot.caption = ggplot2::element_text(family = font,size = 15, face = "italic", color = "#666666", margin = ggplot2::margin(9, 0, 9, 0)), 
        plot.tag = ggplot2::element_text(size = 15),
        #plot.tag.position = "topright",
        plot.caption.position = "plot",
        legend.title = ggplot2::element_text(family = font, size = 18, color = "#222222"), 
        legend.position = "top", 
        legend.text.align = 0, 
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(family = font, size = 18,color = "#222222"), 
        axis.text = ggplot2::element_text(family = font, size = 15,color = "#222222"), 
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,b = 10), size = 12), 
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(5,b = 20), size = 12), 
        axis.title = ggplot2::element_text(family = font, size = 18,color = "#222222"),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank(), 
        panel.background = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white"),
        strip.text = ggplot2::element_text(size = 22, hjust = 0, face = "bold"))
}



# Données transformées
formations_enrichies <- formations |> 
    left_join(academie |> select(ACADEMIE_ET_ASSIMILE, LIBELLE_70), by = c("acaeta" = "ACADEMIE_ET_ASSIMILE")) |> 
    left_join(etablissement |> 
                  distinct(`UAI validée`, `Raison sociale`, `Adresse`, `Nature`) |> 
                  group_by(`UAI validée`) |> 
                  arrange(Nature) |> 
                  filter(row_number() == 1, !is.na(`UAI validée`)), 
              by = c("compos" = "UAI validée")) |> 
    mutate(cursus_lmd = case_when(cursus_lmd == "L" ~ "Licence",
                                  cursus_lmd == "M" ~ "Master",
                                  cursus_lmd == "D" ~ "Doctorat", 
                                  .default = NA_character_)) |> 
    left_join(diplome |> select(DIPLOME_SISE, LIBELLE_INTITULE_1, TYPE_DIPLOME_SISE, SECTEUR_DISCIPLINAIRE_SISE), by = c("diplom" = "DIPLOME_SISE")) |> 
    left_join(discipline |> select(DISCIPLINE_SISE, LIBELLE_COURT, LIBELLE_DISCIPLINE_60), by = c("discipli" = "DISCIPLINE_SISE")) |> 
    left_join(groupe_discipline |> select(GROUPE_DISCIPLINE_SISE, LIBELLE_LONG), by = c("groupe" = "GROUPE_DISCIPLINE_SISE")) |> 
    left_join(secteur_discipline |> select(SECTEUR_DISCIPLINAIRE_SISE, LIBELLE_SECTEUR_DISCIPLINAIRE), by = c("sectdis" = "SECTEUR_DISCIPLINAIRE_SISE")) |> 
    left_join(type_diplome |> select(TYPE_DIPLOME_SISE, LIBELLE_LONG), by = c("typ_dipl" = "TYPE_DIPLOME_SISE")) |> 
    rename(cohorte = formation,
           academie = LIBELLE_70,
           id_etablissement = etabli, #pas d'erreur sur cette colonne
           nom_etablissement = `Raison sociale`,
           adresse_etablissement = Adresse,
           degre_etude = degetu, #pas d'erreur sur cette colonne (extraction du premier caractère et correspond bien à la colonne `degetu`)
           id_diplome = diplom, #pas d'erreur sur cette colonne
           nom_diplome = LIBELLE_INTITULE_1,
           id_discipline = discipli,
           nom_discipline = LIBELLE_COURT,
           nom_long_discipline = LIBELLE_DISCIPLINE_60,
           id_groupe_discipline = groupe,
           nom_groupe_discipline = LIBELLE_LONG.x,
           id_secteur_discipline = SECTEUR_DISCIPLINAIRE_SISE,
           nom_secteur_discipline = LIBELLE_SECTEUR_DISCIPLINAIRE,
           id_type_diplome = TYPE_DIPLOME_SISE,
           nom_type_diplome = LIBELLE_LONG.y) |> 
    select(cohorte, degre_etude, cycle, cursus_lmd, id_diplome, nom_diplome, id_type_diplome, nom_type_diplome, id_discipline, nom_discipline, 
           nom_long_discipline, id_groupe_discipline, nom_groupe_discipline, id_secteur_discipline, nom_secteur_discipline, #id_etablissement, 
           nom_etablissement, adresse_etablissement, academie) |> 
    distinct()

# Nettoyage
#formations_enrichies <- read_csv("data/Export/formations_enrichies.csv")
cohortes_unnest <- cohortes |> 
    select(-reussites) |> 
    mutate(parcours_annee = strsplit(as.character(trace), "\\+")) |> 
    unnest(parcours_annee) |> 
    mutate(parcours_annee = strsplit(as.character(parcours_annee), "&")) |> 
    unnest(parcours_annee) |> 
    mutate(formation = str_extract_all(parcours_annee, "(?<=-)([^-&]+)(?=-)"), #extract characters between - and -
           etablissement = str_extract_all(parcours_annee, "([^\\-]+)$"))   #extract characters after last -
cohortes_unnest_enrichies <- cohortes_unnest |> 
    left_join(formations_enrichies, by = c("parcours_annee" = "cohorte"))



    ## Dataviz n°2 : Parcours et pourcentage de réussites des spécialités SVT et Maths au bac


#--- Préparation des données

#  Liste des cohortes avec les spécialités Maths et SVT au bac
specialites <- cohorte_spe |> 
    mutate(couple_spe = paste(bac_spe1, "-", bac_spe2)) |> 
    filter(couple_spe == "MATHS - SVT")

# Filtre des cohortes appartenant à la liste
filtre_spe_cohortes <- cohortes |> 
    select(-reussites) |>
    filter(cohorteid %in% specialites$cohorteid) |> 
    mutate(trace_save = trace) |> 
    separate(trace, into = c("V1", "V2", "V3"), sep = "\\+") |> 
    mutate(across(starts_with("V"), ~str_extract(., "^[^&]+")),
           V0 = "MATHS - SVT") |> 
    #extraction des intitulés de formation
    mutate(formation_V1 = as.character(str_extract_all(V1, "(?<=-)([^-&]+)(?=-)")), #extract characters between - and -
           formation_V2 = as.character(str_extract_all(V2, "(?<=-)([^-&]+)(?=-)"))) |> 
    #jointure avec les données de formation
    left_join(diplome_OT |> 
                mutate(nom_formation_V1 = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", tolower(LIBELLE_INTITULE_1), perl = TRUE)) |> 
                select(c(DIPLOME_SISE, nom_formation_V1)), 
              by = c("formation_V1" = "DIPLOME_SISE")) |> 
    left_join(diplome_OT |> 
                mutate(nom_formation_V2 = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", tolower(LIBELLE_INTITULE_1), perl = TRUE)) |> 
                select(c(DIPLOME_SISE, nom_formation_V2)), 
              by = c("formation_V2" = "DIPLOME_SISE")) |> 
    mutate(spe_to_V1 = sum(effectif, na.rm = TRUE), .by = c(V0, nom_formation_V1)) |> 
    mutate(V1_to_V2 = sum(effectif, na.rm = TRUE), .by = c(nom_formation_V1, nom_formation_V2))
    
# Données pour le graph
  # Aretes
aretes <- rbind(filtre_spe_cohortes |> 
                 mutate(from = V0, to = nom_formation_V1, `Nombre d'étudiants` = spe_to_V1) |> 
                 distinct(from, to, `Nombre d'étudiants`),
               filtre_spe_cohortes |> 
                 mutate(from = nom_formation_V1, to = nom_formation_V2, `Nombre d'étudiants` = V1_to_V2) |> 
                 distinct(from, to, `Nombre d'étudiants`))
    ## Filtre sur certaines aretes pour plus de lisibilité
top10_post_spe <- aretes |> 
  filter(from == "MATHS - SVT") |> 
  arrange(desc(`Nombre d'étudiants`), .by = from) |> 
  filter(row_number() <= 10)
suite_top10 <- aretes |> 
  filter(from %in% top10_post_spe$to) |> 
  na.omit() |> 
  filter(`Nombre d'étudiants` > 100)
aretes_filtre <- rbind(top10_post_spe, suite_top10) |> na.omit() #|> 
  #mutate(weight_normalized = (`Nombre d'étudiants` - min(`Nombre d'étudiants`)) / (max(`Nombre d'étudiants`) - min(`Nombre d'étudiants`))) |> 
  #mutate(alpha_var = weight_normalized * (1 - .8) + .8)

  # Noeuds
noeuds <- rbind(aretes_filtre |> select(from) |> rename(name = from) |> mutate(group = "Inscription 1ère année"),
                aretes_filtre |> select(to) |> rename(name = to) |> mutate(group = "Inscription 2è année")) |> 
  mutate(group = ifelse(name == "MATHS - SVT", "Spécialités au BAC", group)) |> 
  distinct() |> slice(1, .by = name) 

# Création de l'objet graphe
graph <- graph_from_data_frame(d = aretes_filtre, vertices=noeuds, directed = TRUE)
#network <- graph_from_data_frame(d=, 
 #                                vertices=noeuds, directed=T) 

# Fonction pour couper les labels trop longs
wrap_text <- function(text, width) {
  sapply(text, function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  })
}

# Création du graphique avec ggraph
reseau <- ggraph(graph, layout = 'stress') + 
  geom_edge_link(aes(width = `Nombre d'étudiants`), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(6, 'mm'), color = "grey", alpha = 0.8) + 
  geom_edge_loop(aes(width = `Nombre d'étudiants`), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(4, 'mm'), color = "grey", alpha = 0.8) +   
  geom_node_point(aes(color = factor(group, levels = c("Spécialités au BAC", "Inscription 1ère année", "Inscription 2è année"))), size = 10) + 
  geom_node_text(aes(label = wrap_text(name, width = 25)), vjust = -1.5, hjust = 0, size = 5) + 
  scale_edge_width(range = c(0.5, 5)) + 
  scale_color_manual(values = c("Spécialités au BAC" = "#345E68", "Inscription 1ère année" = "#FEDEA0", "Inscription 2è année" = "#B7C2A5")) +
  labs(title = "Diplômes universitaires des bacheliers aux spécialités SVT et Maths en Ile-de-France",
       subtitle = "Seuls les 10 diplômes regroupant le plus d'étudiants en première année sont affichés\n") +
  theme_void() +
  theme(legend.position = "top",
        legend.title = ggplot2::element_text(size = 18, color = "#222222"), 
        legend.text = ggplot2::element_text(size = 18,color = "#222222"), 
        legend.box = "vertical", legend.box.just = "left",
       # legend.margin = margin(t = -100, r = 0, b = 100, l = 0),
        plot.title = ggplot2::element_text(size = 21, face = "bold", color = "#222222"), 
        plot.subtitle = ggplot2::element_text(size = 18, face = "italic", margin = ggplot2::margin(0, 0, 9, 0))) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title = "Discipline", override.aes = list(lwd = 2)),
         width = guide_legend(override.aes = list(lwd = 20)))
ggsave(file = "figures/dataviz_atelier/reseau_diplomes_svt_maths.png", plot = reseau, width = 13, height = 9)




    ## Dataviz n°4, 2/4 : Tableau de bord de l’orientation en SHS


    # 1) Histogramme des années après le BAC

# Préparation des données
table <- cohortes_unnest |> 
  mutate(annee = str_sub(parcours_annee, 1, 1)) |> 
  filter(annee != ".") |> 
  filter(annee == max(as.numeric(annee)), .by = cohorteid) |> 
  mutate(effectif_reel = ifelse(effectif == 0, 1.14, effectif * 1.14)) |> 
  summarise(nb_etudiants = sum(effectif_reel, na.rm = TRUE), .by = annee) |> 
  mutate(percent = round(nb_etudiants / sum(nb_etudiants) * 100, 0))

# Visualisation
histo <- ggplot(table) +
  aes(x = annee, y = nb_etudiants) +
  geom_col(fill = "#345E68") +
  labs(
    x = "Nombre d'années après le BAC",
    y = "Nombre d'étudiants",
    title = "Répartition des étudiants d'Ile-de-France selon le nombre d'années d'étude après le BAC"
  ) +
  geom_label(aes(y = nb_etudiants + 0.06*max(nb_etudiants), label = paste0(percent, "%")), size = 5, fill = "white", label.size = NA) +
  scale_y_continuous(labels = scales::comma) +
  custom_theme() +
  theme(plot.title.position = "plot")



    # 2) Nombre de réorientations

# Préparation des données
table <- cohortes_unnest_enrichies |> 
  distinct(cohorteid, effectif, nom_diplome) |> 
  summarise(nb_filieres = n(), effectif_reel = ifelse(effectif == 0, 1.14, effectif * 1.14), 
            .by = cohorteid) |> 
  distinct()
resume <- table |> 
  summarise(nb_etudiants = sum(effectif_reel), .by = nb_filieres) |> 
  mutate(categorie = case_when(nb_filieres == 1 ~ "Filière unique",
                               nb_filieres == 2 ~ "Deux filières",
                               .default = "Trois filières et plus")) |> 
  summarise(Freq = sum(nb_etudiants, na.rm = TRUE), .by = categorie)


# Visualisation
donut <- resume |> 
    mutate(fraction = Freq / sum(Freq),
           proportion = round((Freq / sum(Freq))*100),
           ymax = cumsum(fraction),
           ymin = c(0, head(ymax, n = -1)), 
           labelPosition = (ymax + ymin) / 2,
           categorie = factor(categorie, levels = c("Filière unique", "Deux filières", "Trois filières et plus"))) |> 
    ggplot(aes(ymax = ymax, ymin = ymin, xmax = 3.3, xmin = 2, fill = categorie)) +
      geom_rect(col = "white", linewidth = 2) +
      geom_text(x = 4, aes(y = labelPosition, label = paste(proportion,"%",sep = "")), color = "#333333", size = 7) +
      geom_text(aes(x = 0, y = 0, label = format(as.integer(round(sum(Freq), 0)), nsmall = 1, big.mark = ".")), 
                    col = "#333333", alpha = 0.8, size = 7, fontface = "bold", inherit.aes = FALSE) +
      scale_fill_manual(values = c("Filière unique" = "#345E68", 
                                   "Deux filières" = "#FEDEA0", 
                                   "Trois filières et plus" = "#B7C2A5")) +
      coord_polar(theta = "y") +
      labs(title = "Répartition des étudiants d'Ile-de-France selon les\nréorientations au cours des études supérieures",
           subtitle = "Nombre de filières de diplôme par parcours étudiant\n") +
      theme_void() +
      theme(legend.position = "top",
            legend.title = ggplot2::element_text(size = 18, color = "#222222"), 
            legend.text = ggplot2::element_text(size = 18,color = "#222222"), 
            plot.title = ggplot2::element_text(size = 19, face = "bold", color = "#222222"), 
            plot.subtitle = ggplot2::element_text(size = 18, face = "italic", margin = ggplot2::margin(0, 0, 9, 0))) +
      guides(fill = guide_legend(title = "                                     ", reverse = F))
donut


    # 3) Spécialités au BAC

# Préparation des données
table <- cohorte_spe |> 
    mutate(couple_spe = paste(bac_spe1, "-", bac_spe2),
           effectif_reel = ifelse(effectif == 0, 1.14, effectif * 1.14)) |> 
    summarise(effectif_couple_spe = sum(effectif_reel, na.rm = TRUE), .by = couple_spe) |> 
    arrange(desc(effectif_couple_spe)) |> 
    filter(couple_spe != "NA - NA") |> 
    mutate(percent = round((effectif_couple_spe / sum(effectif_couple_spe))*100))

# Visualisation
treemap <- table |> 
  filter(row_number() <= 10) |> 
  ggplot() +
      geom_treemap(aes(area = effectif_couple_spe, fill = couple_spe), col = "white", size = 4) +
      geom_treemap_text(aes(area = effectif_couple_spe, fill = couple_spe, 
                            label = paste0(couple_spe, "\n(", percent, "%)")),
                        colour = "white", place = "centre", size = 15, grow = TRUE) +
      scale_fill_manual(values = c("#345E68", "#FEDEA0", "#B7C2A5", "#023743","#7A9BB1", "#B8AA75", "#7B8598", "#345B48", "#476F84", "#D0BA7C")) +
      labs(title = "Les 10 principales spécialités au BAC choisies par les étudiants d'Ile-de-France") +
      custom_theme() +
      theme(legend.position = "none")



    # 4) Organisations des 3 graphiques ensemble

a <- plot_grid(histo, treemap, nrow = 1, align = 'vh')
ggsave(file = "figures/dataviz_atelier/annees-etudes_specialites-bac.png", plot = a, width = 23, height = 7)
ggsave(file = "figures/dataviz_atelier/repartition-reorientations.png", plot = donut, width = 9, height = 6)




# Brouillon

# Noeuds du graph en réseau, selon le groupe disciplinaire du diplôme
noeuds <- rbind(filtre_spe_cohortes |> 
                  rename(name = V0) |> 
                  mutate(group = "Spécialités BAC") |> 
                  distinct(name, group),
                filtre_spe_cohortes |> 
                  rename(name = nom_formation_V1) |>
                  mutate(group = "V1") |> 
                  distinct(name, group),
                filtre_spe_cohortes |> 
                  rename(name = nom_formation_V2) |> 
                  mutate(group = "V2") |> 
                  distinct(name, group)) |> 
  arrange(name, .by = group) |> 
  slice(1, .by = name)
  # Noeuds
noeuds_filtre_inter <- filtre_spe_cohortes |> 
  left_join(formations_enrichies |> distinct(id_diplome, nom_groupe_discipline) |> slice(1, .by = id_diplome),
            by = c("formation_V1" = "id_diplome")) |> 
  left_join(formations_enrichies |> distinct(id_diplome, nom_groupe_discipline) |> slice(1, .by = id_diplome),
            by = c("formation_V2" = "id_diplome")) |>
  rename(discipline_V1 = nom_groupe_discipline.x,
         discipline_V2 = nom_groupe_discipline.y) |> 
  filter(nom_formation_V1 %in% c(aretes_filtre$from, aretes_filtre$to) | nom_formation_V2 %in% c(aretes_filtre$from, aretes_filtre$to))  
noeuds_filtre <- rbind(noeuds_filtre_inter |> 
                         distinct(nom_formation_V1, discipline_V1) |> 
                         rename(name = nom_formation_V1, group = discipline_V1),
                       noeuds_filtre_inter |> 
                         distinct(nom_formation_V2, discipline_V2) |> 
                         rename(name = nom_formation_V2, group = discipline_V2)) |> 
  distinct() |> na.omit() |> 
  mutate(group = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", tolower(group), perl = TRUE)) |> 
  add_row(name = "MATHS - SVT", group = "Spécialité au BAC")  
