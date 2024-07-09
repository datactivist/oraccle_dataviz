## Données du projet Oraccle - Université Sorbonne Paris Nord

# Librairies
library(tidyverse)
library(plotly)
library(flexdashboard)
library(icons)
library(gt)
library(rvest)
library(summarytools)
library(htmlwidgets)
library(ggiraph)
library(tools)



      ###### Data-visualisation quick-wins



# Import données Oraccle
cohortes_age_premiereins <- read_delim("./data/Oraccle/cohorte_age_premiereins.csv", ",")
cohorte_anbac <- read_delim("./data/Oraccle/cohorte_anbac.csv", ",")
cohorte_bac <- read_delim("./data/Oraccle/cohorte_bac.csv", ",")
cohorte_derniereins <- read_delim("./data/Oraccle/cohorte_derniereins.csv", ",")
cohorte_premiereins <- read_delim("./data/Oraccle/cohorte_premiereins.csv", ",")
cohorte_sexe <- read_delim("./data/Oraccle/cohorte_sexe.csv", ",")
cohorte_spe <- read_delim("./data/Oraccle/cohorte_spe.csv", ",")
cohortes <- read_delim("./data/Oraccle/cohortes.csv", ",")
formations <- read_delim("./data/Oraccle/formations.csv", ",")

# Import données référentiels
academie <- read_csv("./data/Referentiels/n_academie_et_assimile_.csv", locale = locale(encoding = "ISO-8859-1"))
groupe_discipline <- read_csv("./data/Referentiels/n_groupe_discipline_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
secteur_discipline <- read_csv("./data/Referentiels/n_secteur_disciplinaire_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
discipline <- read_csv("./data/Referentiels/n_discipline_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
diplome <- read_csv("./data/Referentiels/n_diplome_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
type_diplome <- read_csv("./data/Referentiels/n_type_diplome_sise_.csv", locale = locale(encoding = "ISO-8859-1"))
etablissement <- read_delim("./data/Referentiels/organismes-2024-04-22.csv", ";")



#------ Base de données avec tous les référentiels

## Formations
formations <- formations |> 
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
           id_etablissement = compos,
           nom_etablissement = `Raison sociale`,
           adresse_etablissement = Adresse,
           degre_etude = degetu,
           id_diplome = diplom,
           nom_diplome = LIBELLE_INTITULE_1,
           id_discipline = discipli,
           nom_discipline = LIBELLE_COURT,
           nom_long_discipline = LIBELLE_DISCIPLINE_60,
           id_groupe_discipline = groupe,
           nom_groupe_discipline = LIBELLE_LONG.x,
           id_secteur_discipline = sectdis,
           nom_secteur_discipline = LIBELLE_SECTEUR_DISCIPLINAIRE,
           id_type_diplome = typ_dipl,
           nom_type_diplome = LIBELLE_LONG.y) |> 
    select(cohorte, degre_etude, cycle, cursus_lmd, id_diplome, nom_diplome, id_type_diplome, nom_type_diplome, id_discipline, nom_discipline, 
           nom_long_discipline, id_groupe_discipline, nom_groupe_discipline, id_secteur_discipline, nom_secteur_discipline, id_etablissement, 
           nom_etablissement, adresse_etablissement, academie) |> 
    distinct()
#rio::export(formations, "data/Export/formations_enrichies.csv")


# Qualité données
# diff_diplome <- test |> 
#     filter(id_type_diplome != TYPE_DIPLOME_SISE) |> 
#     distinct(id_diplome, nom_diplome, id_type_diplome, TYPE_DIPLOME_SISE) |> 
#     rename(diplom = id_diplome,
#            type_diplome_oraccle = id_type_diplome, 
#            type_diplome_sise = TYPE_DIPLOME_SISE) #47 types différents
#rio::export(diff_diplome, "./data/Export/diff_type_diplome.csv")
# diff_secteur_discipline <- test |> filter(id_secteur_discipline != SECTEUR_DISCIPLINAIRE_SISE) |> 
#     distinct(id_diplome, nom_diplome, id_secteur_discipline, SECTEUR_DISCIPLINAIRE_SISE) |> 
#     rename(diplom = id_diplome,
#            secteur_discipline_oraccle = id_secteur_discipline, 
#            secteur_discipline_sise = SECTEUR_DISCIPLINAIRE_SISE) #252 secteurs différents
#rio::export(diff_secteur_discipline, "./data/Export/diff_secteur_discipline.csv")

#a <- table(cohorte_spe$bac_spe1) |> as.data.frame() |> rename(bac_spe1=Var1, nb_ocurrences = Freq) |> arrange(desc(nb_ocurrences))
#rio::export(a, "./data/Export/valeurs_specialite1_bac.csv")
#b <- table(cohorte_spe$bac_spe2) |> as.data.frame() |> rename(bac_spe2=Var1, nb_ocurrences = Freq) |> arrange(desc(nb_ocurrences))
#rio::export(b, "./data/Export/valeurs_specialite2_bac.csv")


## Cohortes
cohortes_unnest <- cohortes |> 
    select(-reussites) |> 
    mutate(parcours_annee = strsplit(as.character(trace), "\\+")) |> 
    unnest(parcours_annee) |> 
    mutate(parcours_annee = strsplit(as.character(parcours_annee), "&")) |> 
    unnest(parcours_annee) |> 
    mutate(formation = str_extract_all(parcours_annee, "(?<=-)([^-&]+)(?=-)"), #extract characters between - and -
           etablissement = str_extract_all(parcours_annee, "([^\\-]+)$")) #extract characters after last -
    #left_join(formations, join_by("parcours_annee" == "cohorte"))



#------ Dataviz


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


# Année du bac
    # Préparation des données
table <- cohorte_anbac |> 
    summarise(nb_etudiants = sum(effectif, na.rm = TRUE), .by = anbac) |> 
    filter(nb_etudiants > 0)
    # Dataviz
graph <- ggplot(table) +
  aes(x = anbac, y = nb_etudiants, text = paste("Année du bac :", anbac,
                                                "\nNombre d'étudiants :", format(as.integer(nb_etudiants), nsmall = 1, big.mark = "."))) +
  geom_col(fill = "#fd710f", alpha = .7) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Année d'obtention", y = "Nombre d'étudiants", title = "Année d'obtention du baccalauréat des étudiants") +
  theme_minimal() +
  custom_theme() +
  theme(plot.margin = margin(0, 0, 5, 0))
ggraph <- ggplotly(graph, tooltip = c("text")) |> 
  layout(annotations = list(text ="En nombre d'étudiants - données cohorte_anbac.csv", 
                      x = 0.1, y = 0.5, font=list(size = 25)))
ggraph
saveWidget(ggraph, file = "figures/distribution_annee_bac.html")


# Sexe des étudiants
    # Préparation des données
cohorte_sexe <- cohorte_sexe |> 
    mutate(cohorteid = as.character(cohorteid),
           sexe = case_match(sexe, 1 ~ "homme", 2 ~ "femme"),
           nb_etudiants_cohorte = sum(effectif), 
           percent = round(effectif / nb_etudiants_cohorte, 5),
           .by = cohorteid)
table_femmes <- cohorte_sexe |> 
    filter(sexe == "femme")
moy <- mean(table_femmes$percent)
    # Dataviz
graph <- ggplot(data = table_femmes) + 
  geom_histogram(aes(x = percent), 
                             fill = "#fd710f", col = "white", alpha = .7, bins = 15) +
  scale_colour_manual("", values = c("#cb1d27")) +
  labs(title = "Distribution de la part des femmes dans les cohortes", x = "Pourcentage de femmes", y = "Nombre de cohortes",
       caption = "Environ 4500 cohortes sont composées de 55% à 60% de femmes") +
  theme_classic() +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept = mean(table_femmes$percent), linetype = 2, col = "#666666", linewidth = .9) +
  geom_label(aes(x = moy+.03, y = 4800, label = paste("Moyenne de", round(moy*100, 0), "%")), 
            col = "#666666", size = 5, hjust = 0, fontface = "italic", family = "Helvetica", fill = "white", label.size = NA) +
  custom_theme() +
  theme(plot.title = ggplot2::element_text(size = 18))
graph
ggsave(file = "figures/distribution_femmes_cohortes.png", plot = graph, width = 9, height = 5)


# Sexe des étudiants par académie (en nombre de cohortes)
    # Préparation des données
table <- cohortes_unnest |> 
    left_join(formations |> select(cohorte, academie), join_by("parcours_annee" == "cohorte")) |> 
    filter(n_distinct(academie) == 1, .by = cohorteid) |> 
    distinct(cohorteid, academie, effectif) |> 
    mutate(cohorteid = as.character(cohorteid)) |> 
    left_join(cohorte_sexe |> select(-c(effectif, nb_etudiants_cohorte)), by = "cohorteid") |> 
    filter(effectif > 0, sexe == "femme") |> 
    mutate(mediane = median(percent, na.rm = TRUE), .by = academie)
    # Dataviz
# table |> 
#      distinct(academie, mediane) |> 
#      arrange(desc(mediane))
graph <- table |> 
    filter(academie %in% c("Mayotte", "Polynésie Française", "Guadeloupe", "Guyane", "Nouvelle Calédonie", "La Réunion", "Martinique", "Aix-Marseille", "Dijon", "Corse")) |> 
    mutate(min = round(min(percent, na.rm = T)*100, 0),
           max = round(max(percent, na.rm = T)*100, 0),
           moy = round(mean(percent, na.rm = TRUE)*100, 0),
           med = round(median(percent, na.rm = TRUE)*100, 0),
           .by = academie) |> 
    ggplot(aes(x = academie, y=percent, 
               tooltip = paste("Académie :", academie, "\nPourcentage de femmes : entre", min, "% et", max, 
                               "% \nMoyenne :", moy, "% \nMédiane :", med, "%"))) + 
      geom_violin_interactive(trim = F, fill = "#fd710f", color = "#fe9c57", position="identity", alpha = .7, scale = "width") + #scale pour avoir même hauteur
      labs(x = "Academie", y = "Pourcentage de femmes", 
           title = "Les 10 des academies ayant la part de femmes la plus élevée", 
           subtitle = "Basé sur les parcours étudiants rattachés à une unique academie (soit 44% \nde tous les parcours)",
           caption = "En Polynésie Française, la moitié des cohortes a moins de 68% de femmes et l'autre moitié \na plus de 68% de femmes, et la part moyenne de femmes est de 63%") +
      theme_classic() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      geom_hline(yintercept = .5, linetype = 2, col = "#666666", linewidth = .7) +
      coord_flip() +
      custom_theme() +
      theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank()) #10 académies sur 34 en tout
ggraph <- girafe(print(graph), width_svg = 11, height_svg = 6)
saveWidget(ggraph, file = "figures/academie_plus_femmes.html")

graph <- table |> 
    filter(academie %in% c("Versailles", "Normandie", "Créteil", "Grenoble", "Rennes", "Limoges", "Poitiers", "Toulouse", "Lille", "Amiens")) |> 
    mutate(min = round(min(percent, na.rm = T)*100, 0),
           max = round(max(percent, na.rm = T)*100, 0),
           moy = round(mean(percent, na.rm = TRUE)*100, 0),
           med = round(median(percent, na.rm = TRUE)*100, 0),
           .by = academie)  |> 
    ggplot(aes(x = academie, y=percent, 
               tooltip = paste("Académie :", academie, "\nPourcentage de femmes : entre", min, "% et", max, 
                               "% \nMoyenne :", moy, "% \nMédiane :", med, "%"))) + 
      geom_violin_interactive(trim = F, fill = "#fe44d5", color = "#fe7ce2", position="identity", alpha = .7, scale = "width") + #scale pour avoir même hauteur
      labs(x = "Academie", y = "Pourcentage de femmes", 
           title = "Les 10 des academies ayant la part de femmes la plus faible", 
           subtitle = "Basé sur les parcours étudiants rattachés à une unique academie (soit 44% de \ntous les parcours)",
           caption = "En Normandie, la part des femmes varie entre 35 et 64%, pour une part moyenne de 51%") +
      theme_classic() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      geom_hline(yintercept = .5, linetype = 2, col = "#666666", linewidth = .7) +
      coord_flip() +
      custom_theme() +
      theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank())
ggraph <- girafe(print(graph), width_svg = 11, height_svg = 6)
saveWidget(ggraph, file = "figures/academie_moins_femmes.html")


# Spécialités au bac
    # Préparation des données
table <- cohorte_spe |> 
    mutate(couple_spe = paste(bac_spe1, "-", bac_spe2)) |> 
    summarise(effectif_couple_spe = sum(effectif, na.rm = TRUE), .by = couple_spe) |> 
    arrange(desc(effectif_couple_spe)) |> 
    filter(couple_spe != "NA - NA")
ref_top10_spe <- table |> filter(row_number() <= 10)

# table <- cohorte_spe |> 
#     mutate(couple_spe = paste(bac_spe1, "-", bac_spe2)) |> 
#     mutate(effectif_couple_spe = sum(effectif, na.rm = TRUE), .by = c(cohorteid, couple_spe)) |> 
#     distinct(cohorteid, couple_spe, effectif_couple_spe) |> 
#     filter(effectif_couple_spe > 0, couple_spe != "NA - NA") |> 
#     left_join(cohorte_anbac, by = "cohorteid") |> 
#     mutate(somme = sum(effectif_couple_spe, na.rm = TRUE), .by = c(anbac, couple_spe)) |> 
#     distinct(nom_discipline, couple_spe, somme, nom_long_discipline)

    # Dataviz
graph <- table |> 
    filter(row_number() <= 10) |> 
    mutate(couple_spe = fct_reorder(couple_spe, effectif_couple_spe)) |> 
    ggplot() + 
    geom_bar_interactive(aes(y=effectif_couple_spe, x=couple_spe, 
                             tooltip = paste("Couple de spécialités :", couple_spe, "\nNombre d'étudiants :", 
                                             format(as.integer(effectif_couple_spe), nsmall = 1, big.mark = "."))),
             position="dodge", stat="identity", width=.6, alpha = .7, fill = "#fd710f") +
    labs(x = "", y = "Nombre d'étudiants", linetype = "",
         title = stringr::str_wrap("Les 10 couples de spécialités les plus choisis par les étudiants", width = 40),
         caption = "Depuis 2021, près de 95.000 étudiants ont choisi les spécialités\nmathématiques et physique chimie") +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    theme_classic() +
    custom_theme() +
    theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.major.y = ggplot2::element_blank())
ggraph <- girafe(print(graph), width_svg = 8, height_svg = 5)
saveWidget(ggraph, file = "figures/top10_specialites.html")


# Disciplines selon spécialités
    # Préparation des données
table <- cohorte_spe |> 
    mutate(couple_spe = paste(bac_spe1, "-", bac_spe2)) |> 
    filter(couple_spe %in% ref_top10_spe$couple_spe) |>
    mutate(effectif_couple_spe = sum(effectif, na.rm = TRUE), .by = c(cohorteid, couple_spe)) |> 
    distinct(cohorteid, couple_spe, effectif_couple_spe) |> #40.120 cohortes ayant les 10 couples de spé les plus choisis
    left_join(cohortes_unnest, by = "cohorteid") |> 
    left_join(formations |> select(cohorte, nom_discipline, nom_long_discipline), join_by("parcours_annee" == "cohorte")) |> 
    filter(n_distinct(nom_discipline) == 1, .by = cohorteid) |> 
    mutate(somme = sum(effectif_couple_spe, na.rm = TRUE), .by = c(nom_discipline, couple_spe)) |> 
    distinct(nom_discipline, couple_spe, somme, nom_long_discipline)
    
    # Dataviz
graph <- table |> 
    filter(!is.na(nom_discipline)) |>
    arrange(nom_discipline) |> 
    mutate(nom_discipline = factor(nom_discipline, levels = rev(unique(nom_discipline)))) |> 
    ggplot(aes(couple_spe, nom_discipline, fill= somme, 
               tooltip = paste("Spécialités :", couple_spe, "\nDiscipline :", toTitleCase(tolower(nom_long_discipline)),
                               "\nNombre d'étudiants :", format(as.integer(somme), nsmall = 1, big.mark = ".")))) + 
        geom_tile_interactive() +
        theme_classic() +
        guides(fill = guide_legend(title = "", reverse = FALSE)) +
        labs(title = stringr::str_wrap("Disciplines de formation des étudiants selon les spécialités choisis au baccalauréat", width = 70), 
             subtitle = "Données des formations rattachées à une unique discipline et des 10 couples \nde spécialités les plus choisies (soit 39% de tous les parcours)",
             y = "Discipline de formation", x = "Couple de spécialités",
             caption = "Près 80.000 étudiants ayant choisi les spécialités mathématiques et physique chimie au baccalauréat \nont suivi une formation dans la discipline des sciences fondamentales et applications") +
        scale_fill_distiller(palette = "RdPu", direction = 1) +    
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, margin = margin(t = -20, r = 0, b = 40, l = 0)),
              plot.margin = margin(50, 0, 10, 0))
ggraph <- girafe(print(graph), width_svg = 11, height_svg = 9)
ggraph
saveWidget(ggraph, file = "figures/disciplines_specialites.html")


