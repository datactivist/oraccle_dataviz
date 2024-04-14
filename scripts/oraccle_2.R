library(tidyverse)
library(jsonlite)

cohortes_bacs <- read_delim("https://data.unif.app/data/cohortes_bacs.csv", ";")
# effectifs par type de bacs
cohortes <- read_delim("https://data.unif.app/data/cohortes.csv", ";")

etablissements <- read_delim("https://data.unif.app/data/etablissements.csv", ";")
ips <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets/fr-en-ips_colleges/exports/csv?lang=fr&qv1=(0921779J)&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B", ";")
#data_ips : https://data.education.gouv.fr/explore/dataset/fr-en-ips_colleges/table/?disjunctive.rentree_scolaire&disjunctive.academie&disjunctive.code_du_departement&disjunctive.departement&disjunctive.uai&disjunctive.code_insee_de_la_commune&disjunctive.nom_de_la_commune&disjunctive.secteur&q=0921779J
# public / privé, IPS, ecart-type

cohortes_sexes <- read_delim("https://data.unif.app/data/cohortes_sexes.csv", ";")
cohortes_uai <- read_delim("https://data.unif.app/data/cohortes_uai.csv", ";")
# grande base par cohorte : nb établissements, sexes, bacs, options, % privé / public

geoloc <- read_delim("https://data.unif.app/data/fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre.csv", ";")
# carto des établissements

cohortes_mentions <- read_delim("https://data.unif.app/data/cohortes_mentions.csv", ";")


##----

# 11 avril 24, import des données pour mission

cohortes_age_premiereins <- read_delim("./data/France/cohorte_age_premiereins.csv", ";")
cohorte_anbac <- read_delim("./data/France/cohorte_anbac.csv", ",")
cohorte_bac <- read_delim("./data/France/cohorte_bac.csv", ",")
cohorte_derniereins <- read_delim("./data/France/cohorte_derniereins.csv", ",")
cohorte_premiereins <- read_delim("./data/France/cohorte_premiereins.csv", ",")
cohorte_sexe <- read_delim("./data/France/cohorte_sexe.csv", ",")
cohorte_spe <- read_delim("./data/France/cohorte_spe.csv", ",")
cohortes <- read_delim("./data/France/cohortes.csv", ",")
formations <- read_delim("./data/France/formations.csv", ",")

