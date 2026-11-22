# Modélisation du prix au m² — préparation des données (pilote : un département)
#
# Entrée  : data/parquet/ventes_logement.parquet (pipeline/03)
# Sortie  : modele/resultats/donnees_<dep>.rds
#
# Protocole d'évaluation (commun à tous les modèles) :
# - apprentissage : ventes du 01/01/<annee_debut> au 30/06/2025
# - test prospectif : les 12 derniers mois (01/07/2025 -> 30/06/2026), jamais vus
#   à l'apprentissage. C'est la situation réelle : estimer le prix actuel d'un bien
#   à partir des ventes passées.
# Coordonnées projetées en Lambert-93 (EPSG:2154), en km, pour que les distances du
# modèle spatial aient un sens (et pas en degrés).

source("pipeline/00_config.R")
suppressPackageStartupMessages(library(sf))

args <- commandArgs(trailingOnly = TRUE)
dep <- if (length(args) >= 1) args[1] else "34"
annee_debut <- 2018
date_test <- as.Date("2025-07-01")

dir.create("modele/resultats", showWarnings = FALSE, recursive = TRUE)
con <- connect_duckdb()
d <- dbGetQuery(con, sprintf("
  SELECT id_mut, date_mutation, annee, semestre, code_commune, nom_commune,
         section_cadastrale, id_parcelle, longitude, latitude, type_local, valeur_fonciere,
         surface, nb_pieces, nb_dependance, surf_terrain, prix_m2
  FROM read_parquet('%s')
  WHERE garde AND code_departement = '%s' AND annee >= %d
    AND longitude IS NOT NULL AND latitude IS NOT NULL", path_ventes, dep, annee_debut))
dbDisconnect(con, shutdown = TRUE)

xy <- sf_project("EPSG:4326", "EPSG:2154", cbind(d$longitude, d$latitude)) / 1000
d$x <- xy[, 1]; d$y <- xy[, 2]

# Temps : semestre (1 = 1er semestre de annee_debut) et temps continu (années)
d$t_sem <- (d$annee - annee_debut) * 2 + d$semestre
d$t <- as.numeric(d$date_mutation - as.Date(sprintf("%d-01-01", annee_debut))) / 365.25

d$log_prix_m2 <- log(d$prix_m2)
d$log_surface <- log(d$surface)
d$pieces <- pmin(pmax(d$nb_pieces, 1), 7)              # 1..7+ ; 0 pièce (rare) -> 1
d$dependance <- factor(pmin(d$nb_dependance, 2), 0:2, c("0", "1", "2+"))
d$log_terrain <- log1p(d$surf_terrain)                 # 0 pour les appartements

d$jeu <- ifelse(d$date_mutation >= date_test, "test", "apprentissage")

msg(sprintf("Département %s : %d ventes géolocalisées (%s)", dep, nrow(d),
            paste(names(table(d$jeu)), table(d$jeu), collapse = ", ")))
print(table(d$type_local, d$jeu))
saveRDS(d, sprintf("modele/resultats/donnees_%s.rds", dep))
