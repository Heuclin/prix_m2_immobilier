# Configuration commune du pipeline DVF
# Benjamin Heuclin
#
# Toutes les étapes sont en SQL DuckDB : lecture directe des CSV (même .gz),
# calcul out-of-core, sorties en Parquet (colonnes typées, compressées,
# lisibles depuis R, Python ou le navigateur).

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

# Racine du projet (le pipeline se lance depuis la racine : Rscript pipeline/run_all.R)
dir_source  <- "data/DVF_source"
dir_parquet <- "data/parquet"
dir.create(dir_parquet, showWarnings = FALSE, recursive = TRUE)

path_brut      <- file.path(dir_parquet, "dvf_brut")             # partitionné par annee
path_mutations <- file.path(dir_parquet, "mutations")           # 1 ligne / mutation, partitionné par annee
path_ventes    <- file.path(dir_parquet, "ventes_logement.parquet") # ventes simples apt / maison
dir_agregats   <- file.path(dir_parquet, "agregats")

# Sources DVF : détectées automatiquement dans dir_source (cf. 01_import_brut.R).
# Tout fichier .csv / .csv.gz au format DVF géolocalisé est pris en compte, quel
# que soit son découpage (annuel, multi-annuel). Pour chaque mois, on retient le
# fichier téléchargé le plus récemment (date de modification) qui couvre ce mois :
# DVF ne publie que 5 années glissantes et révise les années passées à chaque
# millésime. Les anciens fichiers restent la source des années qui ne sont plus
# publiées.
# Le choix fait est écrit dans data/parquet/sources_utilisees.parquet.
colonnes_dvf <- c("id_mutation", "date_mutation", "nature_mutation", "valeur_fonciere",
                  "code_commune", "id_parcelle", "type_local", "surface_reelle_bati",
                  "longitude", "latitude")

# Paramètres de nettoyage des ventes de logements
param <- list(
  surface_min = 9,        # m2 (seuil "logement décent")
  surface_max = 500,      # m2
  valeur_min  = 5000,     # EUR
  prix_m2_min = 100,      # bornes absolues de plausibilité (EUR/m2)
  prix_m2_max = 40000,
  terrain_max_maison = 20000, # m2 : au-delà, vente plutôt agricole / propriété
  z_robuste_max = 4,      # écart robuste max sur log(prix_m2) (en MAD)
  n_min_groupe_z = 30,    # effectif min du groupe commune x type x annee pour le z
  n_min_affichage = 5     # en dessous, les quantiles ne sont pas publiés
)

connect_duckdb <- function(threads = max(1, parallel::detectCores() - 2),
                           memory = "20GB") {
  con <- dbConnect(duckdb(shared_home = FALSE))
  tmp <- file.path(tempdir(), "duckdb_tmp")
  dir.create(tmp, showWarnings = FALSE)
  dbExecute(con, sprintf("SET threads = %d", threads))
  dbExecute(con, sprintf("SET memory_limit = '%s'", memory))
  dbExecute(con, sprintf("SET temp_directory = '%s'", gsub("\\\\", "/", tmp)))
  dbExecute(con, "SET preserve_insertion_order = false")
  con
}

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
