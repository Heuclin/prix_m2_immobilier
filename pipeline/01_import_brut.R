# Étape 1 : CSV DVF bruts -> Parquet typé, partitionné par année
#
# - détection automatique des fichiers DVF dans dir_source (annuels ou
#   multi-annuels, .csv ou .csv.gz) ; pour chaque mois, on retient le fichier
#   téléchargé le plus récemment qui le couvre (cf. 00_config.R)
# - identifiant unique id_mut = source + id_mutation (les id_mutation sont
#   numérotés par fichier)
# - codes géographiques gardés en texte (zéros initiaux, 2A/2B)

source("pipeline/00_config.R")
con <- connect_duckdb()

csv_opts <- "delim=',', quote='\"', escape='\"', header=true, all_varchar=true"
read_dvf <- function(f) sprintf("read_csv('%s', %s)", f, csv_opts)

# 1. Inventaire des fichiers DVF ------------------------------------------------

est_fichier_dvf <- function(f) {
  entete <- tryCatch(readLines(gzfile(f), n = 1, warn = FALSE), error = function(e) "")
  entete <- sub("^﻿", "", entete)
  all(colonnes_dvf %in% strsplit(entete, ",", fixed = TRUE)[[1]])
}

candidats <- list.files(dir_source, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)
fichiers <- candidats[vapply(candidats, est_fichier_dvf, logical(1))]
ignores <- setdiff(candidats, fichiers)
if (length(ignores)) msg("Ignorés (pas au format DVF) : ", paste(basename(ignores), collapse = ", "))
if (!length(fichiers)) stop("Aucun fichier DVF dans ", dir_source)

msg("Inventaire de ", length(fichiers), " fichiers DVF")
inventaire <- do.call(rbind, lapply(fichiers, function(f) {
  d <- dbGetQuery(con, sprintf("SELECT strftime(CAST(date_mutation AS DATE), '%%Y-%%m') AS mois,
                                count(*) AS n FROM %s GROUP BY 1", read_dvf(f)))
  d$fichier <- f
  d$telecharge_le <- file.info(f)$mtime
  d
}))
inventaire <- inventaire[!is.na(inventaire$mois), ]

# 2. Choix d'un fichier par mois ------------------------------------------------

# priorité : téléchargement le plus récent, puis fichier allant le plus loin
fin_fichier <- tapply(inventaire$mois, inventaire$fichier, max)
inventaire$fin_fichier <- fin_fichier[inventaire$fichier]
inventaire <- inventaire[order(inventaire$mois, -as.numeric(inventaire$telecharge_le),
                               -rank(inventaire$fin_fichier), inventaire$fichier), ]
choix <- inventaire[!duplicated(inventaire$mois), ]
choix$source <- gsub("[^A-Za-z0-9]+", "_", sub("\\.csv(\\.gz)?$", "", basename(choix$fichier)))

# Récapitulatif : plages de mois consécutifs par fichier
mois_num <- function(m) as.integer(substr(m, 1, 4)) * 12 + as.integer(substr(m, 6, 7))
choix$plage <- cumsum(c(1, diff(mois_num(choix$mois)) != 1 |
                          head(choix$fichier, -1) != tail(choix$fichier, -1)))
recap <- do.call(rbind, lapply(split(choix, choix$plage), function(d) data.frame(
  du = min(d$mois), au = max(d$mois), fichier = basename(d$fichier[1]),
  telecharge_le = format(d$telecharge_le[1], "%Y-%m-%d"), n_lignes = sum(d$n))))
rownames(recap) <- NULL
msg("Sources retenues :")
print(recap)
# (Parquet plutôt que CSV : sur ce poste, l'antivirus bloque la création de .csv)
duckdb::duckdb_register(con, "recap", recap)
dbExecute(con, sprintf("COPY recap TO '%s' (FORMAT parquet)",
                       file.path(dir_parquet, "sources_utilisees.parquet")))
duckdb::duckdb_unregister(con, "recap")

# Alertes
non_utilises <- setdiff(fichiers, choix$fichier)
if (length(non_utilises))
  msg("Info : fichiers entièrement remplacés par des versions plus récentes : ",
      paste(basename(non_utilises), collapse = ", "))

tous_mois <- mois_num(choix$mois)
trous <- setdiff(seq(min(tous_mois), max(tous_mois)), tous_mois)
if (length(trous))
  warning("Mois absents de toutes les sources : ",
          paste(sprintf("%d-%02d", (trous - 1) %/% 12, (trous - 1) %% 12 + 1), collapse = ", "))

# Les 5 dernières années couvertes devraient venir du dernier millésime : sinon
# elles n'intègrent pas les révisions d'Etalab.
dernier_dl <- max(choix$telecharge_le)
fenetre <- max(tous_mois) - 59
pas_revises <- choix[mois_num(choix$mois) >= fenetre & choix$telecharge_le < dernier_dl, ]
if (nrow(pas_revises))
  warning(sprintf(paste0("%d mois de la fenêtre DVF des 5 dernières années (%s -> %s) viennent ",
                         "de fichiers plus anciens que le dernier téléchargement : ils n'intègrent ",
                         "pas les dernières révisions. Télécharger le fichier complet pour les mettre à jour."),
                  nrow(pas_revises), min(pas_revises$mois), max(pas_revises$mois)))

# 3. Import ---------------------------------------------------------------------

select_typed <- function(f, src, mois) sprintf("
  SELECT
    '%1$s' AS source,
    '%1$s_' || id_mutation AS id_mut,
    id_mutation,
    CAST(date_mutation AS DATE) AS date_mutation,
    numero_disposition,
    nature_mutation,
    TRY_CAST(valeur_fonciere AS DOUBLE) AS valeur_fonciere,
    adresse_numero, adresse_suffixe, adresse_nom_voie, adresse_code_voie,
    lpad(code_postal, 5, '0')       AS code_postal,
    lpad(code_commune, 5, '0')      AS code_commune,
    nom_commune,
    lpad(code_departement, 2, '0')  AS code_departement,
    ancien_code_commune, ancien_nom_commune,
    id_parcelle, ancien_id_parcelle, numero_volume,
    lot1_numero, TRY_CAST(lot1_surface_carrez AS DOUBLE) AS lot1_surface_carrez,
    lot2_numero, TRY_CAST(lot2_surface_carrez AS DOUBLE) AS lot2_surface_carrez,
    lot3_numero, TRY_CAST(lot3_surface_carrez AS DOUBLE) AS lot3_surface_carrez,
    lot4_numero, TRY_CAST(lot4_surface_carrez AS DOUBLE) AS lot4_surface_carrez,
    lot5_numero, TRY_CAST(lot5_surface_carrez AS DOUBLE) AS lot5_surface_carrez,
    TRY_CAST(nombre_lots AS INTEGER) AS nombre_lots,
    TRY_CAST(code_type_local AS INTEGER) AS code_type_local,
    type_local,
    TRY_CAST(surface_reelle_bati AS DOUBLE) AS surface_reelle_bati,
    TRY_CAST(nombre_pieces_principales AS INTEGER) AS nombre_pieces_principales,
    code_nature_culture, nature_culture,
    code_nature_culture_speciale, nature_culture_speciale,
    TRY_CAST(surface_terrain AS DOUBLE) AS surface_terrain,
    TRY_CAST(longitude AS DOUBLE) AS longitude,
    TRY_CAST(latitude  AS DOUBLE) AS latitude
  FROM %2$s
  WHERE strftime(CAST(date_mutation AS DATE), '%%Y-%%m') IN (%3$s)",
  src, read_dvf(f), paste0("'", mois, "'", collapse = ", "))

par_fichier <- split(choix, choix$fichier)
union_sql <- paste(vapply(par_fichier, function(d)
  select_typed(d$fichier[1], d$source[1], d$mois), ""), collapse = "\nUNION ALL\n")

msg("Import de ", length(par_fichier), " fichiers -> ", path_brut)
unlink(path_brut, recursive = TRUE)
dbExecute(con, sprintf("
  COPY (SELECT *, year(date_mutation) AS annee FROM (%s))
  TO '%s' (FORMAT parquet, PARTITION_BY (annee), COMPRESSION zstd)",
  union_sql, path_brut))

# Contrôle : lignes importées = lignes inventoriées
brut <- sprintf("read_parquet('%s/**/*.parquet', hive_partitioning=true)", path_brut)
ctrl <- dbGetQuery(con, sprintf("
  SELECT annee, count(*) AS n_lignes, count(DISTINCT id_mut) AS n_mutations,
         min(date_mutation) AS dmin, max(date_mutation) AS dmax
  FROM %s GROUP BY ALL ORDER BY annee", brut))
print(ctrl)
stopifnot(sum(ctrl$n_lignes) == sum(choix$n))

dbDisconnect(con, shutdown = TRUE)
msg("Étape 1 terminée")
