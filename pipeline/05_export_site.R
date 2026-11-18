# Étape 5 : export des données du site web (site/data/)
#
# Contours : IGN ADMIN EXPRESS COG CARTO (Licence Ouverte 2.0), version
# généralisée pour la cartographie : communes, arrondissements municipaux de
# Paris / Lyon / Marseille, départements. Téléchargé (.7z, ~125 Mo) dans un
# dossier temporaire ; nécessite 7-Zip.
# (Les contours Etalab "contours administratifs" sont sous ODbL : non utilisés
# pour ne pas imposer le partage à l'identique aux données du site.)
# Les sections cadastrales ne sont pas exportées : le site charge leurs contours
# directement depuis cadastre.data.gouv.fr (Licence Ouverte), commune par commune.
#
# Statistiques : pour chaque zone, une propriété `s` (JSON) :
#   {"A": {"2025": [n, q05, q25, med, q75, q95], "12m": [...], ...}, "M": {...}}
#   A = appartements, M = maisons ; quantiles à null si n < param$n_min_affichage.
#
# Tous les fichiers sont des .geojson (seul format JSON que l'antivirus du poste
# laisse créer ; les sections sont une FeatureCollection sans géométrie).

source("pipeline/00_config.R")
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
con <- connect_duckdb()

dir_site <- "site/data"
for (d in file.path(dir_site, c("communes", "sections")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# 1. Contours ---------------------------------------------------------------------
# Édition à mettre à jour chaque année (liste : data.geopf.fr/telechargement/resource/ADMIN-EXPRESS-COG-CARTO)
edition_ign <- "ADMIN-EXPRESS-COG-CARTO_4-0__GPKG_WGS84G_FRA_2026-01-01"
url_ign <- sprintf("https://data.geopf.fr/telechargement/download/ADMIN-EXPRESS-COG-CARTO/%1$s/%1$s.7z",
                   edition_ign)

# Cache utilisateur (hors du projet) ; extraction "à plat" car les chemins de
# l'archive dépassent la limite Windows de 260 caractères
dir_ign <- file.path(tools::R_user_dir("app_dvf", "cache"), edition_ign)
dir.create(dir_ign, recursive = TRUE, showWarnings = FALSE)
f_gpkg <- list.files(dir_ign, pattern = "\\.gpkg$", full.names = TRUE)
if (!length(f_gpkg)) {
  exe_7z <- Filter(nzchar, c(Sys.which("7z"), "C:/Program Files/7-Zip/7z.exe"))
  exe_7z <- exe_7z[file.exists(exe_7z)][1]
  if (is.na(exe_7z)) stop("7-Zip est nécessaire pour décompresser ADMIN EXPRESS")
  dir.create(dir_ign, showWarnings = FALSE)
  f_7z <- file.path(dir_ign, "admin_express.7z")
  msg("Téléchargement IGN ", edition_ign)
  options(timeout = 900)
  download.file(url_ign, f_7z, mode = "wb", quiet = TRUE)
  system2(exe_7z, c("e", "-y", shQuote(paste0("-o", dir_ign)), shQuote(f_7z), "*.gpkg", "-r"),
          stdout = FALSE)
  unlink(f_7z)
  f_gpkg <- list.files(dir_ign, pattern = "\\.gpkg$", full.names = TRUE)
}
msg("Contours : ", basename(f_gpkg[1]))
stopifnot(length(f_gpkg) == 1)

lire <- function(couche, code, nom, dep = NULL) {
  x <- st_read(f_gpkg, couche, quiet = TRUE)
  st_sf(code = x[[code]], nom = x[[nom]],
        departement = if (is.null(dep)) NA_character_ else x[[dep]],
        geometry = st_geometry(x))
}
departements <- lire("departement", "code_insee", "nom_officiel")[, c("code", "nom")]
communes <- lire("commune", "code_insee", "nom_officiel", "code_insee_du_departement")
plm <- lire("arrondissement_municipal", "code_insee", "nom_officiel")
plm$departement <- substr(plm$code, 1, 2)
# Paris, Lyon, Marseille : DVF utilise les codes d'arrondissement
communes <- rbind(communes[!communes$code %in% c("75056", "69123", "13055"), ], plm)

# Simplification topologique (rmapshaper / mapshaper : pas de trous ni de
# chevauchements entre voisins). Proportion de points conservés :
simpl_dep <- 0.04   # vue nationale
simpl_com <- 0.25   # vue départementale
simplifier <- function(x, keep) {
  suppressWarnings(rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE,
                                           weighting = 0.7))
}
departements <- simplifier(departements, simpl_dep)

# 2. Statistiques en JSON (DuckDB) ----------------------------------------------------
agr <- function(niv) sprintf("read_parquet('%s/prix_m2_%s.parquet')", dir_agregats, niv)
stats_json <- function(niv, cle) dbGetQuery(con, sprintf("
  WITH t AS (
    SELECT %2$s AS code, CASE type_local WHEN 'Appartement' THEN 'A' ELSE 'M' END AS t,
           json_group_object(periode, json_array(n, q05, q25, med, q75, q95)) AS j
    FROM %1$s GROUP BY ALL)
  SELECT code, json_group_object(t, j)::VARCHAR AS s FROM t GROUP BY code", agr(niv), cle))

ecrire_geojson <- function(x, f) {
  unlink(f)
  st_write(x, f, driver = "GeoJSON", quiet = TRUE,
           layer_options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))
}

# France (une feature sans géométrie)
s_fr <- stats_json("france", "'FR'")$s
fin <- format(dbGetQuery(con, sprintf("SELECT max(date_mutation) d FROM read_parquet('%s')",
                                      path_ventes))$d)
writeLines(sprintf('{"type":"FeatureCollection","features":[{"type":"Feature","geometry":null,"properties":{"code":"FR","nom":"France","fin":"%s","s":%s}}]}', fin, s_fr),
           file.path(dir_site, "france.geojson"), useBytes = TRUE)

# Départements
s_dep <- stats_json("departement", "code_departement")
dep_out <- merge(departements, s_dep, by = "code", all.x = TRUE)
ecrire_geojson(dep_out, file.path(dir_site, "departements.geojson"))
msg("departements.geojson : ", nrow(dep_out), " départements, ",
    sum(!is.na(dep_out$s)), " avec données")

# Communes et sections, par département
s_com <- stats_json("commune", "code_commune")
s_sec <- dbGetQuery(con, sprintf("
  WITH t AS (
    SELECT code_departement AS dep, section_cadastrale AS code,
           CASE type_local WHEN 'Appartement' THEN 'A' ELSE 'M' END AS t,
           json_group_object(periode, json_array(n, q05, q25, med, q75, q95)) AS j
    FROM %s GROUP BY ALL)
  SELECT dep, code, json_group_object(t, j)::VARCHAR AS s FROM t GROUP BY dep, code", agr("section")))

for (dep in sort(unique(communes$departement))) {
  com_dep <- simplifier(communes[communes$departement == dep, c("code", "nom")], simpl_com)
  com_dep <- merge(com_dep, s_com, by = "code", all.x = TRUE)
  ecrire_geojson(com_dep, file.path(dir_site, "communes", paste0(dep, ".geojson")))

  sec <- s_sec[s_sec$dep == dep, ]
  feats <- sprintf('{"type":"Feature","geometry":null,"properties":{"code":"%s","s":%s}}',
                   sec$code, sec$s)
  writeLines(paste0('{"type":"FeatureCollection","features":[', paste(feats, collapse = ","), "]}"),
             file.path(dir_site, "sections", paste0(dep, ".geojson")), useBytes = TRUE)
}

# Contrôle de jointure : ventes dont la commune n'a pas de contour 2026
manq <- setdiff(s_com$code, communes$code)
msg("Communes DVF sans contour IGN (anciennes communes fusionnées) : ",
    length(manq), " / ", nrow(s_com))
tailles <- file.info(list.files(dir_site, recursive = TRUE, full.names = TRUE))$size
msg(sprintf("Export : %d fichiers, %.1f Mo", length(tailles), sum(tailles) / 1e6))

dbDisconnect(con, shutdown = TRUE)
msg("Étape 5 terminée")
