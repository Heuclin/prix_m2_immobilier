# Étape 4 : statistiques descriptives du prix au m2
#
# Niveaux : France, département, commune (arrondissements pour Paris/Lyon/
# Marseille), section cadastrale. Périodes : chaque année civile + les
# 12 derniers mois disponibles ('12m'). Une seule passe GROUPING SETS.
# Les quantiles ne sont publiés que si n >= param$n_min_affichage (sinon NULL,
# n reste affiché).

source("pipeline/00_config.R")
con <- connect_duckdb()

ventes <- sprintf("read_parquet('%s')", path_ventes)
date_max <- dbGetQuery(con, sprintf("SELECT max(date_mutation) d FROM %s", ventes))$d
date_12m <- seq(date_max, by = "-1 year", length.out = 2)[2] + 1
msg("Fenêtre glissante 12 mois : ", format(date_12m), " -> ", format(date_max))

dbExecute(con, sprintf("
  CREATE TEMP TABLE base AS
  SELECT type_local, code_departement, code_commune, section_cadastrale, prix_m2, surface,
         CAST(annee AS VARCHAR) AS periode
  FROM %1$s WHERE garde
  UNION ALL
  SELECT type_local, code_departement, code_commune, section_cadastrale, prix_m2, surface,
         '12m' AS periode
  FROM %1$s WHERE garde AND date_mutation >= DATE '%2$s'", ventes, date_12m))

nmin <- as.integer(param$n_min_affichage)
stat <- function(expr) sprintf("CASE WHEN count(*) >= %d THEN round(%s) END", nmin, expr)

sql <- sprintf("
SELECT
  CASE WHEN grouping(section_cadastrale) = 0 THEN 'section'
       WHEN grouping(code_commune) = 0       THEN 'commune'
       WHEN grouping(code_departement) = 0   THEN 'departement'
       ELSE 'france' END AS niveau,
  code_departement, code_commune, section_cadastrale,
  type_local, periode,
  count(*) AS n,
  %s AS q05, %s AS q25, %s AS med, %s AS q75, %s AS q95, %s AS moyenne,
  %s AS surface_med
FROM base
GROUP BY GROUPING SETS (
  (type_local, periode),
  (type_local, periode, code_departement),
  (type_local, periode, code_departement, code_commune),
  (type_local, periode, code_departement, code_commune, section_cadastrale)
)",
  stat("quantile_cont(prix_m2, 0.05)"), stat("quantile_cont(prix_m2, 0.25)"),
  stat("median(prix_m2)"), stat("quantile_cont(prix_m2, 0.75)"),
  stat("quantile_cont(prix_m2, 0.95)"), stat("avg(prix_m2)"), stat("median(surface)"))

unlink(dir_agregats, recursive = TRUE)
dir.create(dir_agregats, recursive = TRUE)
dbExecute(con, sprintf("CREATE TEMP TABLE agr AS %s", sql))
for (niv in c("france", "departement", "commune", "section")) {
  f <- file.path(dir_agregats, paste0("prix_m2_", niv, ".parquet"))
  n <- dbExecute(con, sprintf("COPY (SELECT * FROM agr WHERE niveau = '%s'
    ORDER BY code_departement, code_commune, section_cadastrale, type_local, periode)
    TO '%s' (FORMAT parquet, COMPRESSION zstd)", niv, f))
  msg(sprintf("%-12s %9d lignes -> %s", niv, n, f))
}

msg("Contrôle : part des lignes publiables (n >= ", nmin, ") par niveau, période 12m")
print(dbGetQuery(con, "SELECT niveau, type_local, count(*) AS n_zones,
  round(100 * avg((med IS NOT NULL)::INT), 1) AS pct_publiable, median(n) AS n_median
  FROM agr WHERE periode = '12m' GROUP BY ALL ORDER BY niveau, type_local"))
print(dbGetQuery(con, "SELECT code_departement, type_local, n, q25, med, q75
  FROM agr WHERE niveau = 'departement' AND periode = '12m'
    AND code_departement IN ('75', '34', '13', '23') ORDER BY 1, 2"))

dbDisconnect(con, shutdown = TRUE)
msg("Étape 4 terminée")
