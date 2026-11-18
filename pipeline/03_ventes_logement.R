# Étape 3 : ventes simples de logements (1 appartement OU 1 maison)
#
# Table de référence pour les statistiques descriptives ET la modélisation.
# Aucune ligne n'est supprimée après la sélection du périmètre : chaque vente
# porte un `motif_exclusion` (NULL si gardée) pour pouvoir auditer / ajuster
# les règles, et la colonne `garde`.
#
# Périmètre : nature 'Vente' (hors VEFA, adjudications, échanges), une seule
# commune, un seul logement, pas de local commercial. Les dépendances (cave,
# parking) sont autorisées et comptées dans nb_dependance.

source("pipeline/00_config.R")
con <- connect_duckdb()

mut <- sprintf("read_parquet('%s/**/*.parquet', hive_partitioning=true)", path_mutations)
p <- param

sql <- sprintf("
WITH v AS (
  SELECT
    id_mut, date_mutation, annee, mois, trimestre,
    CASE WHEN mois <= 6 THEN 1 ELSE 2 END AS semestre,
    code_departement, code_commune, nom_commune, code_postal, section_cadastrale,
    longitude, latitude,
    CASE WHEN nb_apt = 1 THEN 'Appartement' ELSE 'Maison' END AS type_local,
    valeur_fonciere,
    CASE WHEN nb_apt = 1 THEN surf_apt ELSE surf_maison END AS surface,
    CASE WHEN nb_apt = 1 THEN nullif(surf_carrez_apt, 0) END AS surface_carrez,
    CASE WHEN nb_apt = 1 THEN pieces_apt ELSE pieces_maison END AS nb_pieces,
    nb_dependance,
    CASE WHEN nb_apt = 1 THEN 0 ELSE surf_terrain END AS surf_terrain,
    CASE WHEN nb_apt = 1 THEN 0 ELSE surf_jardin  END AS surf_jardin,
    surf_a_batir, surf_autre,
    flag_dedup_incertain
  FROM %1$s
  WHERE nature_mutation = 'Vente' AND n_nature = 1 AND n_communes = 1
    AND nb_local = 0
    AND ((nb_apt = 1 AND nb_maison = 0) OR (nb_maison = 1 AND nb_apt = 0))
),
v2 AS (
  SELECT *, valeur_fonciere / nullif(surface, 0) AS prix_m2,
    CASE
      WHEN flag_dedup_incertain                         THEN 'dedoublonnage_incertain'
      WHEN valeur_fonciere IS NULL                      THEN 'valeur_manquante'
      WHEN surface IS NULL OR surface < %2$f            THEN 'surface_trop_petite'
      WHEN surface > %3$f                               THEN 'surface_trop_grande'
      WHEN valeur_fonciere < %4$f                       THEN 'valeur_trop_faible'
      WHEN type_local = 'Maison' AND surf_terrain > %7$f THEN 'terrain_trop_grand'
      WHEN surf_a_batir > 0                             THEN 'terrain_a_batir_inclus'
      WHEN valeur_fonciere / surface < %5$f             THEN 'prix_m2_trop_faible'
      WHEN valeur_fonciere / surface > %6$f             THEN 'prix_m2_trop_eleve'
    END AS motif_pre
  FROM v
),
-- écart robuste au prix local : log(prix_m2) vs médiane et MAD du groupe
-- commune x type x année (repli département si effectif insuffisant)
g_com AS (
  SELECT code_commune, type_local, annee, count(*) n_com,
         median(ln(prix_m2)) med_com, mad(ln(prix_m2)) mad_com
  FROM v2 WHERE motif_pre IS NULL GROUP BY ALL
),
g_dep AS (
  SELECT code_departement, type_local, annee,
         median(ln(prix_m2)) med_dep, mad(ln(prix_m2)) mad_dep
  FROM v2 WHERE motif_pre IS NULL GROUP BY ALL
),
v3 AS (
  SELECT v2.*,
    CASE WHEN n_com >= %8$d AND mad_com > 0
         THEN (ln(prix_m2) - med_com) / (1.4826 * mad_com)
         ELSE (ln(prix_m2) - med_dep) / (1.4826 * mad_dep) END AS z_prix
  FROM v2
  LEFT JOIN g_com USING (code_commune, type_local, annee)
  LEFT JOIN g_dep USING (code_departement, type_local, annee)
)
SELECT * EXCLUDE (motif_pre, flag_dedup_incertain),
  ln(prix_m2) AS log_prix_m2,
  coalesce(motif_pre,
           CASE WHEN abs(z_prix) > %9$f THEN 'prix_atypique_local' END) AS motif_exclusion,
  coalesce(motif_pre,
           CASE WHEN abs(z_prix) > %9$f THEN 'x' END) IS NULL AS garde
FROM v3
", mut, p$surface_min, p$surface_max, p$valeur_min, p$prix_m2_min, p$prix_m2_max,
   p$terrain_max_maison, as.integer(p$n_min_groupe_z), p$z_robuste_max)

msg("Ventes de logements -> ", path_ventes)
dbExecute(con, sprintf("COPY (%s ORDER BY code_departement, code_commune, date_mutation)
                        TO '%s' (FORMAT parquet, COMPRESSION zstd)", sql, path_ventes))

ventes <- sprintf("read_parquet('%s')", path_ventes)
msg("Bilan des exclusions :")
print(dbGetQuery(con, sprintf("
  SELECT type_local, coalesce(motif_exclusion, '(gardée)') AS motif, count(*) AS n,
         round(100 * count(*) / sum(count(*)) OVER (PARTITION BY type_local), 2) AS pct
  FROM %s GROUP BY 1, 2 ORDER BY type_local, n DESC", ventes)))
print(dbGetQuery(con, sprintf("
  SELECT annee, type_local, count(*) n, round(median(prix_m2)) med_prix_m2
  FROM %s WHERE garde GROUP BY ALL ORDER BY type_local, annee", ventes)))

dbDisconnect(con, shutdown = TRUE)
msg("Étape 3 terminée")
