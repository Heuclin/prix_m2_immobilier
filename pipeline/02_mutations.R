# Étape 2 : lignes DVF -> une ligne par mutation
#
# Piège de structure DVF : sur une parcelle, chaque local est répété pour chaque
# nature de culture (ex. une maison sur "sols" + "terrain d'agrément" = 2 lignes
# identiques côté bâti). On corrige en divisant par k = nombre de cultures
# distinctes de la parcelle (produit cartésien local x culture).
# Symétriquement, la surface d'une culture est répétée pour chaque local :
# on la compte une seule fois par (parcelle, culture, culture spéciale, surface).
#
# Codes type local : 1 Maison, 2 Appartement, 3 Dépendance, 4 Local commercial.

source("pipeline/00_config.R")
con <- connect_duckdb()

cols <- "source, id_mut, id_mutation, date_mutation, annee, nature_mutation, valeur_fonciere,
  code_postal, code_commune, nom_commune, code_departement, id_parcelle,
  lot1_surface_carrez, lot2_surface_carrez, lot3_surface_carrez, lot4_surface_carrez,
  lot5_surface_carrez, code_type_local, surface_reelle_bati, nombre_pieces_principales,
  code_nature_culture, code_nature_culture_speciale, surface_terrain, longitude, latitude"

# Une mutation n'a qu'une date -> traitement année par année (mémoire bornée)
sql_annee <- function(a) sprintf("
WITH b AS (
  SELECT %s, substr(id_parcelle, 1, 10) AS section_cadastrale
  FROM read_parquet('%s/annee=%d/*.parquet', hive_partitioning=true)
),
k_parcelle AS (
  SELECT id_mut, id_parcelle,
         greatest(1, count(DISTINCT code_nature_culture)) AS k
  FROM b GROUP BY ALL
),
bk AS (
  SELECT b.*, k.k FROM b JOIN k_parcelle k USING (id_mut, id_parcelle)
),
carrez AS (
  SELECT *, coalesce(lot1_surface_carrez, 0) + coalesce(lot2_surface_carrez, 0) +
            coalesce(lot3_surface_carrez, 0) + coalesce(lot4_surface_carrez, 0) +
            coalesce(lot5_surface_carrez, 0) AS surf_carrez
  FROM bk
),
bati AS (
  SELECT id_mut,
    any_value(date_mutation)  AS date_mutation,
    any_value(annee)          AS annee,
    any_value(source)         AS source,
    any_value(id_mutation)    AS id_mutation,
    any_value(nature_mutation) AS nature_mutation,
    count(DISTINCT nature_mutation) AS n_nature,
    max(valeur_fonciere)      AS valeur_fonciere,
    count(DISTINCT valeur_fonciere) AS n_valeurs,
    any_value(code_departement) AS code_departement,
    count(DISTINCT code_commune) AS n_communes,
    mode(code_commune)        AS code_commune,
    mode(nom_commune)         AS nom_commune,
    mode(code_postal)         AS code_postal,
    coalesce(mode(section_cadastrale) FILTER (code_type_local IN (1, 2)),
             mode(section_cadastrale)) AS section_cadastrale,
    count(DISTINCT section_cadastrale) AS n_sections,
    count(DISTINCT id_parcelle) AS n_parcelles,
    coalesce(avg(longitude) FILTER (code_type_local IN (1, 2)), avg(longitude)) AS longitude,
    coalesce(avg(latitude)  FILTER (code_type_local IN (1, 2)), avg(latitude))  AS latitude,
    -- locaux (dédoublonnés par k)
    sum(1.0 / k) FILTER (code_type_local = 2)                     AS nb_apt,
    sum(surface_reelle_bati / k) FILTER (code_type_local = 2)     AS surf_apt,
    sum(surf_carrez / k) FILTER (code_type_local = 2)             AS surf_carrez_apt,
    sum(nombre_pieces_principales / k) FILTER (code_type_local = 2) AS pieces_apt,
    sum(1.0 / k) FILTER (code_type_local = 1)                     AS nb_maison,
    sum(surface_reelle_bati / k) FILTER (code_type_local = 1)     AS surf_maison,
    sum(nombre_pieces_principales / k) FILTER (code_type_local = 1) AS pieces_maison,
    sum(1.0 / k) FILTER (code_type_local = 3)                     AS nb_dependance,
    sum(1.0 / k) FILTER (code_type_local = 4)                     AS nb_local,
    sum(surface_reelle_bati / k) FILTER (code_type_local = 4)     AS surf_local
  FROM carrez GROUP BY id_mut
),
terrain AS (
  SELECT id_mut,
    sum(surface_terrain) AS surf_terrain,
    sum(surface_terrain) FILTER (code_nature_culture = 'S')          AS surf_sols,
    sum(surface_terrain) FILTER (code_nature_culture IN ('J', 'AG')) AS surf_jardin,
    sum(surface_terrain) FILTER (code_nature_culture = 'AB')         AS surf_a_batir,
    sum(surface_terrain) FILTER (code_nature_culture NOT IN ('S', 'J', 'AG', 'AB')) AS surf_autre
  FROM (SELECT DISTINCT id_mut, id_parcelle, code_nature_culture,
               code_nature_culture_speciale, surface_terrain
        FROM b WHERE surface_terrain IS NOT NULL)
  GROUP BY id_mut
)
SELECT
  bati.* EXCLUDE (nb_apt, nb_maison, nb_dependance, nb_local),
  month(date_mutation) AS mois,
  quarter(date_mutation) AS trimestre,
  -- nombres de locaux : entiers si l'hypothèse 'produit cartésien' tient
  round(coalesce(nb_apt, 0))::INTEGER        AS nb_apt,
  round(coalesce(nb_maison, 0))::INTEGER     AS nb_maison,
  round(coalesce(nb_dependance, 0))::INTEGER AS nb_dependance,
  round(coalesce(nb_local, 0))::INTEGER      AS nb_local,
  (abs(coalesce(nb_apt, 0) - round(coalesce(nb_apt, 0))) > 1e-6 OR
   abs(coalesce(nb_maison, 0) - round(coalesce(nb_maison, 0))) > 1e-6) AS flag_dedup_incertain,
  coalesce(terrain.surf_terrain, 0) AS surf_terrain,
  coalesce(surf_sols, 0) AS surf_sols, coalesce(surf_jardin, 0) AS surf_jardin,
  coalesce(surf_a_batir, 0) AS surf_a_batir, coalesce(surf_autre, 0) AS surf_autre
FROM bati LEFT JOIN terrain USING (id_mut)
", cols, path_brut, a)

annees <- sort(as.integer(sub("annee=", "", list.files(path_brut, pattern = "^annee="))))
unlink(path_mutations, recursive = TRUE)
for (a in annees) {
  msg("Mutations ", a)
  dir.create(file.path(path_mutations, paste0("annee=", a)), recursive = TRUE, showWarnings = FALSE)
  dbExecute(con, sprintf("COPY (SELECT * EXCLUDE (annee) FROM (%s)) TO '%s/annee=%d/data.parquet'
                          (FORMAT parquet, COMPRESSION zstd)", sql_annee(a), path_mutations, a))
}

mut <- sprintf("read_parquet('%s/**/*.parquet', hive_partitioning=true)", path_mutations)
dbExecute(con, sprintf("
  CREATE OR REPLACE VIEW m AS
  SELECT *, CASE
    WHEN nb_local > 0 THEN 'local'
    WHEN nb_apt = 1 AND nb_maison = 0 THEN 'appartement'
    WHEN nb_maison = 1 AND nb_apt = 0 THEN 'maison'
    WHEN nb_apt + nb_maison > 1 THEN 'multi_logements'
    WHEN nb_dependance > 0 THEN 'dependance_seule'
    ELSE 'terrain' END AS type_vente
  FROM %s", mut))

msg("Contrôles :")
print(dbGetQuery(con, "SELECT count(*) AS n_mutations,
  avg(flag_dedup_incertain::INT) AS part_dedup_incertain,
  avg((n_valeurs > 1)::INT) AS part_plusieurs_valeurs,
  avg((n_communes > 1)::INT) AS part_multi_communes,
  avg((longitude IS NULL)::INT) AS part_sans_geoloc FROM m"))
print(dbGetQuery(con, "SELECT type_vente, count(*) n FROM m
                       WHERE nature_mutation = 'Vente' GROUP BY 1 ORDER BY n DESC"))

dbDisconnect(con, shutdown = TRUE)
msg("Étape 2 terminée")
