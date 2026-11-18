# Prix du mètre carré immobilier en France

Benjamin Heuclin

Statistiques du prix au m² des **appartements** et des **maisons** en France, par
département, commune (arrondissement pour Paris, Lyon et Marseille) et section cadastrale,
calculées à partir des actes de vente enregistrés dans les
[Demandes de valeurs foncières (DVF)](https://www.data.gouv.fr/datasets/demandes-de-valeurs-foncieres-geolocalisees),
de 2014 à aujourd'hui. L'Alsace, la Moselle et Mayotte ne sont pas couverts par DVF.

Note importante : aucune information sur l'état ou la qualité des biens n'est disponible
dans les données.

> **Refonte en cours (2026)** : l'application R Shiny (`app.R`) est remplacée par un
> pipeline de données R / DuckDB et un site web statique. Suivi détaillé dans
> [`journal.md`](journal.md).

## Organisation

| Dossier | Contenu |
|---|---|
| [`pipeline/`](pipeline/) | Préparation des données (R + DuckDB + Parquet) : import des fichiers DVF, une ligne par vente, nettoyage, statistiques, export du site |
| [`site/`](site/) | Site web statique (carte MapLibre GL, JavaScript sans dépendance de build) |
| `data/` | Données sources et intermédiaires (non versionnées) |
| `app.R`, `Script_R/`, `Prediction/` | Ancienne version (Shiny) et premiers essais de prédiction |

## Mettre à jour les données

1. Télécharger le fichier DVF géolocalisées complet (`.csv.gz`) dans `data/DVF_source/`.
   Garder les anciens fichiers : DVF ne republie que les 5 dernières années.
2. Lancer depuis la racine du projet :
   ```sh
   Rscript pipeline/run_all.R          # données et statistiques (~2 min)
   Rscript pipeline/05_export_site.R   # données du site (~1 min ; nécessite 7-Zip)
   ```
3. Tester le site en local :
   ```sh
   python -m http.server 8765 --directory site
   ```
   puis ouvrir http://127.0.0.1:8765

Packages R : `DBI`, `duckdb`, `sf`, `rmapshaper`, `arrow` (lecture des Parquet).

## Licences

- **Code** : [GNU AGPL-3.0](LICENSE).
- **Données produites** (statistiques, fichiers de `site/data/`) :
  [Licence Ouverte 2.0](https://www.etalab.gouv.fr/licence-ouverte-open-licence), voir
  [`LICENCE-DONNEES.md`](LICENCE-DONNEES.md).
- **Conditions DVF** applicables à toute réutilisation : pas de réidentification des
  personnes, pas d'indexation des données par les moteurs de recherche externes.

## Sources des données

- Valeurs foncières : [DVF géolocalisées](https://www.data.gouv.fr/datasets/demandes-de-valeurs-foncieres-geolocalisees) (DGFiP, Etalab), Licence Ouverte 2.0.
- Contours des communes, arrondissements municipaux et départements : [IGN ADMIN EXPRESS COG CARTO](https://geoservices.ign.fr/adminexpress), Licence Ouverte 2.0.
- Contours des sections cadastrales : [cadastre.data.gouv.fr](https://cadastre.data.gouv.fr) (DGFiP, Etalab), Licence Ouverte 2.0.
- Recherche de communes : [API Découpage administratif](https://geo.api.gouv.fr/decoupage-administratif).
- Fond de carte : [OpenFreeMap](https://openfreemap.org), © OpenMapTiles, données © contributeurs OpenStreetMap.

## Historique

- **Décembre 2021** : création de l'application Shiny (données 2016 → 2021).
- **Novembre 2023** : ajout des années 2014-2015 et 2022 → mi-2023, essais de prédiction.
- **Novembre 2026** : nouveau pipeline DuckDB, données 2014 → juin 2026, prototype de
  site statique, changement de licence (AGPL-3.0 pour le code, Licence Ouverte 2.0 pour
  les données).

**Contact** : <a href='mailto:prix.m2.immobilier.contact@gmail.com'>prix.m2.immobilier.contact@gmail.com</a>
