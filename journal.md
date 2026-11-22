# Journal du projet — Prix du m² immobilier (DVF)

Suivi de l'évolution du travail : décisions, réalisations, constats, prochaines étapes.
Entrées les plus récentes en haut.

---

## 2026-11-19 — Prototype « Estimer un bien » et couche « Estimation » (Hérault)

### Décisions
- Modèle publié : **INLA gaussien, champ statique 2,5 km + section + immeuble**. Il fait jeu
  égal avec la forêt (R² test 0,728 contre 0,714 pour les appartements) et, étant additif,
  il se **calcule dans le navigateur** : pas de serveur. La forêt reste une référence.
- Quarto : pas pour l'application (carte et formulaire = JavaScript). Plus tard, pour les
  pages de méthodologie.

### Essais complémentaires
- Forêt + historique de l'immeuble (ventes antérieures de la même parcelle) : 12,4 → 12,2 %
  (apt), 14,5 → 14,2 % (maisons), mais R² en baisse (0,714 → 0,700). La forêt isolait déjà
  l'immeuble par ses coordonnées (toutes les ventes d'un immeuble ont les mêmes).
- INLA avec vraisemblance de Student (appartements) : 12,46 % (contre 12,52 %), R² 0,726,
  mais **127 min** contre 1,3 min (on perd le raccourci gaussien). Bruit à queues épaisses
  (3,8 degrés de liberté) mais aucun gain utile → abandonné au profit de la calibration
  des intervalles ci-dessous.

### Export du modèle (`modele/04_export_modele.R`)
- Composantes exportées (moyenne, écart-type) : constante, dépendances, courbes surface /
  pièces / terrain, tendance par semestre, champ spatial aux nœuds du maillage (en WGS84),
  effets section et immeuble par commune.
- Deux ajustements par type : sur l'apprentissage (contrôles, calibration) puis sur toutes les
  ventes (modèle publié, prix au 1er semestre 2026). ~30 min pour l'Hérault.
- **Contrôle** : la prédiction par addition des composantes redonne INLA sur le test
  (12,52 % pour les appartements, 14,70 % pour les maisons).
- **Intervalle** : écart-type ≈ racine de la somme des variances (corrélations ignorées,
  1,27 fois trop large) × **k calibré sur le test** (0,71 apt, 0,77 maisons) → couverture
  80,0 %, largeur 53 % (apt) / 66 % (maisons).
- Sorties en `.geojson` (FeatureCollection vide + membre `modele`) car l'antivirus bloque
  la création de `.json` : `site/data/modele/34.geojson` (0,4 Mo) et
  `site/data/modele/34/<commune>.geojson` (340 fichiers, 2,5 Mo).

### Site
- Onglets « Explorer les prix » / « Estimer un bien ».
- Estimer : adresse avec autocomplétion (géocodage IGN Géoplateforme, BAN) ou clic sur la
  carte (repère déplaçable) → parcelle par l'API Carto IGN (sinon la plus proche à ~20 m :
  le point d'adresse tombe souvent sur la voirie) → effets section et immeuble. Affiche la
  valeur, le prix au m², la fourchette à 80 % et l'effet section / immeuble en %.
  Lien direct : `#estimer/lon,lat[/M]`.
- Couche « Estimation » : prix d'un bien type (appartement 60 m² 3 pièces ; maison 100 m²
  4 pièces 500 m² de terrain) par commune (moyenne des emplacements des ventes) et par
  section (calcul dans le navigateur au centre de la section). Encart « Estimation du
  modèle » dans le panneau, à côté de la médiane observée.
- Testé (Edge sans interface) : Montpellier centre 228 000 € pour 60 m² (170–305 k€),
  immeuble avec 62 ventes passées : effet +12 %, fourchette resserrée (181–284 k€) ;
  hors Hérault : message « pas encore de modèle ».

### À faire
- Vérification visuelle dans le navigateur (couche « Estimation », formulaire, mobile).
- Passage à tous les départements (bande de ventes des départements voisins).

---

## 2026-11-18 (suite) — Modèles de prédiction, pilote Hérault (34)

### Protocole
- Dossier `modele/` : `00_donnees.R` (extraction, Lambert-93 en km, variables),
  `fonctions.R` (métriques), `01_reference_rf.R`, `02_inla.R` (paramétré),
  `03_comparaison.R`. Résultats en `.rds` / `.log` dans `modele/resultats/`.
- Réponse : log(prix/m²). Apprentissage : 01/2018 → 06/2025. **Test prospectif : 12 derniers
  mois** (07/2025 → 06/2026), jamais vus. Appartements : 85 418 / 11 063 ventes ;
  maisons : 67 225 / 8 320.
- Métriques : erreur absolue médiane en % (indicateur principal), part à ±10 % / ±20 %,
  R² sur le log, biais, couverture et largeur de l'intervalle à 80 %.

### Résultats (test, 12 derniers mois)

| Modèle | Appart. erreur méd. | Maisons erreur méd. | Remarques |
|---|---|---|---|
| Médiane locale (section → commune → dép., 24 mois) | 15,4 % | 19,2 % | ce que fait l'app actuelle |
| Forêt aléatoire de quantiles (ranger) | **12,5 %** | **14,5 %** | intervalles 80 % bien calibrés (82 % / 85 %), ~20 s |
| INLA, maillage 2,5 km × 17 semestres (47 124 effets) | – | – | arrêté après > 25 min |
| INLA, maillage 5 km × 17 semestres (15 640 effets) | 16,4 % | – | 14,5 min ; portée 4,8 km, σ champ 0,69, ρ = 0,999 |
| INLA 5 km × 17 semestres + section iid | 14,1 % | – | 24 min |
| INLA 5 km, champ **statique** + section | 14,6 % | – | **18 s** |
| INLA 2,5 km statique + section | 14,4 % | – | 1 min |
| INLA 1 km statique + section (16 395 nœuds) | 14,1 % | – | 12 min |
| **INLA 2,5 km statique + section + parcelle (immeuble)** | **12,5 %** | 14,8 % | 1,3 min / 5,3 min ; R² 0,73 (apt) |
| **Moyenne forêt + INLA (section + parcelle)** | **11,9 %** | **14,3 %** | meilleur résultat |

- **ρ = 0,999** entre semestres : la géographie des prix est quasi figée → un champ spatial
  **statique** + tendance temporelle RW1 suffit et divise le temps de calcul par ~80.
- Le maillage fin apporte peu (14,6 → 14,1 %) : la résolution n'est pas le facteur limitant.
- **Effet immeuble (parcelle cadastrale, iid)** : 91,6 % des appartements vendus sur les
  12 derniers mois sont dans un immeuble ayant déjà des ventes (27 % pour les maisons).
  Écart-type de l'effet ≈ 0,21 (log) : c'est l'information la plus utile.
  `id_parcelle` ajouté aux étapes 02 et 03 du pipeline.
- Intervalles INLA un peu trop larges (couverture 85-86 % pour 80 % visés).

- Forêt : la localisation porte 75-85 % de l'importance ; erreur plus forte pour les grands
  appartements (17,5 % > 120 m²) et hors Montpellier (13,5 % contre 10,7 %).
- INLA à maillage grossier : ne voit pas les écarts de prix à l'échelle du quartier
  (centre / périphérie, front de mer) → **ajout d'un effet iid par section cadastrale**
  (variance estimée, pas fixée comme en 2023) ; stratégie `gaussian` pour accélérer.
- Corrections par rapport au script de 2023 : coordonnées en Lambert-93 (et non en degrés),
  plus de fuite des données de test dans la réponse, plus d'effet section à précision fixée,
  surface en RW2 sur log(surface), évaluation prospective.

---

## 2026-11-18 (suite) — Changement de licences, contours IGN

### Décisions
- **Code : GNU AGPL-3.0** (copyleft : toute version modifiée mise en ligne doit publier
  son code). Remplace CC BY-NC-ND 4.0 (inadaptée au code, incompatible avec l'ODbL).
- **Données produites : Licence Ouverte 2.0** (comme DVF, cadastre, Admin Express).
- **Contours : IGN ADMIN EXPRESS COG CARTO** (Licence Ouverte) au lieu des contours
  Etalab (ODbL, partage à l'identique).

### Réalisé
- `LICENSE` (texte AGPL-3.0), `LICENCE-DONNEES.md` (Licence Ouverte 2.0, mention de
  source, conditions DVF, tableau des sources), ancien `LICENCE.txt` supprimé (git rm).
- `README.md` réécrit (refonte, organisation, mise à jour des données, licences, sources,
  historique).
- `05_export_site.R` : contours IGN Admin Express COG CARTO édition 2026 (GPKG, .7z via
  7-Zip, cache dans `tools::R_user_dir("app_dvf", "cache")`), simplification
  topologique avec `rmapshaper` (départements 4 %, communes 25 % des points).
  Données du site : 190 Mo (communes 71 Mo, sections 110 Mo).
- `04_agregats.R` : q05 / q95 masqués si n < 20 (`param$n_min_extremes`), pour ne pas
  publier de quasi-prix individuels.
- Site : mentions de licences et des conditions DVF ; `site/robots.txt` interdit `/data/`.

### À noter
- `robots.txt` n'est lu qu'à la racine d'un domaine : sur une page de projet GitHub
  (`heuclin.github.io/prix_m2_immobilier/`), il sera sans effet → prévoir un domaine
  propre ou une balise / en-tête `noindex` sur les fichiers de données.
- `app.R` (ancienne app Shiny) affiche encore la licence CC BY-NC-ND : à retirer avec
  l'ancienne version.

---

## 2026-11-18 (suite) — Vérification des licences des données

| Donnée | Licence | Statut pour le site |
|---|---|---|
| DVF géolocalisées (DGFiP / Etalab) | Licence Ouverte 2.0 **+ conditions** : pas de réidentification des personnes (art. R112 A-3 LPF), pas d'indexation des données par les moteurs de recherche externes | OK : seuls des agrégats (n ≥ 5) sont publiés, aucune transaction individuelle. Précautions à prendre (voir À faire) |
| Contours administratifs Etalab (`etalab-datasets.geo.data.gouv.fr`) | **ODbL** (jeu compilé à partir d'IGN Admin Express + OpenStreetMap pour les COM) | Partage à l'identique : nos `site/data/*.geojson` (contours + statistiques) seraient une base dérivée à diffuser sous ODbL, ce qui est incompatible avec CC BY-NC-ND |
| Arrondissements PLM via API Géo | Même jeu « contours administratifs » → ODbL | Idem |
| Cadastre Etalab (sections, chargées à la volée) | Licence Ouverte 2.0 | OK (attribution présente) |
| IGN Admin Express (source d'origine des contours) | Licence Ouverte 2.0 | Alternative sans partage à l'identique |
| Fond de carte OpenFreeMap | Service MIT ; données OpenStreetMap (ODbL), schéma OpenMapTiles | OK : simple affichage (« produced work »), attribution ajoutée automatiquement par MapLibre |
| MapLibre GL JS | BSD-3 | OK |
| Ancien `data/france-geojson` (Admin Express 2018) | Licence Ouverte | Plus utilisé (contours 2018 obsolètes) |
| Ancien `data/taux.csv` (toutsurmesfinances.com) | **Pas une donnée ouverte** (contenu d'un site commercial) | Plus utilisé ; si besoin, prendre la série Banque de France (Webstat) |

---

## 2026-11-18 — Reprise du projet, nouveau pipeline et prototype de site

### Contexte
- Application R Shiny créée en 2021, mise à jour en 2023 (données 2014 → juin 2023).
- Nouvelles données téléchargées : `data/DVF_source/dvf_juillet2021_juin2026.csv.gz`
  (DVF géolocalisées, juillet 2021 → juin 2026).
- Objectifs : mettre à jour les données, sortir de Shiny (trop lourd et lent),
  diffuser au grand public, ajouter un modèle de prédiction INLA spatio-temporel.

### Décisions
- **Diffusion grand public** via un **site statique** (pas de serveur R) :
  carte MapLibre GL en JavaScript + fichiers de données précalculés.
- **Pipeline de données en R + DuckDB + Parquet** (remplace `Script_R/script_prix_m2.R`).
- **Sources DVF détectées automatiquement** : pour chaque mois, le fichier téléchargé le
  plus récemment est retenu. Recommandation : télécharger le fichier complet à chaque
  millésime (Etalab révise les 5 dernières années). Ne jamais supprimer les anciens
  fichiers : 2014 → mi-2021 ne sont plus publiés.
- **Quarto** : retenu pour plus tard, pour les pages éditoriales (méthodologie, modèle
  INLA, à propos) autour de la carte, avec publication par GitHub Actions.

### Réalisé
- Revue du projet existant. Problèmes relevés dans l'ancien code :
  - fuite de données dans le modèle INLA (`script_prediction.R` : la réponse utilise
    toutes les données, l'évaluation « test » est en fait sur l'apprentissage) ;
  - SPDE en degrés (à projeter en Lambert-93), effet iid section à précision fixée,
    taux d'intérêt confondu avec l'effet temps ;
  - médiane unique 2014 → 2023 sur la carte, pas de seuil d'effectif, type « Local »
    proposé mais jamais calculé, année 2020 codée en dur.
- **Nouveau pipeline** `pipeline/` (`Rscript pipeline/run_all.R`, ~2 min pour toute la France) :
  - `01_import_brut.R` : CSV → Parquet typé partitionné par année (43,8 M lignes) ;
    identifiant unique `id_mut` = source + id_mutation (les id sont numérotés par fichier) ;
  - `02_mutations.R` : une ligne par mutation (17,4 M) ; correction de la duplication des
    locaux par nature de culture ;
  - `03_ventes_logement.R` : ventes d'un seul appartement / d'une seule maison
    (10,2 M, dont 9,7 M gardées), motif d'exclusion explicite, prix atypiques repérés
    par rapport à la commune (écart robuste sur log prix/m²) ;
  - `04_agregats.R` : quantiles France / département / commune / section, par année et
    sur 12 mois glissants, masqués si n < 5 ;
  - `05_export_site.R` : export des données du site (contours Etalab 2026 + API Géo).
- **Prototype de site** `site/` (`index.html`, `style.css`, `app.js`) : carte à 3 niveaux
  (département → commune → section cadastrale), filtres type / période, panneau avec
  chiffres clés et courbe d'évolution, recherche de commune, URL partageable.
  Testé en local (`python -m http.server 8765 --directory site`) : validé par Benjamin.
- Commit et push sur la branche `pipeline-duckdb` (pipeline uniquement ; `site/` et
  `05_export_site.R` pas encore commités).

### Constats
- **Ancienne règle « une seule ligne »** : écartait ~24 % des ventes de maisons (celles avec
  jardin), car DVF répète chaque local pour chaque nature de culture de la parcelle.
- **Bug corrigé** : `lpad()` de DuckDB tronque les chaînes trop longues (« 971 » → « 97 ») :
  les 4 DOM étaient fusionnés.
- Ancien fichier 2023 : premier semestre seulement. Le nouveau fichier contient les
  années 2021 → 2023 révisées.
- **Sur 12 mois, seules 19 % des sections ont au moins 5 ventes de maisons**, et la moitié
  des communes n'ont pas 5 ventes d'appartements → argument fort pour le lissage par un
  modèle spatio-temporel (INLA).
- Médianes nationales cohérentes : ~3 350 €/m² (appartements) et ~2 170 €/m² (maisons)
  en 2025 ; pic en 2022-2023, repli en 2024.

### Environnement (poste Windows)
- R 4.6.1 : `C:/Users/heuclin/AppData/Local/Programs/R/R-4.6.1/bin/Rscript.exe`
  (pas dans le PATH).
- L'antivirus (WithSecure) bloque la création de certains fichiers sous `D:\Mes Donnees`
  depuis R / les terminaux (`.csv`, `.txt`, `.json`, `.html`, `.js`…) mais pas `.parquet`,
  `.geojson`, `.log`, `.R`. Voir `../CLAUDE.md`. D'où les sorties en Parquet / GeoJSON.

### À faire
- [ ] Modèle INLA spatio-temporel, pilote sur l'Hérault (34) : log prix/m², SPDE Matérn en
      Lambert-93 × AR1 par semestre, covariables (surface, pièces, dépendances, terrain),
      validation croisée spatiale + test prospectif, comparaison avec ranger.
- [ ] Afficher les prix lissés du modèle sur le site.
- [ ] Alléger les données du site (180 Mo, surtout les sections) avant mise en ligne.
- [ ] Harmoniser les communes fusionnées (table de passage COG INSEE).
- [ ] Raccourcis vers l'outre-mer sur la carte.
- [ ] Site Quarto (méthodologie, modèle, à propos) embarquant la carte ; publication
      GitHub Pages par GitHub Actions.
- [x] Commiter `site/` et `05_export_site.R` (commit `bb0a78f`, poussé sur `pipeline-duckdb`).
- [x] Fusionner `pipeline-duckdb` dans `main` (pull request #1, merge `d19e149`).
- [x] Licences : contours IGN Admin Express (Licence Ouverte) ; code AGPL-3.0, données
      Licence Ouverte 2.0.
- [x] DVF : `robots.txt` interdisant `/data/` ; q05 / q95 masqués si n < 20 ; conditions
      DVF dans les mentions du site.
- [ ] Non-indexation des données effective sur GitHub Pages (voir « À noter » ci-dessus).
- [ ] Nettoyer : anciens scripts (`Script_R/`, `Prediction/`), `app.R` Shiny, fichier de
      test `data/parquet/zz_test.txt`.
