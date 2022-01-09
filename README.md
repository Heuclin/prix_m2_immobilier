


# Prix du mètre carré immobilier en France

Benjamin Heuclin

Application R shiny fournissant des résumés statistiques (médianes, moyennes, quantiles ...) du prix de l'immobilier par type de bien (appartements, maisons et locaux commerciaux ou industriels) et par département, commune et section cadastrale. Ces résumés statistiques sont calculés à partir des données issues des actes notariés des transactions immobilières intervenues depuis 2016 sur le territoire métropolitain et les DOM-TOM, à l’exception de l’Alsace, de la Moselle et de Mayotte.

Note importante : aucune information sur la qualité des biens n'est fourni dans les données.


**Pour lancer l'application avec `R` :**
```{r}
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(stringr)
library(DBI)
library(RSQLite)
runGitHub("Heuclin/prix_m2_immobilier")
```

**Licence :** [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/)


**Sources des données :**
                 
* Données brutes : Valeurs foncières : [https://www.data.gouv.fr/en/datasets/demandes-de-valeurs-foncieres-geolocalisees/](https://www.data.gouv.fr/en/datasets/demandes-de-valeurs-foncieres-geolocalisees/)

* Contours des départements et des communes de France : [https://github.com/gregoiredavid/france-geojson](https://github.com/gregoiredavid/france-geojson)

* Contours des arrondissements de Paris : [https://www.data.gouv.fr/en/datasets/arrondissements-1/](https://www.data.gouv.fr/en/datasets/arrondissements-1/) 

* Contours des arrondissements de Lyon :  [https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5](https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5)

* Contours des arrondissements de Marseille : [https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/](https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/)

* Contours des sections cadastrales :  [https://cadastre.data.gouv.fr/datasets/cadastre-etalab](https://cadastre.data.gouv.fr/datasets/cadastre-etalab)

* Liste des départements de France : [https://www.data.gouv.fr/en/datasets/departements-de-france/](https://www.data.gouv.fr/en/datasets/departements-de-france/)

* Liste des codes postaux de France : [https://www.data.gouv.fr/en/datasets/base-officielle-des-codes-postaux/](https://www.data.gouv.fr/en/datasets/base-officielle-des-codes-postaux/)
                  

**A venir :**
                 
* Inclure les ventes de terrain (nécessite un traîtement particulier) 
* Créer un prédicteur de valeur foncière 
                 
**Date de création :**  décembre 2021 

**Dernière mise à jour :**  03/01/2022

**Contact :**  <a href='mailto: prix.m2.immobilier.contact@gmail.com'> prix.m2.immobilier.contact@gmail.com 
                
