# Licence des données produites

Le **code** de ce dépôt est distribué sous licence **GNU AGPL-3.0** (voir [`LICENSE`](LICENSE)).

Les **données produites** par ce projet (statistiques de prix au m² : effectifs, quantiles,
médianes, et à venir les prix estimés par modélisation), ainsi que les fichiers de données
du site (`site/data/`), sont mises à disposition sous la
**[Licence Ouverte / Open Licence version 2.0](https://www.etalab.gouv.fr/licence-ouverte-open-licence)**
(Etalab).

Vous êtes libre de les reproduire, copier, adapter, modifier, extraire, transformer et
exploiter, y compris commercialement, sous réserve de **mentionner la source** et la date de
dernière mise à jour, par exemple :

> Prix du m² immobilier — Benjamin Heuclin, d'après les données DVF (DGFiP / Etalab),
> mise à jour du JJ/MM/AAAA — https://github.com/Heuclin/prix_m2_immobilier

## Conditions liées aux données DVF

Les données de base proviennent des
[Demandes de valeurs foncières](https://www.data.gouv.fr/datasets/demandes-de-valeurs-foncieres)
(DGFiP), publiées sous Licence Ouverte 2.0 avec des conditions particulières qui s'imposent
à toute réutilisation, y compris des données dérivées :

- l'utilisation ne doit pas permettre la **réidentification des personnes** concernées, même
  indirectement (article R112 A-3 du Livre des procédures fiscales) ;
- la réutilisation ne doit pas permettre **l'indexation des données par des moteurs de
  recherche externes**.

Ce projet ne publie que des statistiques agrégées : quantiles masqués en dessous de
5 ventes, quantiles extrêmes (5 % et 95 %) masqués en dessous de 20 ventes, aucune
transaction individuelle.

## Sources et licences

| Donnée | Producteur | Licence |
|---|---|---|
| [DVF géolocalisées](https://www.data.gouv.fr/datasets/demandes-de-valeurs-foncieres-geolocalisees) | DGFiP, Etalab | Licence Ouverte 2.0 + conditions ci-dessus |
| [ADMIN EXPRESS COG CARTO](https://geoservices.ign.fr/adminexpress) (contours des communes, arrondissements municipaux, départements) | IGN | Licence Ouverte 2.0 |
| [Cadastre](https://cadastre.data.gouv.fr) (contours des sections, chargés par le site) | DGFiP, Etalab | Licence Ouverte 2.0 |
| Fond de carte [OpenFreeMap](https://openfreemap.org) | © OpenMapTiles, données © contributeurs OpenStreetMap | ODbL (données OSM) ; simple affichage, attribution sur la carte |
