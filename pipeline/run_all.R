# Lance tout le pipeline DVF, depuis la racine du projet :
#   Rscript pipeline/run_all.R
#
# Entrées  : data/DVF_source/ (CSV DVF géolocalisées, cf. `sources` dans 00_config.R)
# Sorties  : data/parquet/
#   dvf_brut/                 lignes DVF typées, partitionnées par année
#   mutations/                une ligne par mutation (locaux dédoublonnés)
#   ventes_logement.parquet   ventes simples apt / maison + motif d'exclusion
#   agregats/prix_m2_*.parquet  quantiles par niveau x type x période
#
# Pour ajouter un nouveau millésime : déposer le(s) fichier(s) dans
# data/DVF_source/ (fichier complet ou annuels) et relancer (~2 min). Pour chaque
# mois, le fichier téléchargé le plus récemment est retenu ; le choix est affiché
# et écrit dans data/parquet/sources_utilisees.parquet.
# Ne jamais supprimer les anciens fichiers : DVF ne republie que 5 ans.

t0 <- Sys.time()
for (etape in c("01_import_brut.R", "02_mutations.R",
                "03_ventes_logement.R", "04_agregats.R")) {
  cat("\n========", etape, "========\n")
  source(file.path("pipeline", etape), local = new.env())
}
cat("\nPipeline terminé en", format(round(Sys.time() - t0, 1)), "\n")
