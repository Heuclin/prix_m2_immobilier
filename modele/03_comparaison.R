# Comparaison des modèles sur le jeu test (12 derniers mois)
#
# Usage : Rscript modele/03_comparaison.R [dep]
# Sortie : modele/resultats/03_comparaison_<dep>.log / .rds
# Prend toutes les configurations INLA présentes (02_inla_<dep>_<config>_<type>.rds).

source("pipeline/00_config.R")
source("modele/fonctions.R")

args <- commandArgs(trailingOnly = TRUE)
dep <- if (length(args) >= 1) args[1] else "34"
journaliser(sprintf("modele/resultats/03_comparaison_%s.log", dep))

ref <- readRDS(sprintf("modele/resultats/01_reference_rf_%s.rds", dep))
tab <- ref$metriques
tab$duree_min <- NA_real_
inla_res <- list()
for (fi in Sys.glob(sprintf("modele/resultats/02_inla_%s_*.rds", dep))) {
  r <- readRDS(fi)
  inla_res[[fi]] <- r
  tab <- rbind(tab, cbind(r$metriques, duree_min = r$duree_min))
}
tab <- tab[order(tab$type, tab$mdape), ]
cat("\nPerformances sur les ventes des 12 derniers mois (jamais vues à l'apprentissage)\n\n")
print(fmt_metriques(tab), row.names = FALSE)

# Moyenne des prédictions forêt + INLA (sur l'échelle log)
for (r in inla_res) {
  p <- merge(ref$predictions[[r$type]], r$predictions[, c("id_mut", "inla")], by = "id_mut")
  cat(sprintf("\n%s - moyenne forêt + %s :\n", r$type, r$metriques$modele))
  print(fmt_metriques(rbind(
    cbind(foret = "Forêt", evaluer(p$obs, (p$rf + p$inla) / 2)),
    if (!is.null(p$rf_imm)) cbind(foret = "Forêt + immeuble", evaluer(p$obs, (p$rf_imm + p$inla) / 2)))),
    row.names = FALSE)
}
saveRDS(tab, sprintf("modele/resultats/03_comparaison_%s.rds", dep))
sink()
