# Modèles de référence : médiane locale et forêt aléatoire (ranger)
#
# 1. Médiane locale : ce que fait l'application aujourd'hui. Prix/m2 médian des
#    24 derniers mois d'apprentissage dans la section cadastrale (si >= 5 ventes),
#    sinon la commune, sinon le département. Pas de covariable.
# 2. Forêt aléatoire de quantiles sur log(prix/m2) : coordonnées (et coordonnées
#    tournées, qui aident les arbres à découper l'espace en diagonale), temps,
#    surface, pièces, dépendances, terrain. Pas d'identifiant de zone (code postal,
#    section) : l'information spatiale passe par les coordonnées.
# 3. Même forêt + historique de l'immeuble (parcelle cadastrale) : nombre de ventes
#    antérieures, leur log(prix/m2) moyen, leur ancienneté moyenne et l'écart de
#    surface. Pour l'apprentissage, seules les ventes STRICTEMENT antérieures comptent
#    (pas de fuite de la réponse) ; pour le test, toutes les ventes d'apprentissage
#    (l'information dont disposerait le modèle en conditions réelles, comme INLA).
#
# Usage : Rscript modele/01_reference_rf.R [dep]

source("pipeline/00_config.R")
source("modele/fonctions.R")
suppressPackageStartupMessages(library(ranger))

args <- commandArgs(trailingOnly = TRUE)
dep <- if (length(args) >= 1) args[1] else "34"
journaliser(sprintf("modele/resultats/01_reference_rf_%s.log", dep))
d <- readRDS(sprintf("modele/resultats/donnees_%s.rds", dep))

resultats <- list(); predictions <- list()

for (typ in c("Appartement", "Maison")) {
  dt <- d[d$type_local == typ, ]
  app <- dt[dt$jeu == "apprentissage", ]; tst <- dt[dt$jeu == "test", ]
  msg(sprintf("== %s : %d ventes d'apprentissage, %d de test", typ, nrow(app), nrow(tst)))

  # 1. Médiane locale --------------------------------------------------------------
  recent <- app[app$date_mutation >= as.Date("2023-07-01"), ]
  med_par <- function(cle) {
    a <- aggregate(log_prix_m2 ~ g, data.frame(g = recent[[cle]], log_prix_m2 = recent$log_prix_m2),
                   function(v) c(med = median(v), n = length(v)))
    setNames(a$log_prix_m2[, "med"], a$g)[a$log_prix_m2[, "n"] >= 5]
  }
  m_sec <- med_par("section_cadastrale"); m_com <- med_par("code_commune")
  m_dep <- median(recent$log_prix_m2)
  pred_med <- unname(m_sec[tst$section_cadastrale])
  pred_med[is.na(pred_med)] <- unname(m_com[tst$code_commune[is.na(pred_med)]])
  pred_med[is.na(pred_med)] <- m_dep
  resultats[[length(resultats) + 1]] <- cbind(type = typ, modele = "Médiane locale",
                                             evaluer(tst$log_prix_m2, pred_med))

  # 2. Forêt aléatoire de quantiles -------------------------------------------------
  var_rf <- c("x", "y", "xy_p", "xy_m", "t", "log_surface", "pieces", "nb_dependance")
  if (typ == "Maison") var_rf <- c(var_rf, "log_terrain")
  prep <- function(z) { z$xy_p <- z$x + z$y; z$xy_m <- z$x - z$y; z }
  app <- prep(app); tst <- prep(tst)
  t0 <- Sys.time()
  rf <- ranger(x = app[, var_rf], y = app$log_prix_m2, num.trees = 500,
               mtry = 4, min.node.size = 10, quantreg = TRUE,
               num.threads = max(1, parallel::detectCores() - 2), seed = 1)
  q <- predict(rf, tst[, var_rf], type = "quantiles", quantiles = c(0.1, 0.5, 0.9))$predictions
  msg(sprintf("   forêt aléatoire ajustée en %.0f s (OOB R² = %.3f)",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), rf$r.squared))
  resultats[[length(resultats) + 1]] <- cbind(type = typ, modele = "Forêt aléatoire",
                                             evaluer(tst$log_prix_m2, q[, 2], q[, 1], q[, 3]))

  # 3. Forêt + historique de l'immeuble -----------------------------------------------
  app <- feat_immeuble(app, app, anterieures = TRUE)
  tst <- feat_immeuble(tst, app, anterieures = FALSE)
  msg(sprintf("   ventes avec historique d'immeuble : %.0f %% (apprentissage), %.0f %% (test)",
              100 * mean(app$imm_n > 0), 100 * mean(tst$imm_n > 0)))
  var_imm <- c(var_rf, "imm_n", "imm_lp", "imm_dt", "imm_dls")
  t0 <- Sys.time()
  rf2 <- ranger(x = app[, var_imm], y = app$log_prix_m2, num.trees = 500,
                mtry = 5, min.node.size = 10, quantreg = TRUE, importance = "impurity",
                num.threads = max(1, parallel::detectCores() - 2), seed = 1)
  q2 <- predict(rf2, tst[, var_imm], type = "quantiles", quantiles = c(0.1, 0.5, 0.9))$predictions
  msg(sprintf("   forêt + immeuble ajustée en %.0f s (OOB R² = %.3f)",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), rf2$r.squared))
  imp <- sort(rf2$variable.importance, decreasing = TRUE)
  print(round(100 * imp / sum(imp), 1))
  resultats[[length(resultats) + 1]] <- cbind(type = typ, modele = "Forêt + immeuble",
                                             evaluer(tst$log_prix_m2, q2[, 2], q2[, 1], q2[, 3]))

  predictions[[typ]] <- data.frame(id_mut = tst$id_mut, obs = tst$log_prix_m2,
                                   mediane_locale = pred_med, rf = q[, 2],
                                   rf_q10 = q[, 1], rf_q90 = q[, 3],
                                   rf_imm = q2[, 2], rf_imm_q10 = q2[, 1], rf_imm_q90 = q2[, 3],
                                   imm_n = tst$imm_n)
}

res <- do.call(rbind, resultats)
print(fmt_metriques(res), row.names = FALSE)
saveRDS(list(metriques = res, predictions = predictions),
        sprintf("modele/resultats/01_reference_rf_%s.rds", dep))
sink()
