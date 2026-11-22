# Fonctions communes aux modèles : métriques d'évaluation
#
# Tous les modèles prédisent log(prix/m2). La prédiction ponctuelle est la médiane
# exp(mu) ; l'erreur en % sur le prix au m2 est la même que sur la valeur du bien
# (valeur = prix/m2 x surface).
#
# Métriques (sur le jeu test) :
#   mdape      : erreur absolue médiane en % (indicateur principal, robuste)
#   pct_10/20  : part des ventes prédites à ±10 % / ±20 % près
#   rmse_log   : écart quadratique moyen sur l'échelle log (~ erreur relative)
#   r2_log     : R² sur log(prix/m2)
#   biais      : erreur médiane signée en % (> 0 : surestimation)
#   couv80     : part des prix observés dans l'intervalle de prédiction à 80 %
#   larg80     : largeur médiane de l'intervalle à 80 %, en % du prix prédit

evaluer <- function(obs_log, pred_log, q10_log = NULL, q90_log = NULL) {
  ok <- is.finite(obs_log) & is.finite(pred_log)
  obs <- exp(obs_log[ok]); pred <- exp(pred_log[ok])
  err <- (pred - obs) / obs
  res <- data.frame(
    n = sum(ok),
    mdape = 100 * median(abs(err)),
    pct_10 = 100 * mean(abs(err) <= 0.10),
    pct_20 = 100 * mean(abs(err) <= 0.20),
    rmse_log = sqrt(mean((pred_log[ok] - obs_log[ok])^2)),
    r2_log = 1 - sum((pred_log[ok] - obs_log[ok])^2) / sum((obs_log[ok] - mean(obs_log[ok]))^2),
    biais = 100 * median(err)
  )
  if (!is.null(q10_log)) {
    res$couv80 <- 100 * mean(obs_log[ok] >= q10_log[ok] & obs_log[ok] <= q90_log[ok])
    res$larg80 <- 100 * median(exp(q90_log[ok]) - exp(q10_log[ok])) / median(pred)
  } else {
    res$couv80 <- NA_real_; res$larg80 <- NA_real_
  }
  res
}

fmt_metriques <- function(m) {
  m[] <- lapply(m, function(v) if (is.numeric(v)) round(v, 3) else v)
  m
}

# Historique de l'immeuble (même parcelle cadastrale) pour les ventes de `z`, à partir
# des ventes de référence `ref`. anterieures = TRUE : seules les ventes de `ref` de date
# strictement antérieure comptent (z = ref, apprentissage) ; sinon toutes.
# Ajoute imm_n (nombre), imm_lp (log prix/m2 moyen), imm_dt (années écoulées depuis la
# date moyenne), imm_dls (écart de log surface) ; NA si aucune vente.
feat_immeuble <- function(z, ref, anterieures = TRUE) {
  g <- aggregate(cbind(lp = log_prix_m2, t = t, ls = log_surface, n = 1) ~ id_parcelle + date_mutation,
                 data = ref[, c("id_parcelle", "date_mutation", "log_prix_m2", "t", "log_surface")],
                 FUN = sum)
  g <- g[order(g$id_parcelle, g$date_mutation), ]
  cols <- c("lp", "t", "ls", "n")
  if (anterieures) {
    # cumul par immeuble, moins le groupe courant = ventes de dates strictement antérieures
    for (v in cols) g[[v]] <- ave(g[[v]], g$id_parcelle, FUN = cumsum) - g[[v]]
    i <- match(paste(z$id_parcelle, z$date_mutation), paste(g$id_parcelle, g$date_mutation))
  } else {
    g <- aggregate(g[, cols], list(id_parcelle = g$id_parcelle), sum)
    i <- match(z$id_parcelle, g$id_parcelle)
  }
  n <- g$n[i]; n[is.na(n)] <- 0
  moy <- function(v) ifelse(n > 0, g[[v]][i] / n, NA_real_)
  z$imm_n <- n
  z$imm_lp <- moy("lp")
  z$imm_dt <- z$t - moy("t")
  z$imm_dls <- z$log_surface - moy("ls")
  z
}

# Écrit la sortie console dans un .log (seul format texte autorisé par l'antivirus
# hors .R) en plus de l'afficher
journaliser <- function(f) {
  zz <- file(f, open = "wt")
  sink(zz, split = TRUE)
  invisible(zz)
}
