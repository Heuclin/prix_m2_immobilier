# Modèle INLA spatio-temporel du log(prix/m2)
#
#   log(prix/m2)_i = b0 + dépendances_i              (effets fixes)
#                  + f(log surface)_i                (RW2, non linéaire)
#                  + f(pièces)_i                     (RW1)
#                  + f(log terrain)_i                (RW2, maisons seulement)
#                  + g(semestre)_i                   (RW1 : tendance du marché local)
#                  + u(s_i, semestre_i)              (champ spatial Matérn / SPDE,
#                                                     corrélé AR1 d'un semestre à l'autre)
#                  + e_i                             (bruit gaussien)
#
# Coordonnées en Lambert-93 (km). Les ventes de test (12 derniers mois) sont
# incluses avec une réponse manquante : le modèle les prédit sans les avoir vues
# (prévision à 1 et 2 semestres du champ AR1). Intervalle de prédiction = incertitude
# du prédicteur linéaire + bruit d'observation.
#
# Usage : Rscript modele/02_inla.R [dep] [type] [arete_km] [ecart_min_km] [pas]
#   type         : Appartement | Maison | tous            (défaut : tous)
#   arete_km     : longueur max des arêtes du maillage    (défaut : 5)
#   ecart_min_km : distance min entre deux noeuds         (défaut : 2)
#   pas          : sem | an | statique (groupes de temps du champ) (défaut : sem)
#   section      : section | sans  (effet iid par section cadastrale, variance
#                  estimée : capte les écarts locaux plus fins que le maillage)
#   famille      : gaussian | t (vraisemblance de Student, robuste aux ventes atypiques)
# Le coût croît vite avec (nombre de noeuds x nombre de pas de temps).

source("pipeline/00_config.R")
source("modele/fonctions.R")
suppressPackageStartupMessages({ library(INLA); library(fmesher) })

args <- commandArgs(trailingOnly = TRUE)
arg <- function(i, defaut) if (length(args) >= i) args[i] else defaut
dep <- arg(1, "34")
types <- if (arg(2, "tous") == "tous") c("Appartement", "Maison") else arg(2)
arete <- as.numeric(arg(3, 5))
ecart_min <- as.numeric(arg(4, 2))
pas <- arg(5, "sem")
effets_loc <- strsplit(arg(6, "section"), "+", fixed = TRUE)[[1]]   # section, parcelle, sans
avec_section <- "section" %in% effets_loc     # effet iid section cadastrale
avec_parcelle <- "parcelle" %in% effets_loc   # effet iid parcelle (immeuble)
famille <- arg(7, "gaussian")                 # gaussian | t (Student : queues épaisses)
config <- sprintf("a%g_e%g_%s%s%s%s", arete, ecart_min, pas,
                  if (avec_section) "_section" else "", if (avec_parcelle) "_parcelle" else "",
                  if (famille == "t") "_student" else "")
journaliser(sprintf("modele/resultats/02_inla_%s_%s_%s.log", dep, config, paste(types, collapse = "_")))
msg("Configuration : ", config)
d <- readRDS(sprintf("modele/resultats/donnees_%s.rds", dep))
nthreads <- sprintf("%d:1", max(1, parallel::detectCores() - 4))

for (typ in types) {
  dt <- d[d$type_local == typ, ]
  msg(sprintf("== %s : %d ventes (%d test)", typ, nrow(dt), sum(dt$jeu == "test")))

  # Covariables discrétisées sur l'ensemble (sans utiliser la réponse)
  dt$surf_g <- inla.group(dt$log_surface, n = 30, method = "quantile")
  dt$terr_g <- if (typ == "Maison") inla.group(dt$log_terrain, n = 30, method = "cut") else 0
  dt$dep1 <- as.numeric(dt$dependance == "1")
  dt$dep2 <- as.numeric(dt$dependance == "2+")
  reponse <- ifelse(dt$jeu == "test", NA, dt$log_prix_m2)   # NA = à prédire
  # groupes de temps du champ spatio-temporel (la tendance g(t) reste au semestre)
  # "statique" : un seul champ spatial pour toute la période (l'évolution du
  # marché passe alors uniquement par la tendance g(t))
  dt$t_grp <- switch(pas, an = (dt$t_sem + 1) %/% 2, statique = rep(1L, nrow(dt)), dt$t_sem)
  n_sem <- max(dt$t_grp)

  # Maillage : fin là où sont les ventes, grossier en bordure
  t0 <- Sys.time()
  loc <- unique(round(cbind(dt$x, dt$y), 1))
  bord <- list(fm_nonconvex_hull(loc, convex = max(3, arete)),
               fm_nonconvex_hull(loc, convex = 25))
  mesh <- fm_mesh_2d_inla(boundary = bord, max.edge = c(arete, 4 * arete), cutoff = ecart_min)
  msg(sprintf("   maillage : %d noeuds x %d pas de temps = %d effets spatio-temporels (%.0f s)",
              mesh$n, n_sem, mesh$n * n_sem, as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # Priors PC : P(portée < 3 km) = 0.5 ; P(écart-type du champ > 1) = 0.01 ;
  # P(corrélation d'un semestre à l'autre > 0.9) = 0.9
  spde <- inla.spde2.pcmatern(mesh, alpha = 2, prior.range = c(3, 0.5),
                              prior.sigma = c(1, 0.01))
  idx <- inla.spde.make.index("s", n.spde = spde$n.spde, n.group = n_sem)
  A <- inla.spde.make.A(mesh, loc = cbind(dt$x, dt$y), group = dt$t_grp, n.group = n_sem)

  cov <- data.frame(intercept = 1, dep1 = dt$dep1, dep2 = dt$dep2, surf_g = dt$surf_g,
                    pieces = dt$pieces, t_sem = dt$t_sem, terr_g = dt$terr_g,
                    section = as.integer(factor(dt$section_cadastrale)),
                    parcelle = as.integer(factor(dt$id_parcelle)))
  stk <- inla.stack(data = list(y = reponse), A = list(A, 1),
                    effects = list(idx, cov), tag = "obs")

  pc_prec <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  f_terrain <- if (typ == "Maison")
    "+ f(terr_g, model = 'rw2', scale.model = TRUE, hyper = pc_prec)" else ""
  formule <- as.formula(paste(
    "y ~ -1 + intercept + dep1 + dep2",
    "+ f(surf_g, model = 'rw2', scale.model = TRUE, hyper = pc_prec)",
    "+ f(pieces, model = 'rw1', scale.model = TRUE, hyper = pc_prec)",
    f_terrain,
    if (avec_section) "+ f(section, model = 'iid', hyper = pc_prec)" else "",
    if (avec_parcelle) "+ f(parcelle, model = 'iid', hyper = pc_prec)" else "",
    "+ f(t_sem, model = 'rw1', scale.model = TRUE, hyper = pc_prec)",
    if (n_sem == 1) "+ f(s, model = spde)" else paste(
      "+ f(s, model = spde, group = s.group,",
      "    control.group = list(model = 'ar1',",
      "      hyper = list(rho = list(prior = 'pc.cor1', param = c(0.9, 0.9)))))")))

  t0 <- Sys.time()
  fit <- inla(formule, family = famille, data = inla.stack.data(stk, spde = spde),
              control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
              # vraisemblance gaussienne : marginales gaussiennes quasi exactes et
              # bien plus rapides ; hyperparamètres au mode (empirical Bayes)
              control.inla = list(int.strategy = "eb", strategy = "gaussian"),
              control.compute = list(waic = FALSE, dic = FALSE),
              num.threads = nthreads, verbose = FALSE)
  duree <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  msg(sprintf("   INLA ajusté en %.1f min", duree))
  print(round(fit$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")], 3))
  print(round(fit$summary.fixed[, c("mean", "0.025quant", "0.975quant")], 3))

  # Prédictions sur le test : prédicteur linéaire + bruit d'observation
  ii <- inla.stack.index(stk, "obs")$data
  lp <- fit$summary.linear.predictor[ii, ]
  hp <- fit$summary.hyperpar
  sd_bruit <- 1 / sqrt(hp[grep("^Precision for the (Gaussian|student-t) obs", rownames(hp)), "mode"])
  sd_pred <- sqrt(lp$sd^2 + sd_bruit^2)
  tst <- dt$jeu == "test"
  if (famille == "t") {
    # Student d'INLA : bruit = eps / sqrt(précision), eps de Student réduite (variance 1)
    ddl <- hp[grep("^degrees of freedom", rownames(hp)), "mode"]
    msg(sprintf("   degrés de liberté du bruit : %.1f", ddl))
    set.seed(1)
    eps <- rt(4000, ddl) * sqrt((ddl - 2) / ddl) * sd_bruit
    qq <- t(vapply(which(tst), function(i)
      quantile(lp$mean[i] + rnorm(4000, 0, lp$sd[i]) + eps, c(0.1, 0.9), names = FALSE), numeric(2)))
  } else {
    qq <- cbind(lp$mean[tst] - qnorm(0.9) * sd_pred[tst], lp$mean[tst] + qnorm(0.9) * sd_pred[tst])
  }
  m <- evaluer(dt$log_prix_m2[tst], lp$mean[tst], qq[, 1], qq[, 2])
  print(fmt_metriques(cbind(type = typ, modele = paste("INLA", config), m)), row.names = FALSE)
  # contrôle de calibration sur l'apprentissage (résidus "in-sample", donc optimistes)
  r_app <- (dt$log_prix_m2 - lp$mean)[!tst]
  msg(sprintf("   écart-type du bruit %.3f ; résidus d'apprentissage : sd %.3f, |r| médian %.3f",
              sd_bruit, sd(r_app), median(abs(r_app))))

  saveRDS(list(
    type = typ, metriques = cbind(type = typ, modele = paste("INLA", config), m),
    duree_min = duree, mesh = mesh, n_sem = n_sem,
    hyperpar = fit$summary.hyperpar, fixed = fit$summary.fixed,
    effets = fit$summary.random[c("surf_g", "pieces", "t_sem",
                                  if (typ == "Maison") "terr_g")],
    champ = fit$summary.random$s[, c("ID", "mean", "sd")],
    predictions = data.frame(id_mut = dt$id_mut[tst], obs = dt$log_prix_m2[tst],
                             inla = lp$mean[tst], inla_sd = sd_pred[tst],
                             inla_q10 = qq[, 1], inla_q90 = qq[, 2])
  ), sprintf("modele/resultats/02_inla_%s_%s_%s.rds", dep, config, typ))
  rm(fit); gc()
}
sink()
