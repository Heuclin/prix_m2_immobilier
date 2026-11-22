# Export du modèle INLA pour le site : composantes additives en JSON
#
# Le modèle retenu (02_inla.R, configuration a2.5_e0.8_statique_section_parcelle,
# vraisemblance gaussienne) est une SOMME de composantes :
#   log(prix/m2) = b0 + dépendances + f(surface) + f(pièces) [+ f(terrain)]
#                + tendance(semestre) + champ spatial(x, y) + effet section + effet immeuble
# Chaque composante est exportée (moyenne a posteriori + écart-type) ; le navigateur
# reconstitue la prédiction par addition, sans serveur.
#
# Intervalle de prédiction (approché) : écart-type = k x racine(somme des variances des
# composantes + variance du bruit). Les corrélations a posteriori entre composantes
# sont ignorées ; le facteur k corrige cette approximation : il est calibré pour que
# l'intervalle à 80 % contienne 80 % des prix du jeu test.
#
# Deux ajustements par type de bien :
#   1. sur l'apprentissage (avant le 01/07/2025) : contrôle que la reconstruction par
#      composantes redonne les prédictions d'INLA, calibration de k sur le test ;
#   2. sur toutes les ventes : modèle publié (prix au dernier semestre disponible).
#
# Sorties (extension .geojson : seule extension texte que l'antivirus laisse créer ;
# ce sont des FeatureCollection vides portant les données en membre « modele ») :
#   site/data/modele/<dep>.geojson           maillages, champs, courbes, estimations communes
#   site/data/modele/<dep>/<commune>.geojson effets section et immeuble de la commune
#
# Usage : Rscript modele/04_export_modele.R [dep]

source("pipeline/00_config.R")
source("modele/fonctions.R")
suppressPackageStartupMessages({ library(INLA); library(fmesher); library(sf); library(jsonlite) })

args <- commandArgs(trailingOnly = TRUE)
dep <- if (length(args) >= 1) args[1] else "34"
arete <- 2.5; ecart_min <- 0.8
journaliser(sprintf("modele/resultats/04_export_modele_%s.log", dep))
d <- readRDS(sprintf("modele/resultats/donnees_%s.rds", dep))
nthreads <- sprintf("%d:1", max(1, parallel::detectCores() - 4))
annee_debut <- 2018

# Biens types pour la couche « prix estimés » de la carte
BIEN_TYPE <- list(
  Appartement = list(surface = 60, pieces = 3, dependance = 0, terrain = 0,
                     libelle = "appartement de 60 m², 3 pièces"),
  Maison = list(surface = 100, pieces = 4, dependance = 0, terrain = 500,
                libelle = "maison de 100 m², 4 pièces, 500 m² de terrain"))

# --- Ajustement --------------------------------------------------------------------
ajuster <- function(dt, mesh, typ, reponse) {
  spde <- inla.spde2.pcmatern(mesh, alpha = 2, prior.range = c(3, 0.5), prior.sigma = c(1, 0.01))
  A <- inla.spde.make.A(mesh, loc = cbind(dt$x, dt$y))
  cov <- data.frame(intercept = 1, dep1 = dt$dep1, dep2 = dt$dep2, surf_g = dt$surf_g,
                    pieces = dt$pieces, t_sem = dt$t_sem, terr_g = dt$terr_g,
                    section = dt$sec_id, parcelle = dt$par_id)
  stk <- inla.stack(data = list(y = reponse), A = list(A, 1),
                    effects = list(list(s = seq_len(spde$n.spde)), cov), tag = "obs")
  pc_prec <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  formule <- as.formula(paste(
    "y ~ -1 + intercept + dep1 + dep2",
    "+ f(surf_g, model = 'rw2', scale.model = TRUE, hyper = pc_prec)",
    "+ f(pieces, model = 'rw1', scale.model = TRUE, hyper = pc_prec)",
    if (typ == "Maison") "+ f(terr_g, model = 'rw2', scale.model = TRUE, hyper = pc_prec)" else "",
    "+ f(section, model = 'iid', hyper = pc_prec)",
    "+ f(parcelle, model = 'iid', hyper = pc_prec)",
    # values : toutes les modalités et tous les semestres reçoivent un effet, même
    # sans vente observée (prévision de la tendance, sections du test seulement)
    "+ f(t_sem, model = 'rw1', scale.model = TRUE, hyper = pc_prec, values = 1:n_sem)",
    "+ f(s, model = spde)"))
  environment(formule) <- environment()
  n_sem <- max(dt$t_sem)
  t0 <- Sys.time()
  fit <- inla(formule, data = inla.stack.data(stk, spde = spde),
              control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
              control.inla = list(int.strategy = "eb", strategy = "gaussian"),
              num.threads = nthreads, verbose = FALSE)
  msg(sprintf("   INLA ajusté en %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ii <- inla.stack.index(stk, "obs")$data
  list(fit = fit, lp = fit$summary.linear.predictor[ii, c("mean", "sd")])
}

# --- Composantes (moyenne, écart-type) -----------------------------------------------
composantes <- function(fit, typ, niv_sec, niv_par) {
  sr <- fit$summary.random; fx <- fit$summary.fixed; hp <- fit$summary.hyperpar
  sd_hp <- function(motif) 1 / sqrt(hp[grep(motif, rownames(hp)), "mode"])
  eff <- function(nom) data.frame(x = sr[[nom]]$ID, m = sr[[nom]]$mean, s = sr[[nom]]$sd)
  list(
    b0 = fx["intercept", c("mean", "sd")], d1 = fx["dep1", c("mean", "sd")], d2 = fx["dep2", c("mean", "sd")],
    surf = eff("surf_g"), pieces = eff("pieces"), tend = eff("t_sem"),
    terr = if (typ == "Maison") eff("terr_g") else NULL,
    sec = data.frame(code = niv_sec[sr$section$ID], m = sr$section$mean, s = sr$section$sd),
    par = data.frame(code = niv_par[sr$parcelle$ID], m = sr$parcelle$mean, s = sr$parcelle$sd),
    champ = data.frame(m = sr$s$mean, s = sr$s$sd),
    sd_bruit = sd_hp("^Precision for the Gaussian"),
    sd_sec = sd_hp("^Precision for section"), sd_par = sd_hp("^Precision for parcelle"))
}

# Prédiction par addition des composantes : ce que fera le navigateur (app.js)
predire <- function(cp, nd, mesh, t_sem) {
  interp <- function(e, v) approx(e$x, e$m, v, rule = 2)$y
  interp_s <- function(e, v) approx(e$x, e$s, v, rule = 2)$y
  A <- fm_basis(mesh, loc = cbind(nd$x, nd$y))
  i_sec <- match(nd$section_cadastrale, cp$sec$code); i_par <- match(nd$id_parcelle, cp$par$code)
  ou0 <- function(v, defaut) ifelse(is.na(v), defaut, v)
  i_t <- match(t_sem, cp$tend$x)
  mu <- cp$b0[[1]] + cp$d1[[1]] * nd$dep1 + cp$d2[[1]] * nd$dep2 +
    interp(cp$surf, nd$log_surface) + cp$pieces$m[match(nd$pieces, cp$pieces$x)] +
    cp$tend$m[i_t] + as.vector(A %*% cp$champ$m) +
    ou0(cp$sec$m[i_sec], 0) + ou0(cp$par$m[i_par], 0)
  v <- cp$b0[[2]]^2 + (cp$d1[[2]] * nd$dep1)^2 + (cp$d2[[2]] * nd$dep2)^2 +
    interp_s(cp$surf, nd$log_surface)^2 + cp$pieces$s[match(nd$pieces, cp$pieces$x)]^2 +
    cp$tend$s[i_t]^2 + as.vector(A %*% cp$champ$s)^2 +
    ou0(cp$sec$s[i_sec], cp$sd_sec)^2 + ou0(cp$par$s[i_par], cp$sd_par)^2 + cp$sd_bruit^2
  if (!is.null(cp$terr)) {
    mu <- mu + interp(cp$terr, nd$log_terrain); v <- v + interp_s(cp$terr, nd$log_terrain)^2
  }
  data.frame(mu = mu, sd = sqrt(v))
}

# --- Boucle par type -------------------------------------------------------------------
com_geo <- st_read(sprintf("site/data/communes/%s.geojson", dep), quiet = TRUE)
com_ctr <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(com_geo))))
com_xy <- sf_project("EPSG:4326", "EPSG:2154", com_ctr) / 1000

sortie <- list(); effets_com <- list()
t_max <- max(d$t_sem)
libelle_sem <- function(t) sprintf("%d-S%d", annee_debut + (t - 1) %/% 2, (t - 1) %% 2 + 1)

for (typ in c("Appartement", "Maison")) {
  dt <- d[d$type_local == typ, ]
  msg(sprintf("== %s : %d ventes", typ, nrow(dt)))
  dt$surf_g <- inla.group(dt$log_surface, n = 30, method = "quantile")
  dt$terr_g <- if (typ == "Maison") inla.group(dt$log_terrain, n = 30, method = "cut") else 0
  dt$dep1 <- as.numeric(dt$dependance == "1"); dt$dep2 <- as.numeric(dt$dependance == "2+")
  niv_sec <- levels(factor(dt$section_cadastrale)); niv_par <- levels(factor(dt$id_parcelle))
  dt$sec_id <- match(dt$section_cadastrale, niv_sec); dt$par_id <- match(dt$id_parcelle, niv_par)

  loc <- unique(round(cbind(dt$x, dt$y), 1))
  mesh <- fm_mesh_2d_inla(boundary = list(fm_nonconvex_hull(loc, convex = 3),
                                          fm_nonconvex_hull(loc, convex = 25)),
                          max.edge = c(arete, 4 * arete), cutoff = ecart_min)
  msg(sprintf("   maillage : %d noeuds", mesh$n))

  # 1. Apprentissage seul : validation de la reconstruction + calibration de k
  tst <- dt$jeu == "test"
  a1 <- ajuster(dt, mesh, typ, ifelse(tst, NA, dt$log_prix_m2))
  cp1 <- composantes(a1$fit, typ, niv_sec, niv_par)
  p1 <- predire(cp1, dt[tst, ], mesh, dt$t_sem[tst])
  msg(sprintf("   reconstruction vs INLA (test) : écart max %.2e sur la moyenne ; sd approchée / sd INLA médian %.2f",
              max(abs(p1$mu - a1$lp$mean[tst])),
              median(p1$sd / sqrt(a1$lp$sd[tst]^2 + cp1$sd_bruit^2))))
  z <- abs(dt$log_prix_m2[tst] - p1$mu) / p1$sd
  k <- unname(quantile(z, 0.8) / qnorm(0.9))
  avant <- evaluer(dt$log_prix_m2[tst], p1$mu, p1$mu - qnorm(0.9) * p1$sd, p1$mu + qnorm(0.9) * p1$sd)
  apres <- evaluer(dt$log_prix_m2[tst], p1$mu, p1$mu - qnorm(0.9) * k * p1$sd, p1$mu + qnorm(0.9) * k * p1$sd)
  msg(sprintf("   facteur de calibration k = %.3f", k))
  print(fmt_metriques(rbind(cbind(intervalle = "brut", avant), cbind(intervalle = "calibré", apres))),
        row.names = FALSE)
  rm(a1); gc()

  # 2. Toutes les ventes : modèle publié
  a2 <- ajuster(dt, mesh, typ, dt$log_prix_m2)
  cp <- composantes(a2$fit, typ, niv_sec, niv_par)
  print(round(a2$fit$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")], 3))
  rm(a2); gc()

  # Couche « prix estimés » : bien type au dernier semestre, localisation moyenne
  # des ventes de la commune (champ + effet section), sinon centre de la commune
  bt <- BIEN_TYPE[[typ]]
  nd_type <- function(n) data.frame(dep1 = rep(as.numeric(bt$dependance == 1), n),
                                    dep2 = rep(as.numeric(bt$dependance >= 2), n),
                                    log_surface = log(bt$surface), pieces = bt$pieces,
                                    log_terrain = log1p(bt$terrain))
  base <- predire(cp, cbind(nd_type(1), x = mean(dt$x), y = mean(dt$y),
                            section_cadastrale = NA, id_parcelle = NA), mesh, t_max)
  base_mu <- base$mu - as.vector(fm_basis(mesh, loc = cbind(mean(dt$x), mean(dt$y))) %*% cp$champ$m)
  loc_vente <- as.vector(fm_basis(mesh, loc = cbind(dt$x, dt$y)) %*% cp$champ$m) +
    cp$sec$m[match(dt$section_cadastrale, cp$sec$code)]
  par_com <- tapply(loc_vente, dt$code_commune, mean)
  est_com <- setNames(rep(NA_real_, nrow(com_geo)), com_geo$code)
  dans_mesh <- fm_basis(mesh, loc = com_xy[, 1:2], full = TRUE)$ok
  est_com[dans_mesh] <- as.vector(fm_basis(mesh, loc = com_xy[dans_mesh, , drop = FALSE]) %*% cp$champ$m)
  i <- match(names(par_com), names(est_com)); est_com[i[!is.na(i)]] <- par_com[!is.na(i)]
  est_com <- round(exp(base_mu + est_com))
  msg(sprintf("   bien type (%s) : %s €/m² en moyenne départementale",
              bt$libelle, format(round(exp(base_mu + mean(loc_vente))), big.mark = " ")))

  # Maillage en WGS84 (interpolation barycentrique dans le navigateur)
  ll <- sf_project("EPSG:2154", "EPSG:4326", mesh$loc[, 1:2] * 1000)
  r4 <- function(v) round(v, 4)
  sortie[[substr(typ, 1, 1)]] <- list(
    libelle_bien_type = bt$libelle, bien_type = bt[c("surface", "pieces", "dependance", "terrain")],
    b0 = r4(unlist(cp$b0)), d1 = r4(unlist(cp$d1)), d2 = r4(unlist(cp$d2)),
    surf = lapply(cp$surf, r4), pieces = lapply(cp$pieces, r4),
    terrain = if (!is.null(cp$terr)) lapply(cp$terr, r4),
    tendance = list(sem = libelle_sem(cp$tend$x), m = r4(cp$tend$m), s = r4(cp$tend$s)),
    t_courant = which(cp$tend$x == t_max) - 1,
    sd_bruit = r4(cp$sd_bruit), sd_sec = r4(cp$sd_sec), sd_par = r4(cp$sd_par), k = r4(k),
    mesh = list(lon = round(ll[, 1], 5), lat = round(ll[, 2], 5), tri = mesh$graph$tv - 1),
    champ = lapply(cp$champ, r4),
    communes = as.list(est_com[!is.na(est_com)]))

  # Effets section et immeuble, découpés par commune (5 premiers caractères)
  for (nm in c("sec", "par")) {
    e <- cp[[nm]]; com <- substr(e$code, 1, 5)
    for (cc in unique(com)) {
      j <- com == cc
      effets_com[[cc]][[nm]][[substr(typ, 1, 1)]] <-
        setNames(Map(function(a, b) c(round(a, 3), round(b, 3)), e$m[j], e$s[j]), e$code[j])
    }
  }
}

# --- Écriture ---------------------------------------------------------------------------
fc <- function(membre) list(type = "FeatureCollection", features = list(), modele = membre)
ecrire <- function(x, f) write_json(x, f, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
dir_mod <- file.path("site/data/modele", dep)
dir.create(dir_mod, recursive = TRUE, showWarnings = FALSE)
ecrire(fc(c(list(dep = dep, fin = format(max(d$date_mutation)), semestre = libelle_sem(t_max),
                 configuration = sprintf("INLA a%g_e%g statique + section + parcelle, gaussien", arete, ecart_min)),
            sortie)), sprintf("site/data/modele/%s.geojson", dep))
for (cc in names(effets_com)) ecrire(fc(effets_com[[cc]]), file.path(dir_mod, paste0(cc, ".geojson")))
tailles <- file.size(c(sprintf("site/data/modele/%s.geojson", dep),
                       list.files(dir_mod, full.names = TRUE)))
msg(sprintf("Écrit : %d fichiers, %.1f Mo", length(tailles), sum(tailles) / 1e6))
sink()
