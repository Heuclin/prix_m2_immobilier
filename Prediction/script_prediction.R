




rm(list=ls())

library(tidyverse)
require(data.table)
library(RSQLite)
library(dplyr)
library(dbplyr)



# dvf = read.csv("data/DVF_source/data_dvf_sample_5M.csv")


db_all_data <- dbConnect(SQLite(), dbname = "data/DVF_source/db_dvf_full_data.sqlite")
set.seed(42)

dvf_db <- tbl(db_all_data, "dvf_for_pred") %>% 
  # filter(annee %in% c("20220", "2021", "2022")) %>% 
  filter(code_departement == "34") %>%
  filter(lon >2) %>% 
  filter(code_postal != "0") %>% 
  filter(nb_maison+nb_apt == 1) %>% 
  filter(nb_apt == 1, 
         nb_pieces_apt >=1,
         nb_pieces_apt <=7) %>% 
  as_tibble() 
# %>%
# sample_n(100000)

dim(dvf_db)
dbDisconnect(db_all_data)

dvf_db = dvf_db %>% mutate(prix_m2=valeur_fonciere/surface_apt) %>% 
  filter(prix_m2 <=10000)


summary(dvf_db$prix_m2)
hist(dvf_db$prix_m2)



# dvf_db = dvf_db %>% group_by(code_postal) %>%
#   mutate(n=n()) %>% filter(n>5) %>%
#   select(-n) %>% ungroup()
# dim(dvf_db)

# dvf_db = dvf_db %>% rename("pres" = "prés") %>%
#   rename("terrains_a_batir" = "terrains a bâtir")

str(dvf_db)


dim(dvf_db)
names(dvf_db)

# my_data = dvf_db %>% select(-c(mois, code_commune, code_departement, section_cadastrale))
variables = c("valeur_fonciere", "prix_m2", "annee", "lon", "lat", 
              "code_postal", "section_cadastrale",
              "nb_dependance", 
              "apt", "surface_apt", "nb_pieces_apt", 
              "maison", "surface_maison", "nb_pieces_maison")

my_data = dvf_db %>% select(all_of(variables)) #%>% 
# filter(maison+apt > 0)
dim(my_data)
names(my_data)

library(caret)
set.seed(1253)
trainIndex = createDataPartition(my_data$code_postal, p=0.8, list=F)
data_train = my_data[trainIndex, ]; dim(data_train)
data_test = my_data[-trainIndex, ]; dim(data_test)



# Random Forset -----------------------------------------------------------


library(ranger)

fit_rf = ranger(prix_m2 ~ annee+lon+lat+code_postal+section_cadastrale+
                  # nb_dependance+
                  # apt+
                  surface_apt+nb_pieces_apt,
                # maison+surface_maison+nb_pieces_maison, 
                data = data_train,
                num.trees = 500,
                mtry = 4)
fit_rf

data_test$prediction = predict(fit_rf, data_test)$predictions *data_test$surface_apt


plot(data_test$prediction, data_test$valeur_fonciere); abline(0, 1)
cor(data_test$valeur_fonciere, data_test$prediction)^2
caret::RMSE(data_test$prediction, data_test$valeur_fonciere)




# INLA --------------------------------------------------------------------

library(INLA)

# ggplot(my_data, aes(x=lon, y=lat))+geom_point()
# range(my_data$lon)

coords <- as.matrix(my_data[, c("lon","lat")])
head(coords)
summary(coords)

# construction of domain and mesh for plot
prdomain <- inla.nonconvex.hull(coords,
                                convex  = -0.03, concave  = -0.5, 
                                resolution=c(100,100))
plot(prdomain)
# points(coords)



# Construction du mesh
nc.mesh = inla.mesh.2d(boundary = prdomain, 
                       max.edge = c(0.06, 0.12),
                       cutoff = 0.03
)
plot(nc.mesh)
# points(coords)

# fem <- inla.mesh.fem(nc.mesh, order = 2)



spde5 <- inla.spde2.pcmatern(
  # Mesh and smoothness parameter
  mesh = nc.mesh, 
  alpha = 2, #alpha = 2 more smooth than alpha = 1
  # P(practic.range < 0.3) = 0.5
  prior.range = c(0.3, 0.5),
  # P(sigma > 1) = 0.01
  prior.sigma = c(10, 0.01),
  constr = TRUE) 

nb_annee = length(unique(my_data$annee)); nb_annee
iset <- inla.spde.make.index('i', n.spde = spde5$n.spde, n.group = nb_annee)
# ici n.spde est le nombre de noeuds et n.group le nombre d'année. Cette fonction 
# construit des indices
# The index set is made taking into account the number of mesh points in the SPDE 
# model and the number of groups

my_data$annee2 = as.numeric(my_data$annee) - min(as.numeric(my_data$annee))+1
A <- inla.spde.make.A(mesh = nc.mesh, loc = coords, group = my_data$annee2) 

dim(A)
nc.mesh$n*nb_annee
# table(colSums(A) > 0)

variables = c("valeur_fonciere_train", "annee", "lon", "lat", 
              "code_postal", "section_cadastrale",
              "nb_dependance", 
              "apt", "surface_apt", "nb_pieces_apt", 
              "maison", "surface_maison", "nb_pieces_maison")
my_data$valeur_fonciere_train = my_data$valeur_fonciere
my_data$valeur_fonciere_train[-trainIndex] = NA
my_data$prix_m2_train = my_data$prix_m2
my_data$prix_m2_train[-trainIndex] = NA

stk5 <- inla.stack(
  data = list(y = log(my_data$prix_m2), E = my_data$surface_apt),
  A = list(A, 1, 1, 1,1, 1,1, 1, 1, 1, 1, 1), 
  effects = list(i = iset,
                 intercept = rep(1, nrow(my_data)),
                 code_postal = factor(my_data$code_postal),
                 section_cadastrale = factor(my_data$section_cadastrale),
                 nb_dependance = my_data$nb_dependance,
                 apt = my_data$apt,
                 surface_apt = my_data$surface_apt,
                 nb_pieces_apt = factor(my_data$nb_pieces_apt),
                 maison = my_data$maison,
                 surface_maison = my_data$surface_maison,
                 nb_pieces_maison = factor(my_data$nb_pieces_maison),
                 annee2 = factor(my_data$annee2)),
  tag = 'est'
)



# To fit the model, the intercept in the formula must be removed and added as a
# covariate term in the list of effects, so that all the covariate terms in the
# formula can be included in a projector matrix. Then, the matrix of predictors
# is passed to the inla() function in its control.predictor argument


# Déclaration du prior pour le paramètre autorégressif associé au temps
h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
# PC prior on the autoreg. param.
prec.prior <- list(prior = 'pc.prec', param = c(1, 0.01))

# nb_dependance+
#   apt+surface_apt+nb_pieces_apt +
#   maison+surface_maison+nb_pieces_maison,

my_formula = y ~ 0 + 
  intercept + 
  # nb_dependance+
  # apt+
  surface_apt+
  # nb_pieces_apt +
  f(nb_pieces_apt, model = "rw1", constr = TRUE)+
  # maison+
  # surface_maison+nb_pieces_maison+
  # f(code_postal, model = "iid", constr = TRUE, 
  #   hyper=list(prec=list(initial=log(1/1000),fixed=TRUE)))+
  f(section_cadastrale, model = "iid", constr = TRUE, 
    hyper=list(prec=list(initial=log(1/1000),fixed=TRUE)))+
  f(i, model = spde5, group = i.group, 
    control.group = list(model = 'ar1', hyper = h.spec))


fit_inla <- inla(my_formula,
                 # family = "poisson",
                 # E = inla.stack.data(stk5)$E,
                 data = inla.stack.data(stk5), 
                 control.predictor = list(A = inla.stack.A(stk5), compute = TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
)

summary(fit_inla)

tmp = inla.stack.index(stk5, 'est')
names(tmp)
length(tmp$effects)
length(tmp$data)

idx <- inla.stack.index(stk5, 'est')$data
my_data$fitted_inla = exp(fit_inla$summary.fitted.values$mean[idx]) * my_data$surface_apt
data_test$prediction_inla = my_data$fitted_inla[-trainIndex]

dev.off()
plot(data_test$prediction_inla, data_test$valeur_fonciere); abline(0, 1)
cor(data_test$prediction_inla, data_test$valeur_fonciere)^2



tmp = my_data %>% group_by(nb_pieces_apt) %>% 
  summarise(surf_med = median(surface_apt)) %>% 
  mutate(nb_pieces_apt = as.character(nb_pieces_apt))

intercept = fit_inla$summary.fixed[1, "mean"]
beta_surface_apt = fit_inla$summary.fixed[2, "mean"]
blup_nb_pieces = fit_inla$summary.random$nb_pieces_apt
blup_nb_pieces = blup_nb_pieces %>% 
  rename(nb_pieces_apt = ID) %>% 
  left_join(tmp) %>% 
  mutate(effect_surface = beta_surface_apt*beta_surface_apt) %>% 
  mutate(effect_nb_pieces = exp(intercept + effect_surface+ mean))


ggplot(blup_nb_pieces, aes(x=nb_pieces_apt, y = effect_nb_pieces)) +
  geom_point()


# _________________________________________________________________________

# Effet non-linéaire de la surface et du nombre de pièces avec intéraction


ggplot(my_data, aes(x=nb_pieces_apt, y = surface_apt, color=prix_m2))+geom_point()

my_data = my_data %>% mutate(surface_apt_class = cut_width(surface_apt, 15),
                             surface_apt_class2 = as.numeric(surface_apt_class))

my_data$nb_pieces_apt = as.numeric(my_data$nb_pieces_apt)
# my_data$grid_piece_surf = (my_data$nb_pieces_apt - 1)*length(levels(my_data$surface_apt_class)) + 
my_data$surface_apt_class2
nrow = length(unique(my_data$surface_apt_class))
ncol = length(unique(my_data$nb_pieces_apt))
table_ref_grid = expand.grid(
  surface_apt_class = levels(my_data$surface_apt_class),
  nb_pieces_apt = 1:ncol)
table_ref_grid$grid_piece_surf = 1:(ncol*nrow)
table_ref_grid$surface_apt_class2 = as.numeric(table_ref_grid$surface_apt_class)

head(table_ref_grid)
my_data$grid_piece_surf = NULL
my_data = my_data %>% left_join(table_ref_grid)

tmp = my_data %>% group_by(nb_pieces_apt, surface_apt_class, grid_piece_surf) %>% 
  summarise(prix_m2_med = median(prix_m2), n=n())

ggplot(tmp, aes(x=factor(nb_pieces_apt), y = surface_apt_class, fill=prix_m2_med))+
  geom_tile()+
  scale_fill_gradient(low = "lightgreen", high = "red", name = "prix m2 median") +
  # geom_text(aes(label = grid_piece_surf), alpha=0.5)
  geom_text(aes(label = n), alpha=0.5)

stk5 <- inla.stack(
  data = list(y = log(my_data$prix_m2), E = my_data$surface_apt),
  A = list(A, 1, 1, 1,1, 1,1, 1, 1, 1, 1, 1, 1, 1), 
  effects = list(i = iset,
                 intercept = rep(1, nrow(my_data)),
                 grid_piece_surf = my_data$grid_piece_surf,
                 code_postal = factor(my_data$code_postal),
                 section_cadastrale = factor(my_data$section_cadastrale),
                 nb_dependance = my_data$nb_dependance,
                 apt = my_data$apt,
                 surface_apt = my_data$surface_apt,
                 surface_apt_class2= my_data$surface_apt_class2,
                 nb_pieces_apt = factor(my_data$nb_pieces_apt),
                 maison = my_data$maison,
                 surface_maison = my_data$surface_maison,
                 nb_pieces_maison = factor(my_data$nb_pieces_maison),
                 annee2 = factor(my_data$annee2)),
  tag = 'est'
)



my_formula = y ~ 0 + 
  intercept + 
  # nb_dependance+
  # apt+
  # f(grid_piece_surf,  model = "rw2d", nrow = nrow, ncol = ncol, constr = TRUE)+
  f(surface_apt_class2,  model = "rw1", constr = TRUE)+
  f(nb_pieces_apt, model = "iid", constr=TRUE,
    hyper=list(prec=list(initial=log(1/1000),fixed=TRUE)))+
  # f(code_postal, model = "iid", constr = TRUE, 
  #   hyper=list(prec=list(initial=log(1/1000),fixed=TRUE)))+
  f(section_cadastrale, model = "iid", constr = TRUE, 
    hyper=list(prec=list(initial=log(1/1000),fixed=TRUE)))+
  f(i, model = spde5, group = i.group, 
    control.group = list(model = 'ar1', hyper = h.spec))


fit_inla <- inla(my_formula,
                 # family = "poisson",
                 # E = inla.stack.data(stk5)$E,
                 data = inla.stack.data(stk5), 
                 control.predictor = list(A = inla.stack.A(stk5), compute = TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
)

summary(fit_inla)
fit_inla$dic$dic

idx <- inla.stack.index(stk5, 'est')$data
tmp_fitted = fit_inla$summary.fitted.values[idx, ] %>% 
  select(-c(sd, `0.5quant`))
tmp_fitted$mean = exp(tmp_fitted$mean) * my_data$surface_apt
tmp_fitted$`0.025quant` = exp(tmp_fitted$`0.025quant`) * my_data$surface_apt
tmp_fitted$`0.975quant` = exp(tmp_fitted$`0.975quant`) * my_data$surface_apt
tmp_fitted$mode = exp(tmp_fitted$mode) * my_data$surface_apt
tmp_fitted$true = my_data$valeur_fonciere
tmp_fitted$true_train = my_data$valeur_fonciere_train

my_data$fitted_inla = exp(fit_inla$summary.fitted.values$mean[idx]) * my_data$surface_apt
data_test$prediction_inla = my_data$fitted_inla[-trainIndex]

dev.off()
plot(data_test$prediction_inla, data_test$valeur_fonciere); abline(0, 1)
cor(data_test$prediction_inla, data_test$valeur_fonciere)^2
caret::RMSE(data_test$prediction_inla, data_test$valeur_fonciere)


intercept = fit_inla$summary.fixed[1, 1]
table_ref_grid$fitted = exp(intercept + 
                              fit_inla$summary.random$grid_piece_surf$mode)

ggplot(table_ref_grid, aes(x=factor(nb_pieces_apt), y = surface_apt_class, fill=fitted))+
  geom_tile()+
  scale_fill_gradient(low = "lightgreen", high = "red", name = "prix m2 median") +
  geom_text(aes(label = grid_piece_surf), alpha=0.5)
  # geom_text(aes(label = n), alpha=0.5)




