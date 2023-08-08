




rm(list=ls())

library(tidyverse)
require(data.table)
library(RSQLite)
library(dplyr)
library(dbplyr)



dvf = read.csv("data/DVF_source/data_dvf_sample_5M.csv")

View(head(dvf))

dvf2 = dvf %>% sample_n(10000)

names(dvf)

fit_lm = lm(valeur_fonciere ~ 
              # code_commune + 
              code_departement +
              # lon + lat + lon*lat +
              nb_dependance + nb_apt + surface_apt + nb_pieces_apt+
              nb_maison + surface_maison + nb_pieces_maison + 
              nb_local + surface_local + 
              sols + terrains.a.bâtir +
              taux
              ,data = dvf2)

summary(fit_lm)


















