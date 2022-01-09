
# Benjamin Heuclin
# 17/12/2021
# Licence : CC BY-NC-ND 4.0



rm(list=ls())

library(tidyverse)
require(data.table)
library(RSQLite)
library(dplyr)




# Deja fait

# dvf_2016 <- fread("../data/DVF/data_immo_2016.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2016 <- dvf_2016 %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# dvf_2017 <- fread("../data/DVF/data_immo_2017.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2017 <- as_tibble(dvf_2017) %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# dvf_2018 <- fread("../data/DVF/data_immo_2018.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2018 <- as_tibble(dvf_2018) %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# dvf_2019 <- fread("../data/DVF/data_immo_2019.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2019 <- as_tibble(dvf_2019) %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# dvf_2020 <- fread("../data/DVF/data_immo_2020.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2020 <- as_tibble(dvf_2020) %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# dvf_2021 <- fread("../data/DVF/data_immo_2021.csv", sep=",", header=TRUE, encoding = "UTF-8")
# dvf_2021 <- as_tibble(dvf_2021) %>% as_tibble() %>% 
#   select(id_mutation, date_mutation, nature_mutation, valeur_fonciere, code_postal,
#          code_commune, code_departement, id_parcelle, type_local, surface_reelle_bati,
#          nombre_pieces_principales, nature_culture, surface_terrain, longitude, latitude) %>%
#   mutate(annee = substr(date_mutation, 1, 4)) %>% select(-date_mutation) %>%
#   mutate(section_cadastrale = substr(id_parcelle, 1, 10)) %>% select(-id_parcelle) %>%
#   filter(!is.na(valeur_fonciere))
# 
# 
# dvf <- bind_rows(dvf_2016, dvf_2017, dvf_2018, dvf_2019, dvf_2020, dvf_2021)
# 
# dvf%>% tally()
# 
# dvf <- dvf %>% mutate(type_local=replace(type_local, type_local =="", "Terre")) %>%
#   mutate(type_local=replace(type_local, type_local =="Local industriel. commercial ou assimilé", "Local"))
# 
# table(dvf$type_local)
# 
# 
# # elimination des mutations comportant des vente en l'etat futur d'achevement car pas assez d'infos sur ces ventes
# id_delete <- unique(dvf$id_mutation[dvf$nature_mutation  %in% c("Vente en l'état futur d'achèvement", "Adjudication", "Echange", "Expropriation")])
# 
# dvf <- dvf %>% filter(! id_mutation %in% id_delete)
# dvf%>% tally()
# 
# table(dvf$nature_mutation)
# 
# # length(unique(dvf$id_mutation))
# # 6416194 de mutation
# 
# 
# library(RSQLite)
# library(dplyr)
# 
# db <- dbConnect(SQLite(), dbname = "../data/DVF/dvf_2016_to_2021.sqlite")
# dbWriteTable(db, "dvf", dvf, overwrite =TRUE)
# 
# dbListTables(db)
# 
# dbDisconnect(db)








# Summary par département -------------------------------------------------


rm(list=ls())

library(tidyverse)
require(data.table)
library(RSQLite)
library(dplyr)

# library(multidplyr)
# cluster <- new_cluster(7)
# cluster


load("../data/tab_reg_dep_com.Rdata")

num_dep <- tab_dep$code_departement

icesTAF::mkdir(paste0("../data/DVF/summary_par_departement/", num_dep))


db_all_data <- dbConnect(SQLite(), dbname = "../data/DVF/dvf_2016_to_2021.sqlite")
data_all <- tbl(db_all_data, "dvf")


dep <- "D01"
for(dep in num_dep){
  dep2 <- substr(dep, 2, nchar(dep))
  print(dep2)


  # nettoyage du jeux de donnees
  data <- data_all %>% filter(code_departement == dep2) %>% as_tibble()

  # on creer une variable qui donne le nombre de dependance de la mutation
  # puis on supprime les lignes correspondants à des mutations

  # on creer ensuite des variables apt (appartement), maison, terre, local qui
  # valent TRUE si la mutation comporte des ventes d'appartement, maison, terres
  # ou local respectivement

  data <- data %>% group_by(id_mutation) %>%
    mutate(dependance = sum(type_local == "Dépendance")) %>%
    mutate(apt = any(type_local == "Appartement")) %>%
    mutate(maison = any(type_local == "Maison")) %>%
    mutate(terre = any(type_local == "Terre")) %>%
    mutate(local = any(type_local == "Local")) %>%
    ungroup() %>%
    filter(type_local != "Dépendance") %>%
    mutate(type_mut = paste0(1*apt, 1*maison, 1*terre, 1*local))

  # traittement des
  # appartement (1000) 27%
  # maison (0100) 19%
  # local (0001) 3%
  # maison+terre (0110) 12%

  #  total 27+19+3+12 = 61%

  data2 <- data %>% filter(type_mut %in% c("1000", "0100", "0001", "0110"))
  # data2 %>% tally

  data2 <- data2 %>% group_by(id_mutation, nature_mutation, valeur_fonciere,
                              code_commune, code_departement,
                              section_cadastrale, type_local, annee,
                              dependance, apt, maison, terre, local, type_mut) %>%
    summarise(surface_reelle_bati = sum(surface_reelle_bati),
              surface_terrain = sum(surface_terrain),
              nombre_pieces_principales = sum(nombre_pieces_principales),
              longitude = mean(longitude),
              latitude = mean(latitude)) %>%
    ungroup() %>%
    mutate(nature_culture = "Sols") %>%
    group_by(id_mutation, section_cadastrale) %>%
    mutate(surface_terrain = sum(surface_terrain)) %>%
    ungroup() %>%
    filter(type_local != "Terre")

  # data2 %>% tally

  # toto <- data2 %>% as_tibble()
  # table(toto$type_local)
  # table(data2$id_mutation)[table(data2$id_mutation)>1]
  # tmp <- names(table(data2$id_mutation)[table(data2$id_mutation)>1])
  #
  # View(data2[data2$id_mutation %in% tmp, ])

  data2 <- data2 %>% group_by(id_mutation) %>%
    filter(!is.na(surface_reelle_bati)) %>%
    mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
    mutate(VF_vente = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
    mutate(prix_m2 = VF_vente / surface_reelle_bati)

  
  # élimination des valeurs aberrantes
  colnames(data2)
  
  data_tmp <- list(Appartement = data2 %>% filter(type_local == "Appartement"),
                   Maison = data2 %>% filter(type_local == "Maison"),
                   Local= data2 %>% filter(type_local == "Local"))
  


  for(i in c("Appartement", "Maison", "Local")){
    # surface bati
    dat <- data_tmp[[i]]
    n <- nrow(dat)
    summary(dat$surface_reelle_bati)
    
    sum(dat$surface_reelle_bati < 10) / n
    sum(dat$surface_reelle_bati > 500) / n
    
    dat <- dat %>% filter(surface_reelle_bati > 10) %>%
      filter(surface_reelle_bati < 500)
    
    
    # prix de vente
    summary(dat$VF_vente)
    q1 <- quantile(dat$VF_vente, 0.01); q1
    q2 <- quantile(dat$VF_vente, 0.999); q2
    
    dat <- dat %>% filter(VF_vente>q1, VF_vente<q2)
    
    # prix du m2
    summary(dat$prix_m2)
    q1 <- quantile(dat$prix_m2, 0.01); q1
    q2 <- quantile(dat$prix_m2, 0.99); q2
    
    dat <- dat %>% filter(prix_m2>q1, prix_m2<q2)
    
    data_tmp[[i]] <- dat
  }
  
  data2 <- bind_rows(data_tmp[["Appartement"]], data_tmp[["Maison"]], data_tmp[["Local"]])
  

  
  


data_sum_dep_annee <- data2 %>%  group_by(code_departement, type_local, annee) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())

data_sum_dep <- data2 %>%  group_by(code_departement, type_local) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())

data_sum_com_annee <- data2 %>%  group_by(code_departement, code_commune, type_local, annee) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())

data_sum_com <- data2 %>%  group_by(code_departement, code_commune, type_local) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())

data_sum_cad_annee <- data2 %>%  group_by(code_departement, code_commune, section_cadastrale, type_local, annee) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())

data_sum_cad <- data2 %>%  group_by(code_departement, code_commune, section_cadastrale, type_local) %>%
  summarise(C5 = round(quantile(prix_m2, 0.05, na.rm=TRUE)),
            Q1= round(quantile(prix_m2, 0.25, na.rm=TRUE)),
            med= round(median(prix_m2, na.rm=TRUE)),
            Q3= round(quantile(prix_m2, 0.75, na.rm=TRUE)),
            C95 = round(quantile(prix_m2, 0.95, na.rm=TRUE)),
            mean = round(mean(prix_m2)),
            n=n())



  db_dep <- dbConnect(SQLite(), dbname = paste0("../data/DVF/summary_par_departement/", dep, "/summary_dep_", dep, ".sqlite"))
  dbWriteTable(db_dep, "dvf_sum_dep_annee", data_sum_dep_annee, overwrite =TRUE)
  dbWriteTable(db_dep, "dvf_sum_dep"      , data_sum_dep      , overwrite =TRUE)
  dbWriteTable(db_dep, "dvf_sum_com_annee", data_sum_com_annee, overwrite =TRUE)
  dbWriteTable(db_dep, "dvf_sum_com"      , data_sum_com      , overwrite =TRUE)
  dbWriteTable(db_dep, "dvf_sum_cad_annee", data_sum_cad_annee, overwrite =TRUE)
  dbWriteTable(db_dep, "dvf_sum_cad"      , data_sum_cad      , overwrite =TRUE)
  dbDisconnect(db_dep)

  save(data_sum_dep_annee, file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_dep_annee.Rdata"))
  save(data_sum_dep,       file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_dep.Rdata"))
  save(data_sum_com_annee, file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_com_annee.Rdata"))
  save(data_sum_com,       file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_com.Rdata"))
  save(data_sum_cad_annee, file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_cad_annee.Rdata"))
  save(data_sum_cad,       file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_cad.Rdata"))

  rm(data, data2, data_sum_dep_annee, data_sum_dep, data_sum_com_annee, data_sum_com, data_sum_cad_annee, data_sum_cad)
}

dbDisconnect(db_all_data)






# prix m2 par departement pour la france entière --------------------------



load("../data/tab_reg_dep_com.Rdata")

num_dep <- tab_dep$code_departement


num_dep
dep="D01"
data_sum_dep_all <- tibble()

for(dep in num_dep){
  load(file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_dep.Rdata"))
  data_sum_dep
  
  data_sum_dep_all <- bind_rows(data_sum_dep_all, data_sum_dep)
  rm(data_sum_dep)
}

data_sum_dep_all <- data_sum_dep_all %>% mutate(code_departement = paste0("D", code_departement ))
View(data_sum_dep_all)


save(data_sum_dep_all, file="../data/DVF/summary_par_departement/data_sum_dep_all.Rdata")






data_sum_dep_annee_all <- tibble()

for(dep in num_dep){
  load(file = paste0("../data/DVF/summary_par_departement/", dep, "/dvf_sum_dep_annee.Rdata"))
  data_sum_dep_annee
  
  data_sum_dep_annee_all <- bind_rows(data_sum_dep_annee_all, data_sum_dep_annee)
  rm(data_sum_dep_annee)
}

data_sum_dep_annee_all <- data_sum_dep_annee_all %>% mutate(code_departement = paste0("D", code_departement ))
View(data_sum_dep_annee_all)


save(data_sum_dep_annee_all, file="../data/DVF/summary_par_departement/data_sum_dep_annee_all.Rdata")

data_sum_dep_2021_all <- data_sum_dep_annee_all %>% filter(annee == "2021")
save(data_sum_dep_2021_all, file="../data/DVF/summary_par_departement/data_sum_dep_2021_all.Rdata")


################################################################################

# old








# les mutations comportant uniquement des terres
# type_mut = 0010
data_t <- data3 %>% filter(terre & !apt & !maison & !local)
nrow(data_t)

# les mutations comportant uniquement des appartements
# type_mut = 1000
data_apt <- data3 %>% filter(!terre & apt & !maison & !local)
nrow(data_apt)

# les mutations comportant uniquement des maisons
# type_mut = 0100
data_mais <- data3 %>% filter(!terre & !apt & maison & !local)
nrow(data_mais)

# les mutations comportant uniquement des locals
# type_mut = 0001
data_local <- data3 %>% filter(!terre & !apt & !maison & local)
nrow(data_local)

# les mutations comportant uniquement des appartements et des locals
# type_mut = 1001
data_apt_local <- data3 %>% filter(!terre & apt & !maison & local)
nrow(data_apt_local)

data_mais_local <- data3 %>% filter(!terre & !apt & maison & local)
nrow(data_mais_local)

data_terre_local <- data3 %>% filter(terre & !apt & !maison & local)
nrow(data_terre_local)

data_terre_apt <- data3 %>% filter(terre & apt & !maison & !local)
nrow(data_terre_apt)

data_terre_mais <- data3 %>% filter(terre & !apt & maison & !local)
nrow(data_terre_mais)

data_mais_apt <- data3 %>% filter(!terre & apt & maison & !local)
nrow(data_mais_apt)

data_mais_apt_local <- data3 %>% filter(!terre & apt & maison & local)
nrow(data_mais_apt_local)

data_mais_apt_terre <- data3 %>% filter(terre & apt & maison & !local)
nrow(data_mais_apt_terre)

data_apt_terre_local <- data3 %>% filter(terre & apt & !maison & local)
nrow(data_apt_terre_local)

data_apt_terre_local_mais <- data3 %>% filter(terre & apt & maison & local)
nrow(data_apt_terre_local_mais)


nrow(data_t) + nrow(data_apt) + nrow(data_mais) + nrow(data_local) + 
  nrow(data_terre_mais) + nrow(data_apt_local) + nrow(data_terre_apt) +
  nrow(data_mais_local) + nrow(data_terre_local) + 
  nrow(data_mais_apt) + nrow(data_mais_apt_local) + 
  nrow(data_mais_apt_terre) + nrow(data_apt_terre_local) + 
  nrow(data_apt_terre_local_mais)

nrow(data3)

nrow(data_t) + nrow(data_apt) + nrow(data_mais) + nrow(data_local) + 
  nrow(data_terre_mais) + nrow(data_apt_local) + nrow(data_terre_apt) 
# 95%





# Terre

data_terre <- data3 %>% filter(terre & !apt & !maison & !local)
table(data_terre$id_mutation)[table(data_terre$id_mutation)>1]

# View(data_terre %>% filter(id_mutation %in% c("2016-230850", "2016-230865", "2016-230872")))
dim(data_terre)
data_terre <- data_terre %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
                                      code_postal, code_commune, code_departement, 
                                      section_cad, type_local, surface_reelle_bati, 
                                      nombre_pieces_principales, nature_culture, annee, 
                                      dependance, apt, maison, terre, local) %>%
  summarise(surface_terrain = sum(surface_terrain)) %>%
  ungroup()

data_terre <- data_terre %>% group_by(id_mutation) %>% mutate(surface_terrain_tot = sum(surface_terrain))

nrow(data_terre)

table(data_terre$nature_culture)



table(data_terre$id_mutation)[table(data_terre$id_mutation)>1]
tmp <- names(table(data_terre$id_mutation)[table(data_terre$id_mutation)>1])

View(data_terre[data_terre$id_mutation %in% tmp, ])










# Local
data_local <- data3 %>% filter(!terre & !apt & !maison & local)
nrow(data_local)

data_local <- data_local %>% filter(!is.na(surface_reelle_bati))

data_local <- data_local %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
                                      code_commune, code_departement, 
                                      section_cad, type_local, annee, 
                                      dependance, apt, maison, terre, local) %>%
  summarise(surface_reelle_bati = sum(surface_reelle_bati),
            surface_terrain = sum(surface_terrain),
            nombre_pieces_principales = sum(nombre_pieces_principales)) %>%
  ungroup() %>% mutate(nature_culture = "sols")

nrow(data_local)

table(data_local$id_mutation)[table(data_local$id_mutation)>1]
tmp <- names(table(data_local$id_mutation)[table(data_local$id_mutation)>1])

View(data_local[data_local$id_mutation %in% tmp, ])


data_local <- data_local %>% group_by(id_mutation) %>% 
  mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
  mutate(VF_mut = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
  mutate(prix_m2 = VF_mut / surface_reelle_bati)

View(data_local[data_local$id_mutation %in% tmp, ])


toto <- data_local %>% filter(surface_reelle_bati<400)
plot(VF_mut~surface_reelle_bati, data=toto)




# Appartement

data_apt <- data3 %>% filter(!terre & apt & !maison & !local)
nrow(data_apt)

data_apt <- data_apt %>% filter(!is.na(surface_reelle_bati))
nrow(data_apt)

length(unique(data_apt$id_mutation))

data_apt <- data_apt %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
                                  code_commune, code_departement, 
                                  section_cad, type_local, annee, 
                                  dependance, apt, maison, terre, local) %>%
  summarise(surface_reelle_bati = sum(surface_reelle_bati),
            surface_terrain = sum(surface_terrain),
            nombre_pieces_principales = sum(nombre_pieces_principales)) %>%
  ungroup() %>% mutate(nature_culture = "sols")

nrow(data_apt)



table(data_apt$id_mutation)[table(data_apt$id_mutation)>1]
tmp <- names(table(data_apt$id_mutation)[table(data_apt$id_mutation)>1])

View(data_apt[data_apt$id_mutation %in% tmp, ])


data_apt <- data_apt %>% group_by(id_mutation) %>% 
  mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
  mutate(VF_mut = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
  mutate(prix_m2 = VF_mut / surface_reelle_bati)

View(data_apt[data_apt$id_mutation %in% tmp, ])

toto <- data_apt %>% filter(surface_reelle_bati<400)
plot(VF_mut~surface_reelle_bati, data=toto)





# Maison

data_mais <- data3 %>% filter(!terre & !apt & maison & !local)
nrow(data_mais)

data_mais <- data_mais %>% filter(!is.na(surface_reelle_bati))
nrow(data_mais)

length(unique(data_mais$id_mutation))

data_mais <- data_mais %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
                                    code_commune, code_departement, 
                                    section_cad, type_local, annee, 
                                    dependance, apt, maison, terre, local) %>%
  summarise(surface_reelle_bati = sum(surface_reelle_bati),
            surface_terrain = sum(surface_terrain),
            nombre_pieces_principales = sum(nombre_pieces_principales)) %>%
  ungroup() %>% mutate(nature_culture = "sols")

nrow(data_mais)



table(data_mais$id_mutation)[table(data_mais$id_mutation)>1]
tmp <- names(table(data_mais$id_mutation)[table(data_mais$id_mutation)>1])

View(data_mais[data_mais$id_mutation %in% tmp, ])


data_mais <- data_mais %>% group_by(id_mutation) %>% 
  mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
  mutate(VF_mut = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
  mutate(prix_m2 = VF_mut / surface_reelle_bati)

View(data_mais[data_mais$id_mutation %in% tmp, ])

toto <- data_mais %>% filter(surface_reelle_bati<400)
plot(VF_mut~surface_reelle_bati, data=toto)











# Appartement + local

data_apt_loc <- data3 %>% filter(!terre & apt & !maison & local)
nrow(data_apt_loc)

data_apt_loc <- data_apt_loc %>% filter(!is.na(valeur_fonciere))

data_apt_loc <- data_apt_loc %>% filter(!is.na(surface_reelle_bati))
nrow(data_apt_loc)

length(unique(data_apt_loc$id_mutation))

table(data_apt_loc$type_local)

data_apt_loc <- data_apt_loc %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
                                          code_commune, code_departement, 
                                          section_cad, type_local, annee, 
                                          dependance, apt, maison, terre, local) %>%
  summarise(surface_reelle_bati = sum(surface_reelle_bati),
            surface_terrain = sum(surface_terrain),
            nombre_pieces_principales = sum(nombre_pieces_principales)) %>%
  ungroup() %>% mutate(nature_culture = "sols")

nrow(data_apt_loc)



table(data_apt_loc$id_mutation)[table(data_apt_loc$id_mutation)>1]
tmp <- names(table(data_apt_loc$id_mutation)[table(data_apt_loc$id_mutation)>1])

# View(data_apt_loc[data_apt_loc$id_mutation %in% tmp, ])


data_apt_loc <- data_apt_loc %>% group_by(id_mutation) %>% 
  mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
  mutate(VF_mut = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
  mutate(prix_m2 = VF_mut / surface_reelle_bati)

View(data_apt_loc[data_apt_loc$id_mutation %in% tmp, ])

toto <- data_apt_loc %>% filter(surface_reelle_bati<400)
plot(VF_mut~surface_reelle_bati, data=toto)









# Maison + terre ou Appartement + Terre

data_mais_terre <- data3 %>% filter(terre & !apt & maison & !local)
data_apt_terre <- data3 %>% filter(terre & apt & !maison & !local)

data_mais_apt_terre <- bind_rows(data_mais_terre, data_apt_terre)

nrow(data_mais_apt_terre)


data_mais_apt_terre <- data_mais_apt_terre %>% filter(!is.na(valeur_fonciere))
nrow(data_mais_apt_terre)

length(unique(data_mais_apt_terre$id_mutation))



table(data_mais_apt_terre$type_local)

data_mais_apt_terre <- data_mais_apt_terre %>% 
  group_by(id_mutation, nature_mutation, valeur_fonciere, 
           code_commune, code_departement, 
           section_cad, type_local, annee, 
           dependance, apt, maison, terre, local) %>%
  summarise(surface_reelle_bati = sum(surface_reelle_bati),
            surface_terrain = sum(surface_terrain),
            nombre_pieces_principales = sum(nombre_pieces_principales)) %>%
  ungroup() %>% 
  mutate(nature_culture = "sols") %>%
  group_by(id_mutation, section_cad) %>%
  mutate(surface_terrain = sum(surface_terrain)) %>%
  ungroup() 

data_mais_apt_terre <- data_mais_apt_terre %>% filter(type_local %in% c("Appartement", "Maison"))
nrow(data_mais_apt_terre)



table(data_mais_apt_terre$id_mutation)[table(data_mais_apt_terre$id_mutation)>1]
tmp <- names(table(data_mais_apt_terre$id_mutation)[table(data_mais_apt_terre$id_mutation)>1])

View(data_mais_apt_terre[data_mais_apt_terre$id_mutation %in% tmp, ])


data_mais_apt_terre <- data_mais_apt_terre %>% group_by(id_mutation) %>% 
  mutate(surface_bati_tot = sum(surface_reelle_bati)) %>%
  mutate(VF_mut = valeur_fonciere/surface_bati_tot * surface_reelle_bati) %>%
  mutate(prix_m2 = VF_mut / surface_reelle_bati)

View(data_mais_apt_terre[data_mais_apt_terre$id_mutation %in% tmp, ])

toto <- data_mais_apt_terre %>% filter(surface_reelle_bati<400)
plot(VF_mut~surface_reelle_bati, data=toto)


# View(data_mais_apt_terre[data_mais_apt_terre$id_mutation %in% c("2017-459424", "2017-459452"), ])





# 
# # n?toyage du jeux terrain ? b?tir ----------------------------------------
# 
# rm(list=ls())
# 
# library(tidyverse)
# require(data.table)
# library(RSQLite)
# library(dplyr)
# 
# 
# 
# my_db <- src_sqlite("../data/DVF/dvf_2016_to_2021.sqlite", create = FALSE)
# data_tab <- tbl(my_db, "dvf_tab")
# 
# data_tab %>% tally
# 
# data <- data_tab %>% as_tibble()
# 
# colnames(data2)
# 
# table(data$id_mutation)[table(data$id_mutation)>1]
# 
# data2 <- data %>% group_by(id_mutation, nature_mutation, valeur_fonciere, 
#                            code_postal, code_commune, code_departement, 
#                            section_cad) %>%
#   summarise(surface_terrain = sum(surface_terrain))
# 
# nrow(data)
# nrow(data2)
# 
# table(data2$id_mutation)[table(data2$id_mutation)>1]
# 
# 
# toto <- data2 %>% filter(id_mutation %in%  c("2021-76334", "2021-87221"))
# View(toto)
# 
# data_final <- data2 %>% group_by(id_mutation) %>% 
#   mutate(surface_total_mut = sum(surface_terrain)) %>% ungroup() %>%
#   mutate(VF_mut = valeur_fonciere/surface_total_mut*surface_terrain) 
# 
# toto <- data_final %>% filter(id_mutation %in%  c("2021-76334", "2021-87221"))
# View(toto)
# 
# 
# dim(data)
# dim(data2)
# dim(data_final)
# 
# 
# db <- dbConnect(SQLite(), dbname = "../data/DVF/dvf_2016_to_2021.sqlite")
# dbWriteTable(db, "dvf_tab", data_final, overwrite = TRUE)
# dbDisconnect(db)
# 
# 






