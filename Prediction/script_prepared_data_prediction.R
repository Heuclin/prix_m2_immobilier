rm(list=ls())

library(tidyverse)
require(data.table)
library(RSQLite)
library(dplyr)
library(dbplyr)

# proportion = 0.05


# library(multidplyr)
# cluster <- new_cluster(7)
# cluster


# load("data/tab_reg_dep_com.Rdata")



# Lecture des données brutes
db_all_data <- dbConnect(SQLite(), dbname = "data/DVF_source/db_dvf_full_data.sqlite")
dvf_db <- tbl(db_all_data, "dvf") 

# dbDisconnect(db_all_data)



# dvf_db <- tbl(db_all_data, "dvf") %>% as_tibble() %>% sample_frac(size = proportion) 
# dvf = dvf_db %>% sample_n(1000000)
# dim(dvf_db)
# names(dvf_db)
# 


toto = dvf_db %>% 
  # filter(nature_culture != "") %>%
  group_by(nature_culture) %>% summarise(n=n()) %>%
  ungroup() %>%
  collect()
toto = toto %>% mutate(freq=round(n/sum(n)*100, 1)) %>% arrange(freq) 
toto = toto %>% filter(freq >= 3.9)
toto %>% arrange(freq)

dvf_db = dvf_db %>% 
  filter(nature_culture %in% local(toto$nature_culture))




toto = dvf_db %>% 
  # filter(nature_culture != "") %>%
  group_by(code_departement) %>% summarise(n=n()) %>%
  ungroup() %>% #arrange(n) %>%
  collect()
toto %>% print(n=100)

dvf_db = dvf_db %>% 
  filter(!code_departement %in% c("971", "972", "973", "974", "2A", "2B"))


# on supprime les ventes dont la valeur foncière est inférieure à 1000 euros
dvf_db = dvf_db %>% filter(valeur_fonciere > 1000)




my_grid = dvf_db %>% group_by(annee, mois) %>% summarise(n=n()) %>% collect()
my_grid



dbListTables(db_all_data)
dbRemoveTable(db_all_data, "dvf_for_pred")
dbListTables(db_all_data)

id=1
for(id in 1:nrow(my_grid)){
  print(paste0("Année : ", my_grid$annee[id], " - Mois : ", my_grid$mois[id]))
  
  # on récupère les données de la base
  dvf = dvf_db %>% 
    filter(annee == !!my_grid$annee[id], mois == !!my_grid$mois[id]) %>%
    collect()
  dim(dvf)
  
  
  
  
  # Elimination des mutations avec faible surface de type de local nor terrain
  # toto = dvf %>% group_by(id_mutation) %>% 
  #   summarise(surface_bati = sum(surface_reelle_bati, na.rm = TRUE),
  #             surface_terre = sum(surface_terrain, na.rm=TRUE))
  # 
  # toto2 = toto %>% filter(surface_terre == 0, surface_bati<9)
  # dim(toto2)
  # 
  # toto2 = toto %>% filter(surface_terre <20, surface_bati==0)
  # dim(toto2)
  
  # on supprime les mutation à cheval sur plusieurs communes
  toto = dvf %>% 
    group_by(id_mutation, code_commune) %>% unique() %>%
    group_by(id_mutation) %>% mutate(n=n()) %>% filter(n>1)
  
  id_mut_supp = unique(toto$id_mutation)
  dvf = dvf %>% filter(! id_mutation %in% id_mut_supp,
                       valeur_fonciere >1)
  # ok
  
  
  
  # on récupère les variables : annee, mois, valeur_fonciere, code_postal, code_commune,
  # code_departement, lon, lat, et section cadastrale
  dvf_1 = dvf %>% 
    group_by(id_mutation, annee, mois, valeur_fonciere, code_commune, 
             code_departement) %>%
    summarise(code_postal = min(code_postal), 
              lon = mean(longitude, na.rm=TRUE), lat=mean(latitude, na.rm=TRUE),
              section_cadastrale = min(section_cadastrale))
  
  length(unique(dvf$id_mutation))        
  toto = dvf_1 %>% 
    group_by(id_mutation) %>% mutate(n=n()) %>% filter(n>1)         
  nrow(toto)
  # ok ! on a bien une ligne par mutation
  
  
  
  
  # information sur les type de locaux (présence, nombre, surface et nb pièces)
  dvf_type_local = dvf %>% group_by(id_mutation) %>%
    summarise(dependance = any(type_local == "Dépendance"),
              nb_dependance = sum(type_local == "Dépendance"),
              # 
              apt = any(type_local == "Appartement"),
              nb_apt = sum(type_local == "Appartement"),
              surface_apt = sum(surface_reelle_bati[type_local == "Appartement"], na.rm=TRUE),
              nb_pieces_apt = sum(nombre_pieces_principales[type_local == "Appartement"], na.rm=TRUE),
              # 
              maison = any(type_local == "Maison"),
              nb_maison = sum(type_local == "Maison"),
              surface_maison = sum(surface_reelle_bati[type_local == "Maison"], na.rm=TRUE),
              nb_pieces_maison = sum(nombre_pieces_principales[type_local == "Maison"], na.rm=TRUE),
              # 
              local = any(type_local == "Local"),
              nb_local = sum(type_local == "Local"),
              surface_local = sum(surface_reelle_bati[type_local == "Local"], na.rm=TRUE),
              # nb_pieces_local = sum(nombre_pieces_principales[type_local == "Local"], na.rm=TRUE)
    ) 
  
  toto = dvf_type_local %>% 
    group_by(id_mutation) %>% mutate(n=n()) %>% filter(n>1)         
  nrow(toto)
  # ok ! on a bien une ligne par mutation
  
  
  table(dvf$nature_culture)
  
  # information sur les terres
  dvf_terre = dvf %>% 
    select(id_mutation, valeur_fonciere, nature_culture, surface_terrain) %>%
    filter(nature_culture != "") %>%
    group_by(id_mutation) %>%
    pivot_wider(names_from = nature_culture, 
                values_from = surface_terrain,
                values_fill = 0,
                values_fn=sum
    )
  
  toto = dvf_terre %>% 
    group_by(id_mutation) %>% mutate(n=n()) %>% filter(n>1)         
  nrow(toto)
  # ok ! on a bien une ligne par mutation
  
  
  prepared_dvf = dvf_1 %>% 
    full_join(dvf_type_local) %>% 
    full_join(dvf_terre) 
  
  prepared_dvf[is.na(prepared_dvf)] = 0 
  
  
  toto = prepared_dvf %>% 
    group_by(id_mutation) %>% mutate(n=n()) %>% filter(n>1)         
  nrow(toto)
  # ok ! on a bien une ligne par mutation
  
  
  
  
  
  
  

  
  # ajout taux intérêt
  data_taux = read.csv("data/taux.csv", sep=",", dec=".")
  names(data_taux)
  
  prepared_dvf$trimestre = 1
  prepared_dvf$trimestre[prepared_dvf$mois >=4 & prepared_dvf$mois <=6] = 2
  prepared_dvf$trimestre[prepared_dvf$mois >=7 & prepared_dvf$mois <=9] = 3
  prepared_dvf$trimestre[prepared_dvf$mois >=10 & prepared_dvf$mois <=12] = 4
  table(prepared_dvf$trimestre)
  
  prepared_dvf = prepared_dvf %>% left_join(data_taux)
  
  
  # on supprime "id_mutation"
  prepared_dvf = prepared_dvf %>% ungroup %>% select(-id_mutation) 
  
  # on converti les facteurs
  prepared_dvf$annee = as.character(prepared_dvf$annee)
  # prepared_dvf$mois = factor(prepared_dvf$mois)
  prepared_dvf$code_postal = as.character(prepared_dvf$code_postal)
  prepared_dvf$code_commune = as.character(prepared_dvf$code_commune)
  prepared_dvf$code_departement = as.character(prepared_dvf$code_departement)
  prepared_dvf$section_cadastrale = as.character(prepared_dvf$section_cadastrale)
  prepared_dvf$trimestre = as.character(prepared_dvf$trimestre)
  prepared_dvf$mois = as.character(prepared_dvf$mois)
  
  
  
  
  # on sauvegarde toutes les données dans une base
  # db_all_data <- dbConnect(SQLite(), dbname = "data/DVF_source/db_dvf_full_data.sqlite")
  if(!dbExistsTable(db_all_data, "dvf_for_pred")){
    dbWriteTable(db_all_data, "dvf_for_pred", prepared_dvf, overwrite =FALSE)
  }else{
    dbAppendTable(db_all_data, "dvf_for_pred", prepared_dvf)
  }
}


dbListTables(db_all_data)

dbDisconnect(db_all_data)










# création csv ------------------------------------------------------------





db_all_data <- dbConnect(SQLite(), dbname = "data/DVF_source/db_dvf_full_data.sqlite")
set.seed(42)
dvf_db <- tbl(db_all_data, "dvf_for_pred") %>% 
  as_tibble() %>% filter(annee %in% c("20220", "2021", "2022")) %>%
  sample_n(1000000)

dbDisconnect(db_all_data)

dvf_db = dvf_db %>% group_by(code_postal) %>%
  mutate(n=n()) %>% filter(n>5) %>%
  select(-n) %>% ungroup()

dim(dvf_db)
# dvf_db[is.na(dvf_db)] = 0 





dim(dvf_db)
View(head(dvf_db, 100))


write.csv(dvf_db, file="data/DVF_source/data_dvf_sample_1M.csv",
          row.names = FALSE)









