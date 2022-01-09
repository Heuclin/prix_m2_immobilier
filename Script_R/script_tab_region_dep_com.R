

# Benjamin Heuclin
# 17/12/2021
# Licence : CC BY-NC-ND 4.0





get_centroids <- function(sf_object){
  tmp <- do.call(rbind, sf_object %>% st_centroid() %>% st_geometry())
  colnames(tmp) <- c("lon","lat")
  return(data.frame(tmp))
}


# Région ------------------------------------------------------------------



region <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-avec-outre-mer.geojson")
region$code <- paste0("R", region$code)


tab_region <- cbind(data.frame(code_region = region$code,
                               nom_region = region$nom),
                    get_centroids(region))





cbind(tab_region$nom_region, 
list.files("data/france-geojson/regions"))

order <- c(10, 4, 2, 14, 9, 6, 17, 3, 15, 16, 1, 7, 12, 18, 5, 8, 11, 13)

tab_region$nom_region_2 <- list.files("data/france-geojson/regions")[order]


head(tab_region)

rownames(tab_region) <- tab_region$code_region



# département  ------------------------------------------------------------

# https://www.data.gouv.fr/en/datasets/departements-de-france/
tab_dep <- read.csv("../data/departements-france.csv", sep=",", header=TRUE,
                    encoding = "UTF-8")
tab_dep$code_region <- as.character(tab_dep$code_region)
id_tmp <- which(nchar(tab_dep$code_region)==1)
tab_dep$code_region[id_tmp] <- paste0("0", tab_dep$code_region[id_tmp])

tab_dep$nom_region %in% tab_region$nom_region
# ok

tab_dep$code_departement <- paste0("D", tab_dep$code_departement)
tab_dep$code_region <- paste0("R", tab_dep$code_region)
rownames(tab_dep) <- tab_dep$code_departement

head(tab_dep)

tab_dep$nom_departement_2 <- list.files("../data/france-geojson/departements")


# obtention des centres gps de chaque département
departement <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson")
departement$code <- paste0("D", departement$code)
rownames(departement) <- departement$code

departement$nom %in% tab_dep$nom_departement
# ok

cbind(departement$nom, tab_dep$nom_departement)
#  ! Attention, c'est pas le méme ordre

tab_dep <- cbind(tab_dep, get_centroids(departement[tab_dep$code_departement, ]))

head(tab_dep)





# Commune -----------------------------------------------------------------


commune <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes-avec-outre-mer.geojson")

head(commune)
dim(commune)


tab_commune <- data.frame(code_insee = commune$code, 
                          nom_commune = commune$nom,
                          code_departement = paste0("D", substr(commune$code, 1, 2)))

id_tmp <- which(substr(tab_commune$code_insee, 1, 2) == "97")
tab_commune$code_departement[id_tmp] <-  paste0("D", substr(commune$code[id_tmp], 1, 3))
# table(tab_commune$nom_departement)
# ok

tab_commune$nom_departement <- tab_dep[tab_commune$code_departement, "nom_departement"]
tab_commune$code_region <- tab_dep[tab_commune$code_departement, "code_region"]
tab_commune$nom_region <- tab_dep[tab_commune$code_departement, "nom_region"]

head(tab_commune)


# for(i in 1:nrow(commune)){
#   print(i)
#   get_centroids(commune[i, ])
#   
# }

commune[6706, ]
plot(commune[6706, ])
get_centroids(commune[6706, ])

tmp <- get_centroids(commune[-6706, ])
head(tmp)
tmp2 <- rbind(tmp[1:6705, ], c(-0.5528, 48.8510), tmp[6706:nrow(tmp), ])

nrow(tmp)
nrow(tmp2)
nrow(tab_commune)

head(tmp2)

tab_commune <- cbind(tab_commune, tmp2)

tab_commune[6704:6708, ]








# arrondissement

# Paris

tab_commune[tab_commune$code_departement=="D75", ]

tab_commune <- tab_commune[tab_commune$code_departement!="D75", ]

# https://www.data.gouv.fr/en/datasets/arrondissements-1/
paris <- read_sf("https://www.data.gouv.fr/fr/datasets/r/4765fe48-35fd-4536-b029-4727380ce23c")

paris2 <- cbind(data.frame(code_insee = paris$c_arinsee,
                           nom_commune = paris$l_aroff,
                           code_departement ="D75",
                           nom_departement ="Paris",
                           code_region = "R11",
                           nom_region = "Île-de-France"),
                get_centroids(paris))

tab_commune <- rbind(tab_commune, paris2)





# Lyon

tab_commune[tab_commune$nom_commune =="Lyon", ]
tab_commune <- tab_commune[tab_commune$nom_commune !="Lyon", ]

# https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5
lyon <- read_sf("https://transcode.geo.data.gouv.fr/services/5e2a1e77fa4268bc255379c2/feature-types/ms:adr_voie_lieu.adrarrond?format=GeoJSON&projection=WGS84")

lyon2 <- data.frame(code_insee = lyon$insee,
                           nom_commune = lyon$nomreduit,
                           code_departement ="D69",
                           nom_departement ="Rhône",
                           code_region = "R84",
                           nom_region = "Auvergne-Rhône-Alpes",
                          lon=NA,
                          lat=NA)

tab_commune <- rbind(tab_commune, lyon2)




# Marseille

tab_commune[tab_commune$nom_commune =="Marseille", ]
tab_commune <- tab_commune[tab_commune$nom_commune !="Marseille", ]

# https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/
# https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/?location=10,43.31319,5.73074&basemap=jawg.light&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6ImFycm9uZGlzc2VtZW50cy1taWxsZXNpbWVzMCIsIm9wdGlvbnMiOnt9fSwiY2hhcnRzIjpbeyJhbGlnbk1vbnRoIjp0cnVlLCJ0eXBlIjoiY29sdW1uIiwiZnVuYyI6IkFWRyIsInlBeGlzIjoiY29kZV9jb21tdW5lIiwic2NpZW50aWZpY0Rpc3BsYXkiOnRydWUsImNvbG9yIjoiI0ZGNTE1QSJ9XSwieEF4aXMiOiJjb2RlX2luc2VlIiwibWF4cG9pbnRzIjo1MCwic29ydCI6IiJ9XSwidGltZXNjYWxlIjoiIiwiZGlzcGxheUxlZ2VuZCI6dHJ1ZSwiYWxpZ25Nb250aCI6dHJ1ZX0%3D
Marseille <- read_sf("../data/arrondissements/arrondissements-millesimes0.geojson")

Marseille <- Marseille %>% filter(code_dpartement == 13)

Marseille2 <- cbind(data.frame(code_insee = Marseille$code_insee,
                           nom_commune = Marseille$commune,
                           code_departement ="D13",
                           nom_departement ="Marseille",
                           code_region = "R93",
                           nom_region = "Bouches-du-Rhône"),
                get_centroids(Marseille))

tab_commune <- rbind(tab_commune, Marseille2)



save(tab_region, tab_dep, tab_commune, file="../data/tab_reg_dep_com.Rdata")








# Paris -------------------------------------------------------------------






# # Pour récupérer les codes postaux
# 
# # FONCTIONNE PAS POUR TOUTES LES COMMUNES
# 
# 
# # source : data.gouv la poste
# # https://www.data.gouv.fr/en/datasets/base-officielle-des-codes-postaux/
# code_postal <- read.csv("data/code_postal.csv", header=TRUE, sep=",")
# 
# head(code_postal)
# dim(code_postal)
# 
# code_postal$code_insee <- as.character(code_postal$code_insee)
# code_postal$code_insee[nchar(code_postal$code_insee)==4] <- paste0("0", code_postal$code_insee[nchar(code_postal$code_insee)==4])
# 
# code_postal$code_postal <- as.character(code_postal$code_postal)
# code_postal$code_postal[nchar(code_postal$code_postal)==4] <- paste0("0", code_postal$code_postal[nchar(code_postal$code_postal)==4])
# 
# head(code_postal)
# 
# code_postal <- data.frame(code_postal)
# 
# 
# head(commune)
# 
# code_postal[code_postal$Code_commune_INSEE == "41257", ]
# code_postal[code_postal$Nom_commune == "THENAY", ]
# code_postal[code_postal$Nom_commune == "LYON", ]
# 
# 
# table(commune$code %in% code_postal$code_insee)
# 
# commune[! commune$code %in% code_postal$Code_commune_INSEE, ]







