
# Benjamin Heuclin
# 
# Application R shiny : Prix du mètre carré immobilier en France
# 
# 17/12/2021
# Licence : CC BY-NC-ND 4.0



rm(list=ls())

# library(tidyverse)
library(tidyr)
library(dplyr)

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(stringr)
library(DBI)
library(RSQLite)


load("data/tab_reg_dep_com.Rdata")




# region <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-avec-outre-mer.geojson")
# region$code <- paste0("R", region$code)

departement <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson")
departement$code2 <- departement$code
departement$code <- paste0("D", departement$code)

# commune <- read_sf("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes-avec-outre-mer.geojson")
commune <- commune_num_dep <- cadastre_all <- NULL

id_region <- NULL
id_departement <- NULL
id_commune <- NULL
id_cadastre <- NULL
cadastre <- NULL




list_reg <- as.list(tab_region$code_region)
names(list_reg) <- tab_region$nom_region
list_reg <- c(list(" - " = "R0"), list_reg)

list_dep <- as.list(tab_dep$code_departement)
names(list_dep) <- paste0(substr(tab_dep$code_departement, 2, nchar(tab_dep$code_departement)), " - ", tab_dep$nom_departement)
list_dep <- c(list(" - " = "D0"), list_dep)



# my_colorRamp <- "magma"
my_colorRamp <- "RdYlBu"
# my_colorRamp <-  colorRamp(c("#660000", "#990000", "#CC0000", "#FF0000", "#FF8000" ,
# "#FF9933", "#CCCC00", "#66CC00", "#00FFFF", "#0080FF", "#3333FF", 
# "#9933FF"), interpolate = "spline")
# my_bins <- c(0, 1000, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 
#              6000, 7000, 8000, 10000, 14000, Inf)



zoom_min_com <- 9
zoom_min_cad <- 13
zoom_max_dep <- zoom_min_com-1
zoom_max_com <- zoom_min_cad-1

old_commune <- old_commune_2 <- old_cadastre <- old_id_departement <- NULL

# Header ------------------------------------------------------------------
header <- dashboardHeader(title = "Prix du metre carré")


# Sidebar -----------------------------------------------------------------

tag_bas <- tags$head(
  tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    ")))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Carte", tabName = "Carte", icon = icon("map")),
    menuItem("A propos", icon = icon("info-circle"), tabName = "A_propos", badgeColor = "green"),
    
    br(),
    HTML("<center><a href='https://github.com/Heuclin'><strong> Benjamin Heuclin </strong></a><br>
         <a href='https://github.com/Heuclin/prix_m2_immobilier'><strong> GitHub </strong>  </a> <br>
         <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/'> <img src='licence_cc.png', width='100', height='33', alt='Licence CC BY-NC-ND'> </a>
         </center>"),
    # HTML("<center> <a href='https://github.com/Heuclin'> <img src='github_logo.png', width='80', height='60', alt='github'> </a> </center>")
    tag_bas
    ),
  width = 150,
  collapsed=FALSE)


# Body --------------------------------------------------------------------
body <- dashboardBody(
  
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="autor", content="Benjamin Heuclin"),
    tags$meta(name="description", content="Résumés statistiques (médiane, moyenne, quantiles ...) du prix de l'immobilier par type de bien (appartments, maisons et locaux commerciaux ou industriels) et par département, commune et section cadastrale calculés à partir des données issues des actes notariés des transactions immobilières intervenues depuis 2016"),
    tags$meta(name="keywords", content="valeur foncière, prix du mètre carré, prix du m2, immobilier, prix, évolution, Maison, Appartement, Local, Locaux, France, Paris, Lyon, Marseille, Nice, Montpellier, Toulouse, Bordeaux, Lille, Rouen, R shiny, département, commune, section cadastrale"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$meta(name="github-repo", content = "Heuclin/r4ds")
  ),
  
  tabItems(
    tabItem(tabName = "Carte",
            fluidRow(
              column(width = 8,
                     leafletOutput("map", height = 700)
              ),
              column(width=4,
                     # selectInput("selReg", label = "Région",
                     #             choices = list_reg,
                     #             selected = "R0", multiple = FALSE),
                     
                     selectInput("selDep", label = "Département",
                                 choices = list_dep,
                                 selected = "D0", multiple = FALSE),
                     
                     selectInput("selCom", label = "Commune",
                                 choices = list(" - " = "C0"),
                                 selected = "C0", multiple = FALSE),
                     
                     selectInput("selCad", label = "Section cadastrale",
                                 choices = list(" - " = "c0"),
                                 selected = "c0", multiple = TRUE),
                     
                     selectInput("Type_local", label = "Type de local",
                                 choices = list("Maison", "Appartement", "Local"),
                                 selected = "Maison", multiple = FALSE),
                     
                     plotOutput("boxplot")
              )
            )
    ),
    tabItem(tabName = "A_propos",
            HTML("
                 <h2> A propos </h2><br>
                 <strong> Auteur : Benjamin Heuclin </strong> <br>
                 <strong> Licence :</strong> <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/'> CC BY-NC-ND 4.0 </a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/'> <img src='licence_cc.png', width='150', height='50', alt='Licence CC BY-NC-ND'> </a><br>
                 <strong> Description :</strong> <p> Cette application R shiny fourni des résumés statistiques (médianes, moyennes, quantiles ...) 
                 du prix de l'immobilier par type de bien (appartements, maisons et locaux commerciaux ou industriels)
                 et par département, commune et section cadastrale. <br>
                 Ces résumés statistiques sont calculés à partir des données issues des actes notariés des transactions immobilières 
                 intervenues depuis 2016 sur le territoire métropolitain et 
                 les DOM-TOM, à l’exception de l’Alsace, de la Moselle et de Mayotte. <br>
                 Note importante : aucune information sur la qualité des biens n'est fourni dans les données.</p> 
                 <strong> Sources des données : </strong> <br>
                 <ul>
                  <li> Données brutes : Valeurs foncières : <a href='https://www.data.gouv.fr/en/datasets/demandes-de-valeurs-foncieres-geolocalisees/'> https://www.data.gouv.fr/en/datasets/demandes-de-valeurs-foncieres-geolocalisees/ </a> </li>
                  <li> Contours des départements et des communes de France : <a href='https://github.com/gregoiredavid/france-geojson'> https://github.com/gregoiredavid/france-geojson </a>  </li>
                  <li> Contours des arrondissements de Paris : <a href='https://www.data.gouv.fr/en/datasets/arrondissements-1/'> https://www.data.gouv.fr/en/datasets/arrondissements-1/ </a> </li>
                  <li> Contours des arrondissements de Lyon :  <a href='https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5'> https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5 </a> </li>
                  <li> Contours des arrondissements de Marseille :  <a href='https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/'> https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/ </a> </li>
                  <li> Contours des sections cadastrales :  <a href='https://cadastre.data.gouv.fr/datasets/cadastre-etalab'> https://cadastre.data.gouv.fr/datasets/cadastre-etalab </a> </li>
                  <li> Liste des départements de France : <a href='https://www.data.gouv.fr/en/datasets/departements-de-france/'> https://www.data.gouv.fr/en/datasets/departements-de-france/ </a> </li>
                  <li> Liste des codes postaux de France : <a href='https://www.data.gouv.fr/en/datasets/base-officielle-des-codes-postaux/'> https://www.data.gouv.fr/en/datasets/base-officielle-des-codes-postaux/ </a> </li>
                 </ul> 
                 <strong> Codes : </strong> <a href='https://github.com/Heuclin/prix_m2_immobilier'> https://github.com/Heuclin/prix_m2_immobilier </a> <br>
                 <strong> A venir : </strong> <br>
                 <ul>
                  <li> Inclure les ventes de terrain (nécessite un traîtement particulier) </li>
                  <li> Créer un prédicteur de valeur foncière </li>
                 </ul>
                 <strong>Date de création : </strong> décembre 2021 <br>
                 <strong>Dernière mise à jour : </strong> 03/01/2022 <br>
                 <strong>Contact : </strong> <a href='mailto: prix.m2.immobilier.contact@gmail.com'> prix.m2.immobilier.contact@gmail.com </a> <br>
                 ")
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")















# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  observeEvent(input$Type_local, {
    commune_num_dep <- NULL
    commune <- NULL
    cadastre_all <- NULL
    
    updateSelectInput(inputId ="selDep",
                      label = "Département",
                      choices = list_dep,
                      selected = "D0")
    
    updateSelectInput(inputId ="selCom",
                      label = "Commune",
                      choices = list(" - " = "C0"),
                      selected = "C0")
    
    updateSelectInput(inputId ="selCad",
                      label = "Cadastre",
                      choices = list(" - " = "c0"),
                      selected = "c0")
    output$boxplot <- renderPlot({})
    
    old_commune <- old_commune_2 <- old_cadastre <- old_id_departement <- NULL
    
  })
  
  output$map <- renderLeaflet({
    
    load("data/DVF/summary_par_departement/data_sum_dep_all.Rdata")
    data_sum_dep_all <- data_sum_dep_all %>% filter(type_local == input$Type_local)
    
    departement_tmp <- departement %>% left_join(data_sum_dep_all, by=c("code" = "code_departement"))
    
    my_bins_dep <- round(quantile(data_sum_dep_all$med, probs = seq(0, 1, 0.1), na.rm=TRUE))
    
    
    my_pal <- colorBin(
      palette = my_colorRamp, #"RdBu",
      domain = departement_tmp$med,
      reverse = TRUE,
      # Echelle avec des quantiles sinon Paris prend toute la place...
      bins = my_bins_dep 
    )
    # print(quantile(departement_tmp$med, probs = c(0, 10, 20, 40, 50, 60, 70, 80, 90, 95, 98, 99, 100)/100, na.rm=TRUE))
    labels_dep <<- sprintf(
      "<strong>%s - %s</strong><br/> 
            prix du m<sup>2</sup> median : %d <br/>
                    calculer sur %d mutations <br/>
                    de 2016 à 2021",
      departement_tmp$code2, departement_tmp$nom, departement_tmp$med, departement_tmp$n
    ) %>% lapply(htmltools::HTML)
    # names(labels_dep) <- departement_tmp$code
    
    leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT", options = providerTileOptions(minZoom=2, maxZoom=18)) %>%
      setView(2.2137, 46.2276, 6) %>% # centre de la france
      # setView(3.8767, 43.6108, 7) %>% # Montpellier
      # Départements
      # addPolygons(
      #     data = departement_tmp,
      #     weight = 4,
      #     color= "black",
      #     opacity = 1,
      #     fill = FALSE) %>%
      addPolygons(
        data = departement_tmp,
        layerId = ~code,
        weight = 2,
        color= "black",
        opacity = 0.8,
        label = labels_dep, #~nom,
        fill = TRUE,
        # Application de la fonction palette
        fillColor = ~my_pal(med),
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "red",
                                            weight = 3, opacity = 1),
        group = ~code) %>%
      addLegend(pal=my_pal, values=departement_tmp$med, opacity=0.9,  layerId = "legend",
                title = "Prix du m2 median", position = "bottomleft" ) %>%
      groupOptions("min_zoom_commune",  zoomLevels = zoom_min_com:30)  
  })
  
  
  # modification des widget departement / commune / cadastre par clique sur la carte
  observeEvent(input$map_shape_click, {
    # click event
    click <- input$map_shape_click
    id <- click$id
    # print(id)
    
    if(substr(id, 1, 1) == "D"){ # Département
      id_departement <- id
      id_region <- tab_dep[id_departement, "code_region"]
      
      # list_dep_tmp <- list_dep[c(TRUE, tab_dep$code_region == id_region)]
      list_dep_tmp <- list_dep
      updateSelectInput(inputId ="selDep",
                        label = "Département",
                        choices = list_dep_tmp,
                        selected = id_departement)
      
      updateSelectInput(inputId ="selCom",
                        label = "Commune",
                        choices = list(" - " = "C0"),
                        selected = "C0")
      
      updateSelectInput(inputId ="selCad",
                        label = "Cadastre",
                        choices = list(" - " = "c0"),
                        selected = "c0")
      
      
    }else if(nchar(id)==5){ # commune
      id_commune <- id
      tmp_dep <- ifelse(substr(id_commune, 1, 2) == "97", substr(id_commune, 1, 3), substr(id_commune, 1, 2))
      id_departement <- paste0("D", tmp_dep)
      id_region <- tab_dep[id_departement, "code_region"]
      id_cadastre <- "c0"
      
      list_com_tmp <- as.list(tab_commune[tab_commune$code_departement == id_departement, "code_insee"])
      names(list_com_tmp) <- paste0(tab_commune[tab_commune$code_departement == id_departement, "nom_commune"])
      list_com_tmp <- c(list(" - " = "C0"), list_com_tmp)
      
      updateSelectInput(inputId ="selCom",
                        label = "Commune",
                        choices = list_com_tmp,
                        selected = id_commune)
      
      updateSelectInput(inputId ="selCad",
                        label = "Cadastre",
                        choices = list(" - " = "c0"),
                        selected = "c0")
      
      
    }else if(nchar(id)==10){ # cadastre
      id_cadastre <- id
      id_commune <- substr(id, 1, 5)
      tmp_dep <- ifelse(substr(id_commune, 1, 2) == "97", substr(id_commune, 1, 3), substr(id_commune, 1, 2))
      id_departement <- paste0("D", tmp_dep)
      id_region <- tab_dep[id_departement, "code_region"]
      
      id_tmp <- substr(cadastre_all, 1, 5) == id_commune
      list_cad_tmp <- as.list(cadastre_all[id_tmp])
      names(list_cad_tmp) <- cadastre_all[id_tmp]
      list_cad_tmp <- c(list(" - " = "c0"), list_cad_tmp)
      
      updateSelectInput(inputId ="selCad",
                        label = "Cadastre",
                        choices = list_cad_tmp,
                        selected = id_cadastre)
    }
    
    
  })
  
  
  
  # Departement
  observeEvent(input$selDep, {
    print(input$selDep)
    
    if(input$selDep != "D0"){
      id <- id_departement <- input$selDep
      num_dep <- substr(id_departement, 2, nchar(id_departement))
      
      id_region <- tab_dep[id_departement, "code_region"]
      id_commune <- "C0"
      id_cadastre <- "c0"
      
      
      tmp <- tab_dep[id_departement, "nom_departement_2"]
      if(id_departement != "D75"){
        commune_tmp <- read_sf(paste0(
          "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements/",
          tmp, "/communes-", tmp,".geojson"
        ))
        # browser()
        if(id_departement == "D69"){
          commune_tmp <- commune_tmp %>% filter(nom != "Lyon")
          # https://geo.data.gouv.fr/fr/datasets/b086ed56567269ede1a9ea280c5ff25ba28554e5
          # lyon <- read_sf("https://transcode.geo.data.gouv.fr/services/5e2a1e77fa4268bc255379c2/feature-types/ms:adr_voie_lieu.adrarrond?format=GeoJSON&projection=WGS84")
          lyon <- read_sf("data/arrondissements/arrondissements_lyon.json")
          lyon <- lyon %>% mutate(code = insee, nom = nomreduit) %>% select(code, nom, geometry)
          commune_tmp <- rbind(commune_tmp, lyon)
        }
        
        if(id_departement %in% c("D13")){
          commune_tmp <- commune_tmp %>% filter(!nom %in% c("Marseille", "Lyon", "Paris"))
          # https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/?location=10,43.31319,5.73074&basemap=jawg.light&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6ImFycm9uZGlzc2VtZW50cy1taWxsZXNpbWVzMCIsIm9wdGlvbnMiOnt9fSwiY2hhcnRzIjpbeyJhbGlnbk1vbnRoIjp0cnVlLCJ0eXBlIjoiY29sdW1uIiwiZnVuYyI6IkFWRyIsInlBeGlzIjoiY29kZV9jb21tdW5lIiwic2NpZW50aWZpY0Rpc3BsYXkiOnRydWUsImNvbG9yIjoiI0ZGNTE1QSJ9XSwieEF4aXMiOiJjb2RlX2luc2VlIiwibWF4cG9pbnRzIjo1MCwic29ydCI6IiJ9XSwidGltZXNjYWxlIjoiIiwiZGlzcGxheUxlZ2VuZCI6dHJ1ZSwiYWxpZ25Nb250aCI6dHJ1ZX0%3D
          data_tmp <- read_sf("data/arrondissements/arrondissements-millesimes0.geojson")
          
          data_tmp <- data_tmp %>% filter(code_dpartement == substr(id_departement, 2, 3)) %>% mutate(code = code_insee, nom = commune) %>% select(code, nom, geometry)
          commune_tmp <- rbind(commune_tmp, data_tmp)
        } 
        
      }else{ # Paris
        commune_tmp <- read_sf("https://www.data.gouv.fr/fr/datasets/r/4765fe48-35fd-4536-b029-4727380ce23c")
        commune_tmp <- commune_tmp %>% 
          mutate(code = as.character(c_arinsee),
                 nom = paste0("Paris ", c_ar, " ", l_aroff)) %>%
          select(code, nom, geometry)
        
        # # https://public.opendatasoft.com/explore/dataset/arrondissements-millesimes0/information/?location=10,43.31319,5.73074&basemap=jawg.light&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6ImFycm9uZGlzc2VtZW50cy1taWxsZXNpbWVzMCIsIm9wdGlvbnMiOnt9fSwiY2hhcnRzIjpbeyJhbGlnbk1vbnRoIjp0cnVlLCJ0eXBlIjoiY29sdW1uIiwiZnVuYyI6IkFWRyIsInlBeGlzIjoiY29kZV9jb21tdW5lIiwic2NpZW50aWZpY0Rpc3BsYXkiOnRydWUsImNvbG9yIjoiI0ZGNTE1QSJ9XSwieEF4aXMiOiJjb2RlX2luc2VlIiwibWF4cG9pbnRzIjo1MCwic29ydCI6IiJ9XSwidGltZXNjYWxlIjoiIiwiZGlzcGxheUxlZ2VuZCI6dHJ1ZSwiYWxpZ25Nb250aCI6dHJ1ZX0%3D
        # commune_tmp <- read_sf("data/arrondissements/arrondissements-millesimes0.geojson")
        # commune_tmp <- commune_tmp %>% filter(code_dpartement == substr(id_departement, 2, 3)) %>% mutate(code = code_insee, nom = commune) %>% select(code, nom, geometry)
        
      }
      
      # chargement des communes :
      if(! id_departement %in% commune_num_dep){
        commune_num_dep <<- c(commune_num_dep, id_departement)
        commune <<- rbind(commune, commune_tmp)
      }
      
      Type_local_tmp <- input$Type_local
      db_dep <- dbConnect(SQLite(), dbname = paste0("data/DVF/summary_par_departement/", id_departement, "/summary_dep_", id_departement, ".sqlite"))
      # dbListTables(db_dep)
      # data_sum_com_annee <- tbl(db_dep, "dvf_sum_com_annee") %>% filter(annee=="2020") %>% filter(type_local == Type_local_tmp) %>% as_tibble()
      data_sum_com_annee <- tbl(db_dep, "dvf_sum_com") %>% filter(type_local == Type_local_tmp) %>% as_tibble()
      data_cad_med <- tbl(db_dep, "dvf_sum_cad_annee") %>% filter(type_local == Type_local_tmp) %>% select(med) %>% as_tibble()
      dbDisconnect(db_dep)
      
      my_bins <<- round(quantile(data_cad_med, prob = seq(0, 1, 0.1), na.rm=TRUE))
      
      commune_tmp <- commune_tmp %>% left_join(data_sum_com_annee %>% select(code_commune, med, n), by=c("code" = "code_commune"))
      
      
      
      my_pal <- colorBin(
        palette = my_colorRamp, #"RdBu",
        domain = commune_tmp$med,
        reverse = TRUE,
        # Echelle avec des quantiles sinon Paris prend toute la place...
        bins = my_bins #quantile(commune_tmp$med, probs = c(0, 10, 20, 40, 50, 60, 70, 80, 90, 95, 98, 99, 100)/100, na.rm=TRUE)
      )
      # print(quantile(departement_tmp$med, probs = c(0, 10, 20, 40, 50, 60, 70, 80, 90, 95, 98, 99, 100)/100, na.rm=TRUE))
      labels <- sprintf(
        "<strong>%s</strong><br/> 
                    prix du m<sup>2</sup> median : %d <br/>
                    calculer sur %d mutations <br/>
                    de 2016 à 2021",
        commune_tmp$nom, commune_tmp$med, commune_tmp$n
      ) %>% lapply(htmltools::HTML)
      
      
      leafletProxy("map") %>% 
        removeShape(old_commune) %>%
        removeShape(old_cadastre) %>%
        removeShape(old_commune_2) %>%
        # clearGroup("min_zoom_commune") %>%
        groupOptions(old_id_departement,   zoomLevels = 2:30) %>%
        # addPolylines(
        #     data = departement[departement$code==id_departement, ],
        #     layerId = id_departement,
        #     weight = 3,
        #     color="red",
        #     opacity =1,
        #     label = labels_dep[[which(departement$code == id_departement)]],
        #     fill = TRUE,
        #     fillOpacity = 0,
        #     highlightOptions = highlightOptions(color = "red",
        #                                         weight = 3, opacity = 1),
      #     group = "min_zoom_dep") %>%
      addPolylines(
        data = commune_tmp, 
        layerId = ~nom,
        weight = 3,
        color="black",
        opacity =1,
        fill = FALSE,
        group = "min_zoom_commune") %>%
        addPolylines(
          data = commune_tmp, 
          layerId = ~code,
          weight = 1,
          color="black",
          opacity =1,
          label = labels,
          fill = TRUE,
          # Application de la fonction palette
          fillColor = ~my_pal(med),
          fillOpacity = 0.4,
          highlightOptions = highlightOptions(color = "red",
                                              weight = 3, opacity = 1),
          group = ~code) %>%            
        groupOptions(commune_tmp$code, zoomLevels = zoom_min_com:30)  %>%
        groupOptions(id_departement,   zoomLevels = 2:zoom_max_dep) %>%
        addLegend(pal=my_pal, values=commune_tmp$med, opacity=0.9, layerId = "legend",
                  title = "Prix du m2 median", position = "bottomleft" )
      
      if(input$map_zoom < zoom_min_com) leafletProxy("map") %>% 
        setView(tab_dep[id_departement, "lon"], tab_dep[id_departement, "lat"], zoom_min_com)
      
      old_commune <<- commune_tmp$code
      old_commune_2 <<- commune$nom
      old_id_departement <<- id_departement
      old_cadastre <- NULL
      
      
      list_com_tmp <- as.list(tab_commune[tab_commune$code_departement == id_departement, "code_insee"])
      names(list_com_tmp) <- paste0(tab_commune[tab_commune$code_departement == id_departement, "nom_commune"])
      list_com_tmp <- list_com_tmp[order(names(list_com_tmp))]
      list_com_tmp <- c(list(" - " = "C0"), list_com_tmp)
    }else{
      list_com_tmp = list(" - " = "C0")
    }
    
    
    if((input$selDep != "D0") & (input$selCom %in% c("", "C0"))) {
      output$boxplot <- renderPlot({
        
        load(paste0("data/DVF/summary_par_departement/", input$selDep, "/dvf_sum_dep_annee.Rdata"))
        data_sum_dep_annee <- data_sum_dep_annee %>% filter(type_local==input$Type_local)
        nb_obs_annee <- data_sum_dep_annee$n
        mean <- data_sum_dep_annee$mean
        data_sum_dep_annee$annee_order <- as.numeric(factor(data_sum_dep_annee$annee)) #, labels = order(unique(data_sum_dep_annee$annee)))
        
        tmp <- data_sum_dep_annee %>% 
          pivot_longer(cols=c("C5", "Q1", "med", "Q3", "C95"), names_to = "quantile", values_to = "valeur") %>% 
          mutate(number = as.numeric(annee)-2015)
        
        dif_plus <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.04)
        dif_moins <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.025)
        tmp <- tmp %>% mutate(pos = valeur + dif_plus)
        tmp$pos[tmp$quantile %in% c("C5", "Q3")] <- tmp$valeur[tmp$quantile %in% c("C5", "Q3")]-c(dif_moins)
        
        par(mar=c(3, 3, 3, 1))
        boxplot(tmp$valeur ~ tmp$annee, main=paste0(tab_dep[id_departement, "nom_departement_2"], ", ", input$Type_local, ", nb mutations (n) : ", sum(data_sum_dep_annee$n)), xlab="", ylab="Prix du m2", ylim =range(tmp$valeur)+c(-200, +300))
        text(x=unique(tmp$annee_order), y=mean,  labels="*", cex=3, col=2)
        text(x=tmp$annee_order, y=tmp$pos,  labels=tmp$valeur, cex=0.9, col=1)
        mtext(text = paste0("n = ", nb_obs_annee), at = 1:length(nb_obs_annee), cex=1, col=1, side = 1,
              line=2)
        
      })
    }
    
    
    selected_com <- "C0"
    if(! input$selCom %in% c("", "C0")){
      if(substr(input$selCom, 1, 2) == substr(id_departement, 2, 3)){
        selected_com <- input$selCom
      } # else{leafletProxy("map") %>% removeShape(layerId= "toto2")}
    }
    
    updateSelectInput(inputId ="selCom",
                      label = "Commune",
                      choices = list_com_tmp,
                      selected = selected_com)
    
    
  })
  
  # COMMUNE
  observeEvent(input$selCom, {
    print(input$selCom)
    
    if(! input$selCom %in% c("", "C0")){
      id_commune <- id <- input$selCom
      tmp_dep <- ifelse(substr(id_commune, 1, 2) == "97", substr(id_commune, 1, 3), substr(id_commune, 1, 2))
      id_departement <- paste0("D", tmp_dep)
      id_region <- tab_dep[id_departement, "code_region"]
      id_cadastre <- "c0"
      
      # chargement et affichage des cadastres par commune
      # if(! id_commune %in% c("69123", "13055", "75056")){
        # browser()
        cadastre_geojson <- read_sf(paste0("https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/", id_commune, "/geojson/sections"))
      # }else{
      #   cadastre_geojson <- read_sf(paste0("https://cadastre.data.gouv.fr/bundler/cadastre-etalab/departements/", substr(id_commune, 1, 2), "/geojson/sections"))
      # }
      
      Type_local_tmp <- input$Type_local
      db_dep <- dbConnect(SQLite(), dbname = paste0("data/DVF/summary_par_departement/", id_departement, "/summary_dep_", id_departement, ".sqlite"))
      # dbListTables(db_dep)
      data_sum_cad <- tbl(db_dep, "dvf_sum_cad") %>% filter(type_local == Type_local_tmp, code_commune == id_commune) %>% as_tibble()
      dbDisconnect(db_dep)
      
      
      cadastre_geojson <- cadastre_geojson %>% left_join(data_sum_cad %>% select(section_cadastrale, med, n), by=c("id" = "section_cadastrale"))
      
      
      my_pal <- colorBin(
        palette = my_colorRamp, #"RdBu",
        domain = cadastre_geojson$med,
        reverse = TRUE,
        bins = my_bins #quantile(cadastre_geojson$med, probs = c(0, 10, 20, 40, 50, 60, 70, 80, 90, 95, 98, 99, 100)/100, na.rm=TRUE)
      )
      labels_cad <- sprintf(
        "<strong>%s</strong><br/>
                    prix du m<sup>2</sup> median : %d <br/>
                    calculer sur %d mutations <br/>
                    de 2016 à 2021",
        cadastre_geojson$code, cadastre_geojson$med, cadastre_geojson$n
      ) %>% lapply(htmltools::HTML)
      
      
      leafletProxy("map") %>%
        # addPolylines(
        #     data = commune[commune$code==id_commune, ], # commune[substr(commune$code, 1, nchar(id)-1) == num_dep, ]
        #     layerId = "toto",
        #     weight = 2,
        #     color="red",
        #     opacity =1,
        #     label = ~ nom,
        #     fill = TRUE,
        #     # Application de la fonction palette
        #     # fillColor = ~pal(density),
        #     fillOpacity = 0,
      #     highlightOptions = highlightOptions(color = "red",
      #                                         weight = 3, opacity = 1),
      #     group = "min_zoom_commune") %>%
      addPolygons(
        data = cadastre_geojson,
        layerId = ~id,
        weight = 1,
        color="black",
        opacity =1,
        label = labels_cad,
        fill = TRUE,
        fillColor = ~my_pal(med),
        fillOpacity = 0.4,
        highlightOptions = highlightOptions(color = "red",
                                            weight = 3, opacity = 1),
        group = ~id) %>%
        groupOptions(cadastre_geojson$id, zoomLevels = zoom_min_cad:30)%>%
        groupOptions(id_commune, zoomLevels = zoom_min_com:zoom_max_com)  %>%
        addLegend(pal=my_pal, values=cadastre_geojson$med, opacity=0.9,  layerId = "legend",
                  title = "Prix du m2 median", position = "bottomleft" )
      
      old_cadastre <<- c(old_cadastre, cadastre_geojson$id)
      if(input$selCom != id_departement){
        updateSelectInput(inputId ="selDep",
                          label = "Département",
                          choices = list_dep,
                          selected = id_departement)
      }
      
      cadastre_all <<- unique(c(cadastre_all, cadastre_geojson$id))
      
      list_cad_tmp <- as.list(cadastre_geojson$id)
      list_cad_tmp <- c(list(" - " = "c0"), list_cad_tmp)
    }else{
      list_cad_tmp <- list(" - " = "c0")
    }
    
    selected_cad <- "c0"
    if(! input$selCad %in% c("", "c0")){
      if(substr(input$selCad, 1, 5) == id_commune){
        selected_cad <- input$selCad
      } 
    }
    updateSelectInput(inputId ="selCad",
                      label = "Cadastre",
                      choices = list_cad_tmp,
                      selected = selected_cad)
    
    
    if((input$selCom != "C0") & (input$selCad %in% c("", "c0"))) {
      output$boxplot <- renderPlot({
        
        Type_local_tmp <- input$Type_local
        db_dep <- dbConnect(SQLite(), dbname = paste0("data/DVF/summary_par_departement/", id_departement, "/summary_dep_", id_departement, ".sqlite"))
        # dbListTables(db_dep)
        data_sum_com_annee <- tbl(db_dep, "dvf_sum_com_annee") %>% filter(type_local == Type_local_tmp, code_commune == id_commune) %>% as_tibble()
        dbDisconnect(db_dep)
        
        data_sum_com_annee$annee_order <- as.numeric(factor(data_sum_com_annee$annee)) #, labels = order(unique(data_sum_com_annee$annee)))
        
        nb_obs_annee <- data_sum_com_annee$n
        tmp <- data_sum_com_annee %>%
          pivot_longer(cols=c("C5", "Q1", "med", "Q3", "C95"), names_to = "quantile", values_to = "valeur")
        
        dif_plus <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.04)
        dif_moins <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.025)
        tmp <- tmp %>% mutate(pos = valeur + dif_plus)
        tmp$pos[tmp$quantile %in% c("C5", "Q3")] <- tmp$valeur[tmp$quantile %in% c("C5", "Q3")]-c(dif_moins)
        
        id_tmp <- which(tab_commune$code_insee == id_commune)[1]
        
        par(mar=c(3, 3, 3, 1))
        boxplot(tmp$valeur ~ tmp$annee, main=paste0(tab_commune[id_tmp, "nom_commune"], ", ", input$Type_local, ", nb mutations (n) : ", sum(data_sum_com_annee$n)), xlab="", ylab="Prix du m2", ylim =range(tmp$valeur)+c(-200, +300))
        text(x=unique(tmp$annee_order), y=data_sum_com_annee$mean,  labels="*", cex=3, col=2)
        text(x=tmp$annee_order, y=tmp$pos,  labels=tmp$valeur,
             cex=0.9, col=1)
        mtext(text = paste0("n = ", nb_obs_annee), at = 1:length(nb_obs_annee), cex=1, col=1, side = 1,
              line=2)
        
      })
    }
  })
  
  # CADASTRE
  observeEvent(input$selCad, {
    print(input$selCad)
    
    
    if(! input$selCad  %in% c("", "c0")){
      id_cadastre <- input$selCad
      id_commune <- substr(id_cadastre, 1, 5)
      tmp_dep <- ifelse(substr(id_commune, 1, 2) == "97", substr(id_commune, 1, 3), substr(id_commune, 1, 2))
      id_departement <- paste0("D", tmp_dep)
      id_region <- tab_dep[id_departement, "code_region"]
      
      output$boxplot <- renderPlot({
        
        Type_local_tmp <- input$Type_local
        db_dep <- dbConnect(SQLite(), dbname = paste0("data/DVF/summary_par_departement/", id_departement, "/summary_dep_", id_departement, ".sqlite"))
        dbListTables(db_dep)
        data_sum_cad_annee <- tbl(db_dep, "dvf_sum_cad_annee") %>% filter(type_local == Type_local_tmp, section_cadastrale == id_cadastre) %>% as_tibble()
        dbDisconnect(db_dep)
        nb_obs_annee <- data_sum_cad_annee$n
        data_sum_cad_annee$annee_order <- as.numeric(factor(data_sum_cad_annee$annee)) #, labels = order(unique(data_sum_cad_annee$annee))))
        
        tmp <- data_sum_cad_annee %>%
          pivot_longer(cols=c("C5", "Q1", "med", "Q3", "C95"), names_to = "quantile", values_to = "valeur") %>%
          mutate(number = as.numeric(annee)-2015)
        
        dif_plus <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.04)
        dif_moins <- as.numeric(tmp[(tmp$quantile=="med") & (tmp$annee=="2020"), "valeur"] * 0.025)
        tmp <- tmp %>% mutate(pos = valeur + dif_plus)
        tmp$pos[tmp$quantile %in% c("C5", "Q3")] <- tmp$valeur[tmp$quantile %in% c("C5", "Q3")]-c(dif_moins)
        
        id_tmp <- which(tab_commune$code_insee == id_commune)[1]
        
        par(mar=c(3, 3, 3, 1))
        boxplot(tmp$valeur ~ tmp$annee, main=paste0(id_cadastre, ", ", input$Type_local, ", nb mutations (n) : ", sum(data_sum_cad_annee$n)), xlab="", ylab="Prix du m2", ylim =range(tmp$valeur)+c(-200, +300))
        text(x=unique(tmp$annee_order), y=data_sum_cad_annee$mean,  labels="*", cex=3, col=2)
        text(x=tmp$annee_order, y=tmp$pos,  labels=tmp$valeur,
             cex=0.9, col=1)
        mtext(text = paste0("n = ", nb_obs_annee), at = 1:length(nb_obs_annee), cex=1, col=1, side = 1,
              line=2)
        
      })
      
      
      
      
      list_com_tmp <- as.list(tab_commune[tab_commune$code_departement == id_departement, "code_insee"])
      names(list_com_tmp) <- paste0(tab_commune[tab_commune$code_departement == id_departement, "nom_commune"])
      list_com_tmp <- c(list(" - " = "C0"), list_com_tmp)
      
      if(input$selCom != id_commune){
        updateSelectInput(inputId ="selCom",
                          label = "Commune",
                          choices = list_com_tmp,
                          selected = id_commune)
      }
    }
    
    
  })
}






# Run the application 
shinyApp(ui = ui, server = server)
