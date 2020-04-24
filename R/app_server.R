#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr %>% filter group_by mutate select rename ungroup summarise arrange desc pull case_when between last
#' @importFrom httr GET write_disk authenticate
#' @importFrom readxl read_excel
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_text theme_minimal scale_x_discrete scale_x_date labs theme element_text
#' @importFrom scales breaks_width
#' @importFrom utils head
#' @importFrom rworldmap joinCountryData2Map mapCountryData
#' @importFrom lubridate date as_date
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom shinyjs show
#' @importFrom shinydashboard renderValueBox

#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  global <- reactiveValues()
  
  global$hier<-format(Sys.Date()-1, "%d/%m/%Y")
  
  # Chargement initial des donnees
  observeEvent(input$chgt,{
    chemin_url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date()-1, "%Y-%m-%d"), ".xlsx", sep = "")
    
    #download the dataset from the website to a local temporary file
    GET(chemin_url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    covid1 <- read_excel(tf,sheet = 1)
    
    # renommage des quelques variables
    global$covid <- covid1 %>% 
      rename(continent=continentExp,
             codepays=countryterritoryCode,
             pays=countriesAndTerritories, 
             date=dateRep,
             morts=deaths) %>%
      select(continent,codepays,pays,date,morts)
    
    # pour cacher l apercu tant que les donnees ne sont pas chargees
    shinyjs::show(id="box_apercu")
    
  })
  
  # selection des donnees selon les dates selectionnees dans le sliderinput
  observeEvent( c(global$covid,input$choix_date) , {
    req(global$covid)
    global$covid_date <- global$covid %>%
      dplyr::filter(between(as_date(date),input$choix_date[1],input$choix_date[2]))
    
    global$total_par_jour<-global$covid_date %>%
      group_by(date) %>% 
      mutate(pays="tous_les_pays",morts=sum(morts)) %>% 
      ungroup() %>% 
      select(pays,date,morts) %>% 
      unique()
    
    # ajout dans le fichier initial covid
    global$total_par_jour<-global$covid_date %>% 
      select(pays,date,morts) %>% 
      rbind(global$total_par_jour) %>% 
      mutate(date=date(x = date)) %>% 
      arrange(pays,date)
    
    # calcul du nombre total de deces
    global$deces<-global$covid_date %>% 
      filter(!is.na(codepays)) %>%       
      group_by(codepays,pays) %>% 
      summarise(total_deces=sum(morts)) %>% 
      arrange(desc(total_deces)) %>% 
      ungroup() %>% 
      mutate(covid19=case_when(
        total_deces > 0 ~ "1",
        TRUE            ~ "0"),
        classe_covid=cut(total_deces, 
                         breaks=c(-Inf,0, 50, 500,5000,10000,20000,50000,1000000),
                         include.lowest = FALSE,
                         right = TRUE,
                         labels = c("Pas de cas","<50","<500","<5000","<10000","<20000","<50000",">50000")))
    
    # selection des pays avec plus de 100 deces
    global$top100<-global$deces %>% 
      filter(total_deces>=100) %>% 
      arrange(desc(total_deces))
    
    # recuperation de la liste de tous les pays avec plus de 100 deces + tous_les_pays 
    global$ordre<- global$top100 %>% pull(pays)
    global$ordre_avec_tous<-c("tous_les_pays",global$ordre)
    
  })

  
  output$apercu <- function(){
    req(global$covid)
    global$covid %>% 
      arrange(desc(date),pays) %>% 
      head(500) %>% 
      arrange(date,desc(pays)) %>% 
      head(10) %>% 
      arrange(date,pays) %>% 
      knitr::kable("html") %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
  }
  
  output$deces <- renderTable({
    req(global$top100)
    global$top100
  })
  
  # preparation de la capture d ecran du site europeen
  output$site_europeen <- renderImage({
    list(src="inst/app/www/site_europeen_covid19.png",
         height=350,
         contentType = 'image/png',
         alt="Capture d'\u00E9cran du site europ\u00E9en")
  },deleteFile = FALSE)
  
  output$logo_suivicovid <- renderImage({
    list(src="inst/app/www/favicon.png",
         height=100,width=100,style="margin:0px 50px",
         contentType = 'image/png',
         alt="Logo de l appli Suivi COVID")
  },deleteFile = FALSE)
  
  # preparation des ValueBox du tableau de bord
  #############################################
  # nombre de nouveaux cas
  output$box_nouveauxcas<-renderValueBox({
    req(global$covid)
    nb_nouveauxcas<-global$covid %>% 
      filter(date==max(date)) %>% 
      summarise(nb_nouveauxcas=sum(morts)) %>% 
      pull(nb_nouveauxcas)
    
    valueBox(
      value = tags$p(nb_nouveauxcas, style = "font-size: 150%;color: white; font-weight: bold;"),
      subtitle = tags$p("Nouveaux cas hier", style = "font-size: 220%;color: white; font-weight: bold"),
      icon = icon("cross"),
      width=6
    )
  })
  
  # nombre total de morts dans le monde au total
  output$box_mortstot<-renderValueBox({
    req(global$covid)
    nb_morts_tot_hier<-global$covid %>% 
      summarise(nb_morts_tot_hier=sum(morts)) %>% 
      pull(nb_morts_tot_hier)
    
    valueBox(
      value = tags$p(nb_morts_tot_hier, style = "font-size: 120%;color: white; font-weight: bold;"),
      subtitle = tags$p("Nombre total de morts", style = "font-size: 220%;color: white; font-weight: bold"),
      icon = icon("globe"),
      color="orange",
      width=6
    )
  }) 
  # nombre de nouveaux cas
  output$box_nbpays100<-renderValueBox({
    req(global$covid)
    nbpays100<-global$covid %>% 
      filter(!is.na(codepays)) %>%       
      group_by(codepays,pays) %>% 
      summarise(total_deces=sum(morts)) %>% 
      filter(total_deces>=100) %>% 
      nrow() 
    valueBox(
      value = tags$p(nbpays100, style = "font-size: 150%;color: white; font-weight: bold;"),
      subtitle = tags$p("Nombre de pays avec plus de 100 d\u00E9c\u00E8s", style = "font-size: 220%;color: white; font-weight: bold"),
      icon = icon("frown"),
      color="green",
      width=6
    )
  })
  # nombre de nouveaux cas
  output$box_pirepays<-renderValueBox({
    req(global$covid)
    pirepays<-global$covid %>% 
      filter(!is.na(codepays)) %>%       
      group_by(codepays,pays) %>% 
      summarise(total_deces=sum(morts)) %>% 
      ungroup() %>% 
      filter(total_deces==max(total_deces)) %>% 
      mutate(pays2=paste0(pays,"(",total_deces,")")) %>% 
      pull(pays2)
    valueBox(
      value = tags$p(pirepays, style = "font-size: 80%;color: white; font-weight: bold;"),
      subtitle = tags$p("Pays avec le plus de cas hier", style = "font-size: 220%;color: white; font-weight: bold"),
      icon = icon("flag"),
      color="purple",
      width=6
    )
  })  
  
  # lancement des differents modules
  
  callModule(mod_apercu_server, "apercu_ui_1",r=global)  
  
  callModule(mod_histogramme_server, "histogramme_ui_1",r=global)
  
  callModule(mod_progression_server, "progression_ui_1",r=global)
  
  callModule(mod_progressionavectout_server, "progressionavectout_ui_1",r=global)
  
  callModule(mod_carte_server, "carte_ui_1",r=global)
  
  callModule(mod_bulles_server, "bulles_ui_1",r=global)
  
  callModule(mod_nuage_server, "nuage_ui_1",r=global)

}
