#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr %>% filter group_by mutate select rename ungroup summarise arrange desc pull case_when
#' @importFrom httr GET write_disk authenticate
#' @importFrom readxl read_excel
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_text theme_minimal scale_x_discrete scale_x_date labs theme element_text
#' @importFrom scales breaks_width
#' @importFrom utils head
#' @importFrom rworldmap joinCountryData2Map mapCountryData
#' @importFrom lubridate date
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom shinyjs show
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  global <- reactiveValues()
  
  global$hier<-format(Sys.Date()-1, "%d/%m/%Y")
  
  # creation de la selection des donnees
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
    
    # calcul du nombre de deces total par jour
    global$total_par_jour<-global$covid %>%
      group_by(date) %>% 
      mutate(pays="tous_les_pays",morts=sum(morts)) %>% 
      ungroup() %>% 
      select(pays,date,morts) %>% 
      unique()
    
    # ajout dans le fichier initial covid
    global$total_par_jour<-global$covid %>% 
      select(pays,date,morts) %>% 
      rbind(global$total_par_jour) %>% 
      mutate(date=date(x = date)) %>% 
      arrange(pays,date)
    
    # calcul du nombre total de deces
    global$deces<-global$covid %>% 
      filter(!is.na(codepays)) %>%       
      group_by(codepays,pays) %>% 
      summarise(total_deces=sum(morts)) %>% 
      arrange(desc(total_deces)) %>% 
      ungroup() %>% 
      mutate(covid19=case_when(
                        total_deces > 0 ~ "1",
                        TRUE            ~ "0"),
             classe_covid=cut(total_deces, 
                              breaks=c(-Inf,0, 50, 500,5000,10000,50000),
                              include.lowest = FALSE,
                              right = TRUE,
                              labels = c("Pas de cas","<50","<500","<5000","<10000",">10000")))
    
    # selection des pays avec plus de 100 deces
    global$top100<-global$deces %>% 
      filter(total_deces>=100) %>% 
      arrange(desc(total_deces))
    
    # recuperation de la liste de tous les pays avec plus de 100 deces + tous_les_pays 
    global$ordre<- global$top100 %>% pull(pays)
    global$ordre_avec_tous<-c("tous_les_pays",global$ordre)
    
    # pour cacher l apercu tant que les donnees ne sont pas chargees
    shinyjs::show(id="box_apercu")
    
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
         alt="Capture d'écran du site européen")
  },deleteFile = FALSE)
  
  callModule(mod_apercu_server, "apercu_ui_1",r=global)  
  
  callModule(mod_histogramme_server, "histogramme_ui_1",r=global)
  
  callModule(mod_progression_server, "progression_ui_1",r=global)
  
  callModule(mod_progressionavectout_server, "progressionavectout_ui_1",r=global)
  
  callModule(mod_carte_server, "carte_ui_1",r=global)
  
  callModule(mod_bulles_server, "bulles_ui_1",r=global)

}
