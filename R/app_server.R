#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr %>% filter group_by mutate select rename ungroup summarise arrange desc pull case_when
#' @importFrom httr GET write_disk authenticate
#' @importFrom xlsx read.xlsx
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_text theme_minimal scale_x_discrete scale_x_date labs theme element_text
#' @importFrom scales breaks_width
#' @importFrom utils head
#' @importFrom rworldmap joinCountryData2Map mapCountryData
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
    covid1 <- read.xlsx(tf,sheetIndex = 1)
    
    global$covid <- covid1 %>% 
      rename(codepays=countryterritoryCode,pays=countriesAndTerritories, date=dateRep) %>%
      select(codepays,pays,date,deaths)
    
    global$total_par_jour<-global$covid %>%
      group_by(date) %>% 
      mutate(total=sum(deaths)) %>% 
      ungroup()
    
    # calcul du nombre total de deces
    global$deces<-global$covid %>% 
      filter(!is.na(codepays)) %>%       
      group_by(codepays,pays) %>% 
      summarise(total_deces=sum(deaths)) %>% 
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
    
    global$ordre<- global$top100 %>% pull(pays)
  })
  

  output$deces <- renderTable({
    global$top100
  })
  callModule(mod_apercu_server, "apercu_ui_1",r=global)  
  
  callModule(mod_histogramme_server, "histogramme_ui_1",r=global)
  
  callModule(mod_progression_server, "progression_ui_1",r=global)
  
  callModule(mod_progressionavectout_server, "progressionavectout_ui_1",r=global)
  
  callModule(mod_carte_server, "carte_ui_1",r=global)

}
