#' carte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom rgdal readOGR
#' @importFrom leaflet leaflet setView addTiles colorFactor addPopups addPolygons addLegend popupOptions highlightOptions colorNumeric leafletOutput renderLeaflet

mod_carte_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("carte"))
  )
}
    
#' carte Server Function
#'
#' @noRd 
mod_carte_server <- function(input, output, session,r){
  ns <- session$ns
  content <- paste(sep = "<br/>",
                   "<b><a href='http://joliot.cea.fr/drf/joliot/Pages/Entites_de_recherche/medicaments_technologies_sante/SPI/uiaa.aspx'>LIAA</a></b>",
                   "SPI - MTS",
                   "CEA Saclay")
  world<-rgdal::readOGR("inst/app/www/world.geo.json")
  
  # ajout des variables total_deces et des categories
  observe({
    req(r$deces)
    world$covid<-(r$deces$total_deces[match(world$iso_a3,r$deces$codepays)])
    world$classe_covid<-(r$deces$classe_covid[match(world$iso_a3,r$deces$codepays)])
  
    # definition de la palette de couleurs
    pal <- colorFactor("YlOrRd", domain = r$deces$classe_covid, na.color = "transparent")
    pal <- colorFactor(c("white","yellow","orange","orangered","red4","black"), domain = r$deces$classe_covid, na.color = "transparent")
  
  
    # definition du label qui apparait sur le pays
    world$labels <- paste0("<strong> Country: </strong> ",
                           world$name, "<br/>",
                           "<strong> Nombre de décès: </strong> ",
                           world$covid, "<br/>"
                    ) %>% 
                    lapply(htmltools::HTML)
  
    # creation de la carte interactive
    output$carte <- renderLeaflet({  
      leaflet(world) %>%
        setView(lat=48.721436, lng=2.143060, zoom = 3) %>% 
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addPopups(lat=48.721436, lng=2.143060, content, options = popupOptions(closeButton = FALSE)) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                    fillColor = ~pal(world$classe_covid), label=~labels,
                    highlight=highlightOptions(
                      color="black",
                      bringToFront=TRUE
                    )) %>% 
        addLegend(pal = pal, values = ~world$classe_covid, opacity = 1.0,
                  title=paste0("Nombre de décès COVID19 le ",format(Sys.time(), "%d %B %Y")))   
    })
  })
}    
## To be copied in the UI
# mod_carte_ui("carte_ui_1")
    
## To be copied in the server
# callModule(mod_carte_server, "carte_ui_1")
 
