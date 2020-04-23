#' nuage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2 wordcloud2Output

mod_nuage_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Présentation des pays avec plus de 1000 décès en WORDCLOUD"),
    wordcloud2Output(ns("nuage")),
    h2("(Rem: package wordcloud2)") 
  )
}
    
#' nuage Server Function
#'
#' @noRd 
mod_nuage_server <- function(input, output, session,r){
  ns <- session$ns
 
  output$nuage<-renderWordcloud2({
    req(r$deces)
    # preparation de la table pour le nuage de mots
    data_wc<- r$deces %>%
      select(pays, total_deces) %>%
      filter(total_deces>100) %>% 
      mutate(pays=case_when(
        pays == "United_States_of_America" ~ "USA",
        pays == "United_Kingdom" ~ "UK",
        pays == "Democratic_Republic_of_the_Congo" ~ "RDC",
        TRUE ~ paste0(pays))) %>% 
      rename(word=pays,freq=total_deces) %>% 
      arrange(desc(freq))
    
    wordcloud2(data_wc, size = 1, minSize = 0, gridSize =  0,
               color = brewer.pal(8, "Dark2"), shape="circle", rotateRatio = 0.4,
               minRotation = -pi/5, maxRotation = -pi/5)
  })
}
    
## To be copied in the UI
# mod_nuage_ui("nuage_ui_1")
    
## To be copied in the server
# callModule(mod_nuage_server, "nuage_ui_1")
 
