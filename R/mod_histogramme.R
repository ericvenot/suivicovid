#' histogramme UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom ggplot2 ggplot aes geom_bar geom_text theme_minimal scale_x_discrete scale_x_date labs theme element_text
mod_histogramme_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("histogramme"))
  )
}
    
#' histogramme Server Function
#'
#' @noRd 
mod_histogramme_server <- function(input, output, session,r){
  ns <- session$ns

  output$histogramme <- renderPlot({
    req(r$top100)
    r$top100 %>% 
      ggplot()+
      aes(x=pays,y=total_deces)+
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=total_deces), vjust=-0.3, size=3.5)+
      theme_minimal()+
      scale_x_discrete(limits=r$ordre)+
      labs(title=(paste0("Nombre de morts par pays le ",r$hier)),
           x="Pays",y="Nombre de deces")+
      theme(axis.text.x=element_text(angle = 45),
            plot.title = element_text(hjust=0.5),
            axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold"))
  })  
}
    
## To be copied in the UI
# mod_histogramme_ui("histogramme_ui_1")
    
## To be copied in the server
# callModule(mod_histogramme_server, "histogramme_ui_1")
 
