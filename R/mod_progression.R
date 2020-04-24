#' progression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom lubridate date
mod_progression_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h2("Progression du nombre de d\u00E9c\u00E8s journaliers pour les pays avec plus de 100 d\u00E9c\u00E8s"),
      plotOutput(ns("progression"),height = 800),
      h2("(Rem: package ggplot)")
    )
  )
}
    
#' progression Server Function
#'
#' @noRd 
mod_progression_server <- function(input, output, session,r){
  ns <- session$ns
  
  observe({
    output$progression <- renderPlot({
      req(r$covid)
      r$covid %>% 
        filter(pays %in% r$ordre) %>% 
        mutate(date=date(x = date)) %>% 
        ggplot()+
        aes(x=date,y=morts,group=pays)+
        theme_minimal()+
        geom_line(aes(color=pays),size=1)+
        labs(title=(paste0("Nombre de morts par jour (mise a jour du ",r$hier,")")),
             x="Pays",y="Nombre de deces"
        )+
        theme(legend.position="bottom",
              plot.title = element_text(hjust=0.5),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold")
        )+
        scale_x_date(NULL,
                     breaks = scales::breaks_width("5 days"), 
                     labels = scales::label_date_short()
        )
    })
  }) 
}
    
## To be copied in the UI
# mod_progression_ui("progression_ui_1")
    
## To be copied in the server
# callModule(mod_progression_server, "progression_ui_1")
 
