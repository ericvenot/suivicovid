#' progressionavectout UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly ggplotly rangeslider renderPlotly plotlyOutput
mod_progressionavectout_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Progression du nombre de d\u00E9c\u00E8s journaliers pour les pays avec plus de 100 d\u00E9c\u00E8s"),
    br(),
    p(" Ce graphique est ",strong("interactif!")," Vous pouvez : ",
      tags$br(),
      " - ", strong("retirer")," un (ou plusieurs) pays en cliquant sur ce pays dans la l\u00E9gende,",
      tags$br(),
      " - ", strong("ne s\u00E9lectionner qu'un des pays")," en double cliquant sur ce pays dans la l\u00E9gende,",
      tags$br(),
      " - ", strong("s\u00E9lectionner une partie des dates")," en faisant glisser les barres verticales de l apercu en cas.",
      .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
    br(),
    plotlyOutput(ns("progressionavectout"),height=700),
    h2("(Rem: package ggplotly)")
  )
}

#' progressionavectout Server Function
#'
#' @noRd 
mod_progressionavectout_server <- function(input, output, session,r){
  ns <- session$ns
  output$progressionavectout <- renderPlotly({
    req(r$total_par_jour)
    p<-r$total_par_jour %>% 
        filter(pays %in% r$ordre_avec_tous) %>% 
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
    ggplotly(p,dynamicTicks = TRUE) %>%
      rangeslider() 
  }) 
}
    
## To be copied in the UI
# mod_progressionavectout_ui("progressionavectout_ui_1")
    
## To be copied in the server
# callModule(mod_progressionavectout_server, "progressionavectout_ui_1")
 
