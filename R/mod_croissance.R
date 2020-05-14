#' croissance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_croissance_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Taux de croissance du nombre de d\u00E9c\u00E8s dans les pays avec plus de 1000 d\u00E9c\u00E8s cumul\u00E9s"),
    h3("   (taux de croissance d'un pays pour un jour donn\u00E9 = nombre de nouveaux d\u00E9c\u00E8s / nombre total de d\u00E9c\u00E8s )"),
    br(),
    p(" Ce graphique est ",strong("interactif!")," Vous pouvez : ",
      tags$br(),
      " - ", strong("retirer")," un (ou plusieurs) pays en cliquant sur ce pays dans la l\u00E9gende,",
      tags$br(),
      " - ", strong("ne s\u00E9lectionner qu'un (ou plusieurs) pays")," en double cliquant sur ce pays dans la l\u00E9gende,",
      tags$br(),
      " - ", strong("zoomer")," en s\u00E9lectionnant une partie du graphique.",
      .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
    br(),
    plotlyOutput(ns("croissance"),height=700),
    h2("(Rem: package ggplotly)")
  )
}
    
#' croissance Server Function
#'
#' @noRd 
mod_croissance_server <- function(input, output, session,r){
  ns <- session$ns
  output$croissance <- renderPlotly({
    req(r$total_par_jour)
    
    p<-r$total_par_jour %>% 
      arrange(pays,date) %>%
      group_by(pays) %>% 
      mutate(cumul=cumsum(morts)) %>% 
      select(pays,date,cumul,morts) %>% 
      arrange(pays,date) %>% 
      mutate(jour=dplyr::row_number(),
             prec=dplyr::lag(cumul,order_by=pays)) %>% 
      mutate(taux=morts/cumul*100) %>%
      filter(cumul>=1000) %>% 
      ggplot()+
      aes(x=date,y=taux,color=pays)+
      theme_minimal()+
      geom_point(size=0.2)+
      geom_smooth(method="loess",se=FALSE)+
      labs(title=(paste0("Taux de croissance pour les pays avec plus de 1000 d\u00E9c\u00E8s cumul\u00E9s")),
           x="Date",y="Taux de croissance (en %)"
      )+
      theme(legend.position="bottom",
            plot.title = element_text(hjust=0.5),
            axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold")
      )
    ggplotly(p)
  })
}
    
## To be copied in the UI
# mod_croissance_ui("croissance_ui_1")
    
## To be copied in the server
# callModule(mod_croissance_server, "croissance_ui_1")
 
