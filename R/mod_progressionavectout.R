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
    plotlyOutput(ns("progressionavectout"))
  )
}

#' progressionavectout Server Function
#'
#' @noRd 
mod_progressionavectout_server <- function(input, output, session,r){
  ns <- session$ns
  output$progressionavectout <- renderPlotly({
    req(r$total_par_jour)
    r$total_par_jour %>% 
      filter(pays %in% r$ordre_avec_tous) %>% 
      ggplot()+
      aes(x=date,y=deaths,group=pays)+
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
    ggplotly(p = ggplot2::last_plot(),dynamicTicks = TRUE) %>%
      rangeslider()
  }) 
}
    
## To be copied in the UI
# mod_progressionavectout_ui("progressionavectout_ui_1")
    
## To be copied in the server
# callModule(mod_progressionavectout_server, "progressionavectout_ui_1")
 
