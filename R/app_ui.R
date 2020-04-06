#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Construction de la page d accueil
    fluidPage(
      
      # pour des titres plus beaux (il faut que le fichier soit dans le repertoire www)
      theme = "bootstrap.css",
      
      # titre avec mise a jour de la date
      h1("Suivi de la progression mondiale du COVID 19"),
      hr(),
      h2("Eric VENOT"),
      h2(format(Sys.time(), "%d %B %Y")),
      
      # definition de la page Utilisateur
      sidebarLayout(
        sidebarPanel(
          actionButton(inputId = "chgt", label="Chargement du fichier")
        ), 
        mainPanel( 
          tabsetPanel(
            tabPanel("Apercu de la table chargee",
                     mod_apercu_ui("apercu_ui_1")
            ),
            tabPanel("Total Deces",
                     tableOutput("deces")
            ),
            tabPanel("Histogramme",
                     mod_histogramme_ui("histogramme_ui_1")
            ),
            tabPanel("Progression",
                     mod_progression_ui("progression_ui_1")
            ),
            tabPanel("Progression avec total deces",
                     mod_progressionavectout_ui("progressionavectout_ui_1")
            ),
            tabPanel("Atlas",
                     mod_carte_ui("carte_ui_1")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'suivicovid'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

