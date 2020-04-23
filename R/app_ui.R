#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody dashboardPage sidebarMenu menuItem infoBox tabItem tabItems box
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
app_ui <- function(request) {
    header <- dashboardHeader(
    title = "Suivi COVID19" , 
    titleWidth = 230
  )
  sidebar<-dashboardSidebar(
    width=230,
    sidebarMenu(
      menuItem("Acceuil", tabName = "accueil", icon = icon("home")),
      menuItem("Chargement des donnees", tabName = "chargement",icon = icon("cloud-download-alt"), 
               badgeLabel = "A faire en 1er!", badgeColor = "red"),
      menuItem("Les plus touches", tabName = "bulles", icon = icon("spinner")),
      menuItem("Histogramme", tabName = "histo", icon = icon("chart-bar")),
      menuItem("Progression", tabName = "prog", icon = icon("chart-line")),
      menuItem("Progression Interactive", tabName = "prog_int", icon = icon("hand-pointer")),
      menuItem("Carte Mondiale", tabName = "carte", icon = icon("globe-africa")),
      menuItem("Trucs", tabName = "trucs", icon = icon("question-circle")),
      menuItem("Site web de l'equipe", icon = icon("users"), href = "http://joliot.cea.fr/drf/joliot/Pages/Entites_de_recherche/medicaments_technologies_sante/SPI/uiaa.aspx")
    )
  )
  body<-dashboardBody(
    useShinyjs(), 
    tabItems(
      # First tab content
      tabItem(tabName="accueil",
              fluidRow(
                column(width=12, style="margin-left:4%; margin-right:4%",
                       box(width=NULL,solidHeader=TRUE,
                           h2("Page d'acceuil"),
                           hr(),
                           p("Bienvenue sur cette appli Shiny test qui permet de visualiser la progression de la pandémie ",strong("COVID19"),
                             " dans les différents pays à partir des données publiques disponibles sur le site européen ", 
                             a(href = 'https://www.ecdc.europa.eu', 'https://www.ecdc.europa.eu', .noWS = "outside"),
                             ".",
                             .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                           br(),
                           tags$a(imageOutput("site_europeen"),href = 'https://www.ecdc.europa.eu', target="_blank"),
                           p("(vous pouvez cliquer sur l'image pour vous rendre sur la page dont sont issues les données).")
                       )
                )
              )
      ),
      tabItem(tabName="chargement",
              fluidRow(
                column(12,
                       box(width=NULL,solidHeader=TRUE,
                         h2("Chargement des donnees"),
                         hr(),
                         p("Cette application Shiny test utilise les données publiques disponibles sur le site européen: https://www.ecdc.europa.eu.",
                           tags$br(),
                           tags$br(),
                           "Le chargement des données pourrait être automatique mais cela permet de tester l'",strong("action Button")," ;-)",
                           .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                         hr(),
                         p("Pour obtenir la dernière version des données (de la veille), cliquer sur le bouton suivant:",
                           .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                         actionButton(inputId = "chgt", 
                                      label="Chargement des données à partir du site https://www.ecdc.europa.eu", 
                                      icon = icon("cloud-download-alt", "fa-2x"),
                                      style='padding:12px; font-size:150%;color: #fff; background-color: #337ab7; border-color: #2e6da4'
                         ),
                         tags$br(),
                         hr(),
                         shinyjs::hidden(
                           div(id="box_apercu",
                             box(
                              title=span(icon("glasses"),"Aperçu des données chargées:",style = "font-weight: bold;color: white"),
                              footer = strong("... ce n'est qu'un extrait..."),
                              status = "info",
                              solidHeader = TRUE,
                              width=NULL,
                              tableOutput("apercu")
                             )
                           )
                         )
                       )
                )
              )
      ),
      tabItem(tabName="bulles",
              fluidRow(
                column(12,height=1000,
                  box(width=NULL,height = 900,solidHeader=TRUE,
                    mod_bulles_ui("bulles_ui_1")
                  )
                )
              )
      ),
      tabItem(tabName="histo",
              fluidRow(
                column(12,height=1000,
                  box(width = NULL,height = 900,solidHeader=TRUE,
                    mod_histogramme_ui("histogramme_ui_1")
                  )
                )
              )
      ),
      tabItem(tabName="prog",
              fluidRow(
                column(12,height=1000,
                       box(width = NULL,height = 900,solidHeader=TRUE,
                           mod_progression_ui("progression_ui_1")
                  )
                )
              )
      ),        
      tabItem(tabName="prog_int",
              fluidRow(
                column(12,height=1000,
                       box(width = NULL,height = 900,solidHeader=TRUE,
                           mod_progressionavectout_ui("progressionavectout_ui_1")
                  )
                )
              )
      ), 
      tabItem(tabName="carte",
              fluidRow(
                column(12,height=1000,
                       box(width = NULL,height = 900,solidHeader=TRUE,
                           mod_carte_ui("carte_ui_1")
                       )
               )
              )
      ), 
      tabItem(tabName="trucs",
              box(h2("Trucs pour l'utilisation de Shiny et Dashboard"),
                  hr(),
                  h4("Site pour la liste des icones disponibles: "),
                  tags$a(href="https://fontawesome.com/icons?from=io", "https://fontawesome.com/icons?from=io")
              )
      )
    )
  )
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # realisation de la page
    dashboardPage(header,sidebar,body)
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

