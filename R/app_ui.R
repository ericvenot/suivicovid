#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody dashboardPage sidebarMenu menuItem infoBox tabItem tabItems box valueBox valueBoxOutput 
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom lubridate day month year mdy 
#' @noRd
app_ui <- function(request) {
  title <- tags$a(href = 'https://ericvenot.shinyapps.io/suivicovid',
                  "Suivi COVID",style = "font-size:20px;font-weight: bold;color: white")
  header <- dashboardHeader(
              title=title,
              titleWidth = 230
  )
  sidebar<-dashboardSidebar(
    width=230,
    sidebarMenu(
      menuItem("Acceuil", tabName = "accueil", icon = icon("home")),
      menuItem("Chargement des donn\u00E9es", tabName = "chargement",icon = icon("cloud-download-alt"), 
               badgeLabel = "A faire en 1er!", badgeColor = "red"),
      menuItem("Tableau de bord", tabName = "tableau_bord", icon = icon("tachometer-alt")),
      sliderInput(inputId = "choix_date",
                  label = "Choix des dates \u00E0 consid\u00E9rer:",
                  min = as.Date("01/01/2020","%d/%m/%Y"),
                  max = as.Date(Sys.Date()-1,"%d/%m/%Y"),
                  value=c(as.Date("01/01/2020"),as.Date(Sys.Date()-1)),
                  timeFormat="%d/%m/%Y"),
      menuItem("Taux de croissance", tabName = "croissance", icon = icon("bell")),
      menuItem("Les plus touch\u00E9s", tabName = "bulles", icon = icon("spinner")),
      menuItem("Nuage de pays", tabName = "nuage", icon = icon("cloud")),
      menuItem("Histogramme", tabName = "histo", icon = icon("chart-bar")),
      menuItem("Progression", tabName = "prog", icon = icon("chart-line")),
      menuItem("Progression Interactive", tabName = "prog_int", icon = icon("hand-pointer")),
      menuItem("Carte Mondiale", tabName = "carte", icon = icon("globe-africa")),
      menuItem("Trucs", tabName = "trucs", icon = icon("question-circle")),
      menuItem("Site web de l'\u00E9quipe", icon = icon("users"), href = "http://joliot.cea.fr/drf/joliot/Pages/Entites_de_recherche/medicaments_technologies_sante/SPI/uiaa.aspx"),
      tags$img(imageOutput("logo_suivicovid",height=100,width=100))
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
                           h2(strong('Bienvenue sur ma premi\u00E8re Appli SHINY'),
                              tags$br(),
                              " (suite \u00E0 la formation ThinkR ("
                              ,tags$a(href = 'https://thinkr.fr', 'https://thinkr.fr', .noWS = "outside"),
                              "))"),
                           hr(),
                           p("Cette ",strong("appli Shiny"), " test permet de visualiser la progression de la pand\u00E9mie ",strong("COVID19"),
                             " dans les diff\u00E9rents pays \u00E0 partir des donn\u00E9es publiques disponibles sur le site europ\u00E9en ", 
                             a(href = 'https://www.ecdc.europa.eu', 'https://www.ecdc.europa.eu', .noWS = "outside"),
                             ".",
                             tags$br(),
                             tags$br(),
                             strong("Derni\u00E8re mise \u00E0 jour:")," ajout du suivi du taux de croissance.",                             
                             tags$br(),
                             tags$br(),
                             "Le code qui a permis de g\u00E9n\u00E9rer cette appli est disponible sur ",strong("github"),": ",
                             a(href = 'https://github.com/ericvenot/suivicovid', 'https://github.com/ericvenot/suivicovid', .noWS = "outside"),
                             tags$br(),
                             tags$br(),
                             strong("Pour l'utiliser,")," il faut tout d'abord ",strong("charger les donn\u00E9es")," dans l'onglet ",strong("Chargement des donn\u00E9s !"),
                             .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                           br(),
                           tags$a(imageOutput("site_europeen"),href = 'https://www.ecdc.europa.eu', target="_blank"),
                           p("(vous pouvez cliquer sur l'image pour vous rendre sur la page dont sont issues les donn\u00E9es).")
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
                         p("Cette application Shiny test utilise les donn\u00E9es publiques disponibles sur le site europ\u00E9en: https://www.ecdc.europa.eu.",
                           tags$br(),
                           tags$br(),
                           "Le chargement des donn\u00E9es pourrait \u00EAtre automatique mais cela permet de tester l'",strong("action Button")," ;-)",
                           .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                         hr(),
                         p("Pour obtenir la derni\u00E8re version des donn\u00E9es ",strong("(donn\u00E9es de la veille)"),", cliquez sur le bouton suivant:",
                           .noWS = c("after-begin", "before-end"), style = "font-size:14px"),
                         actionButton(inputId = "chgt", 
                                      label="Chargement des donn\u00E9es \u00E0 partir du site https://www.ecdc.europa.eu", 
                                      icon = icon("cloud-download-alt", "fa-2x"),
                                      style='padding:12px; font-size:150%;color: #fff; background-color: #337ab7; border-color: #2e6da4'
                         ),
                         tags$br(),
                         hr(),
                         shinyjs::hidden(
                           div(id="box_apercu",
                             box(
                              title=span(icon("glasses"),"Aper\u00E7u des donn\u00E9es charg\u00E9es:",style = "font-weight: bold;color: white"),
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
      tabItem(tabName="tableau_bord",
              fluidRow(
                column(12,
                       valueBoxOutput("box_nouveauxcas"),
                       valueBoxOutput("box_nbpays100")
                )
              ),
              fluidRow(
                column(12,
                       valueBoxOutput("box_mortstot"),
                       valueBoxOutput("box_pirepays")
                )
              ),
              fluidRow(
                column(12,
                       valueBoxOutput("box_piretaux1"),
                       valueBoxOutput("box_piretaux2")
                       
                )
              )
              
      ),
      tabItem(tabName="croissance",
              fluidRow(
                column(12,height=1000,
                       box(width = NULL,height = 900,solidHeader=TRUE,
                           mod_croissance_ui("croissance_ui_1")
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
      tabItem(tabName="nuage",
              fluidRow(
                column(12,height=1000,
                       box(width=NULL,height = 900,solidHeader=TRUE,
                           mod_nuage_ui("nuage_ui_1")
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
              box(width = NULL,height = 600,solidHeader=TRUE,
                  h2(strong("Trucs pour l'utilisation de Shiny et Dashboard")),
                  hr(),
                  h3(strong("Le tuto sur l'application GOLEM de ThinkR: "),
                     tags$a(href="https://rtask.thinkr.fr/fr/demarrer-avec-golem", "https://rtask.thinkr.fr/fr/demarrer-avec-golem", target="_blank")),
                  br(),
                  h3(strong("Les tutos sur Shiny par Rstudio: "),
                     tags$br(),
                     tags$a(href="https://shiny.rstudio.com/tutorial/", "https://shiny.rstudio.com/tutorial/", target="_blank"),
                     tags$br(),
                     tags$a(href="https://rstudio.github.io/shinydashboard/index.html/", "https://rstudio.github.io/shinydashboard/index.html/", target="_blank")),
                  br(),
                  h3(strong("Cours sur Shiny avec Dashboard: "),
                     tags$a(href="https://curso-r.github.io/my-first-dashboard-with-shiny-csds2019/#1", "https://curso-r.github.io/my-first-dashboard-with-shiny-csds2019/#1", target="_blank")),
                  br(),
                  h3(strong("Site pour la liste des icones disponibles: "),
                     tags$a(href="https://fontawesome.com/icons?from=io", "https://fontawesome.com/icons?from=io", target="_blank")),
                  br(),
                  h3(strong("La meme chose fait par Dr Rehan Zafar avec Flexdashboard (tuto sous YouTube): "),
                     tags$a(href="https://www.youtube.com/c/DrRehanZafar", "https://www.youtube.com/c/DrRehanZafar", target="_blank")),
                  br(),
                  h3(strong("La meme idee sur COVID 19 prepraree pour le ShinyContest 2020: "),
                     tags$a(href="https://www.r-bloggers.com/covid-19-shiny-plotly-dashboard/", "https://www.r-bloggers.com/covid-19-shiny-plotly-dashboard/", target="_blank")),
                  br(),
                  h3(strong("Le graal absolu (vainqueur shinyContest 2019: "),
                     tags$a(href="https://scotland.shinyapps.io/ScotPHO_profiles_tool/", "https://scotland.shinyapps.io/ScotPHO_profiles_tool/", target="_blank"))
                  
              )
      )
    )
  )
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # realisation de la page
    dashboardPage(header,sidebar,body),
    tags$footer(column(4, "\u00a9 Eric VENOT - Test Appli SHINY avec Dashboard (Avril 2020)"), 
                column(2, tags$a(href="mailto:eric.venot@inrae.fr", tags$b("Contactez-moi si souci!"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                column(2, tags$a(href="http://joliot.cea.fr/drf/joliot/Pages/Entites_de_recherche/medicaments_technologies_sante/spi.aspx", 
                                 tags$b("Lien vers le SPI"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                column(2, tags$a(href="http://joliot.cea.fr/drf/joliot/Pages/Entites_de_recherche/medicaments_technologies_sante/SPI/uiaa.aspx", 
                                 tags$b("Lien vers le laboratoire LIAA"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                style = "position:fixed;
                         text-align:center;
                         left: 0;
                         bottom:0;
                         width:100%;
                         z-index:1000;  
                         height:30px; /* Height of the footer */
                         color: white;
                         padding: 10px;
                         font-weight: bold;
                         background-color: #1995dc"
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

