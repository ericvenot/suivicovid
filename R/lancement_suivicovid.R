
# probleme car par d exportation comme run_app

lancement_suivicovid <- function() {
  appDir <- system.file("R", package = "suivicovid")  
  shiny::runApp(appDir, display.mode = "normal")}