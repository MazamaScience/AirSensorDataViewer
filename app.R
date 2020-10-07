# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
base::options( 
  golem.app.prod = TRUE, 
  shiny.port = 3838,
  shiny.host = '0.0.0.0' 
)
AirSensorDataViewer::run_app() # add parameters here (if any)
