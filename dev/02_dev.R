# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("AirSensor")
usethis::use_package("PWFSLSmoke")
usethis::use_package("promises")
usethis::use_package("future")
usethis::use_package("lubridate")
usethis::use_package("shinyWidgets")
usethis::use_package("shinythemes")
usethis::use_package("rlang")
usethis::use_package("plotly")
usethis::use_package("stringr")
usethis::use_package("worldmet")
usethis::use_package("DT")
usethis::use_package("waiter")
usethis::use_package("gridExtra")
usethis::use_package("tidyr")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "main_panel" ) # Main Panel 
golem::add_module( name = "overview" ) # Overview tabset
golem::add_module( name = "calendar" ) # Calendar tabset 
golem::add_module( name = "raw" ) # Raw data tabset
golem::add_module( name = "patterns")
golem::add_module( name = "compare" )
golem::add_module( name = "video" )

golem::add_module( name = "latest" ) # Latest dv navtab
golem::add_module( name = "datatable" )

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "ggplots" )
golem::add_fct( "plotlys" )
golem::add_fct( "loadNOAA" )
golem::add_fct( "tables" )
golem::add_fct( "load" )
golem::add_fct( "leaflet" )

# Util functions
golem::add_utils("helpers")
golem::add_utils("logs")

## Docker Stuff
# golem::add_dockerfile(
#   output = "docker/Dockerfile2",
#   from = "rocker/r-ver:3.6.3", 
#   port = 3838,
# )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "tiotemp_watcher" )
golem::add_js_handler( "tiotemp_handler" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ----
# usethis::use_vignette("AirSensorDataViewer")
# devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

