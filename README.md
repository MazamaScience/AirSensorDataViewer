# AirSensor DataViewer 

Welcome to the AirSensor DataViewer repository! 
AirSensor DataViewer provides an interface web application for visualizing time series PurpleAir air sensor data. The package is an extension of the AirSensor  R package with support by Mazama Science and the South Coast Air Quality Management District. 

## Usage

To use interactively (un-hosted) download the application as a package via github and use the included function `run_app()` to run via local R session.   

### Installation

`install.packages(“remotes”)`

`remotes::install_github(“MazamaScience/AirSensorDataViewer”)`

### Run via RStudio 

`AirSensorDataViewer::run_app()`

### Docker

To deploy the application on a server with Docker ready to go:

Create docker image : 

`make airsensordataviewer_build`

Run desktop client: 

`make desktop_build`

`make desktop_up`

By default, the application will be forwarded to `0.0.0.0:8000`. 

## Notes 

AirSensor DataViewer is built with Shiny utilizing {golem} framework.  The AirSensor R package functions as the primary backend for data processing and visualization. 
