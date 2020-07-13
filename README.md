# AirSensor DataViewer

Welcome to the AirSesnor DataViewer repository!

AirSensor DataViewer intends to provide an interface for visualizing displaying time series PurpleAir air sensor data supported by the AirSensor R Package and Mazama Science. 

## Installation

From GitHub:

`install.packages("remotes")`

`remotes::install_github("MazamaScience/AirSensorDataViewer")`

## Running from source

### Desktop

`AirSensorDataViewer::run_app()`

### Docker (WIP)

`make airsensordataviewer_build`

`make desktop_build`

`make desktop up`