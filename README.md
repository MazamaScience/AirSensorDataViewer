# AirSensor DataViewer 

Welcome to the AirSensor DataViewer repository! 

AirSensor DataViewer provides an interface web application for visualizing time 
series PurpleAir air sensor data. The package is an extension of the **AirSensor**
R package with support by Mazama Science and the South Coast Air Quality 
Management District. 

## Usage

There are a couple of ways to launch and use AirSensor DataViewer - either 
through R itself, interactively, or hosted with a ready-to-serve docker image for 
server use, which can be created using [this guide](#docker).

### Installation

```
install.packages(“remotes”)
remotes::install_github(“MazamaScience/AirSensorDataViewer”)

AirSensorDataViewer::run_app()
```

Yup, thats it.


### Docker

To create the docker image, navigate to the project root directory and use `make` 
to build the docker image. The docker image should contain all necessary 
libraries and dependencies, as well as being preconfigured for `shiny-server`. 

Create docker image : 

`make airsensordataviewer_build`

Run desktop client: 

`make desktop_build`

`make desktop_up`

By default, the `desktop_*` version will be available at `localhost:8080/asdv/test`. 

To shut the close and exit the docker image:
`make desktop_down`

You can check the logs on the running docker container with:

```
docker exec -ti airsensor-dataviewer-desktop /bin/bash
...
cd /var/log/shiny-server/
ls
```


## Notes 

AirSensor DataViewer is built with Shiny utilizing {golem} framework.  The 
**AirSensor** and **tiotemp** R packages for data processing and visualization. 

