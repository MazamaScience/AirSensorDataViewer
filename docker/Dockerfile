# BASE DOCKER
# AirSensor package and AirSensorDataViewer

# R-version 3.6.3
FROM rocker/r-ver:3.6.3

LABEL maintainer="hans@mazamascience.com" \
      maintainer="jon@mazamascience.com"

LABEL build_date="2020-07-07"
LABEL version="0.9.5"

LABEL description="AirSensor DataViewer Docker Image"

# Create directories for AirSensor
RUN mkdir -p /home/mazama/data/Spatial

# Add CARB spatial data required by AirSensor
#RUN wget -nv http://mazamascience.com/RData/Spatial/CA_AirBasins_01.RData \
#  -O /home/mazama/data/Spatial/CA_AirBasins_01.RData

RUN apt-get update && apt-get install -y  default-jre-headless gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libjq-dev libpng-dev libprotobuf-dev libssh2-1-dev libssl-dev libudunits2-dev libv8-dev libxml2-dev make pandoc pandoc-citeproc protobuf-compiler libprotoc-dev zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
#RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1", dependencies = TRUE)'
#RUN Rscript -e 'devtools::install_github("ThinkR-open/golem", ref = "0.2.1")' 
RUN Rscript -e 'install.packages("golem")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.14")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
#RUN Rscript -e 'remotes::install_version("AirSensor",upgrade="never", version = "0.9.5")'
RUN Rscript -e 'remotes::install_version("promises",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.17.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.9")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_github("MazamaScience/AirSensor")'
RUN Rscript -e 'remotes::install_github("MazamaScience/PWFSLSmoke@da92a044be28ecce28b97ed008cdff4a78e67f98")'
RUN Rscript -e 'remotes::install_github("mazamascience/MazamaSpatialUtils@081c441862c6ec56ccbaa0fb1ed7101eb654015b")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

RUN R -e 'remotes::install_local(upgrade = "never")'

EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');AirSensorDataViewer::run_app()"
