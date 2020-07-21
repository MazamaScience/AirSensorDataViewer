FROM rocker/r-ver:3.6.3
RUN apt-get update
RUN apt-get install -y  default-jre-headless
RUN apt-get install -y  gdal-bin
RUN apt-get install -y  git-core
RUN apt-get install -y  libcurl4-openssl-dev
RUN apt-get install -y  libgdal-dev
RUN apt-get install -y  libgeos-dev libgeos++-dev
RUN apt-get install -y  libgit2-dev
RUN apt-get install -y  libjq-dev
RUN apt-get install -y  libpng-dev
RUN apt-get install -y  libprotobuf-dev
RUN apt-get install -y  libssh2-1-dev
RUN apt-get install -y  libssl-dev
RUN apt-get install -y  libudunits2-dev
RUN apt-get install -y  libv8-dev
RUN apt-get install -y  libxml2-dev
RUN apt-get install -y  make
RUN apt-get install -y  pandoc pandoc-citeproc
RUN apt-get install -y  protobuf-compiler libprotoc-dev
RUN apt-get install -y  zlib1g-dev
RUN rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("MazamaCoreUtils",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.7")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.14")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("PWFSLSmoke",upgrade="never", version = "1.2.111")'
RUN Rscript -e 'remotes::install_version("promises",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.18.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.9")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("worldmet",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("gridExtra",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_github("MazamaScience/tiotemp")'
RUN Rscript -e 'remotes::install_github("MazamaScience/AirSensor")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');AirSensorDataViewer::run_app()"
