FROM rocker/shiny-verse:3.6.3
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
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.7")'
RUN Rscript -e 'remotes::install_version("digest",upgrade="never", version = "0.6.25")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("vctrs",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("clipr",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.9")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("gridExtra",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("promises",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("MazamaCoreUtils",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("gt",upgrade="never", version = "0.2.2")'
RUN Rscript -e 'remotes::install_version("bsplus",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.18.0")'
RUN Rscript -e 'remotes::install_github("mazamascience/MazamaSpatialUtils@f5359d1ddd5652b869631ce82409a0c8f90a0fc5")'
RUN Rscript -e 'remotes::install_github("MazamaScience/AirSensor@eef84ddd485515a9ccfb533d1af4cb22c2cb7f46")'
RUN Rscript -e 'remotes::install_github("MazamaScience/tiotemp@8194ae4c78082503dafab4e4ea18d361600f6365")'
RUN Rscript -e 'remotes::install_github("davidcarslaw/worldmet@0d440de4fffbb64c0c2a263c55dc4a5a16a259a3")'
RUN Rscript -e 'remotes::install_github("MazamaScience/PWFSLSmoke@92f7ad31b5408a7542bae2c1a1db84e0131d0f03")'
RUN Rscript -e 'remotes::install_github("ThinkR-open/golem@bf9d0411e337d80d878ed62168360f920668acc2")'

# Create the build zone, copy the local directory over to the docker image, build and install R package.
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'

# Remove sample apps
RUN rm -rf /srv/shiny-server/

# copy app to image
COPY . /srv/shiny-server/asdv

# add .conf file to image/container to preserve log file
COPY ./shiny-server.conf  /etc/shiny-server/shiny-server.conf


# When run image and create a container, this container will listen on port 3838
EXPOSE 3838

# Avoiding running as root --> run container as user instead
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
RUN chmod -R 755 /srv/shiny-server/asdv

# execute in the following as user --> imortant to give permission before that step
USER shiny

##run app
CMD ["/usr/bin/shiny-server.sh"]


