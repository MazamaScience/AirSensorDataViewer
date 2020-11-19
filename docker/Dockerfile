FROM mazamascience/airsensor-dataviewer-base:1.0.4
MAINTAINER Hans Martin <hans@mazamascience.com>

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


