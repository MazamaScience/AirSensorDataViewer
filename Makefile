################################################################################
# Makefile for building and running docker containers for AirSensor DataViewer
#
# On joule, ProxypPass settings are defined in:
#
#   /etc/httpd/conf.d/tools.mazamascience.com.conf
#
# Proxying instructions from:
#
#   https://support.rstudio.com/hc/en-us/articles/213733868-Running-Shiny-Server-with-a-Proxy
# 
# Background here:
# 
#   https://www.linuxjournal.com/content/integrating-web-applications-apache
#
# Note that we are proxying from the port exposed in the Dockerfile.
# 
# 6700-6709 airsensor ---------------------------------------------------------
# # 6701 -- v1 operational
# # 6709 -- test
#  <Proxy *>
#    Allow from localhost
#  </Proxy>
#
# RewriteEngine on
# RewriteCond %{HTTP:Upgrade} =websocket
# RewriteRule /airsensor-test/(.*) ws://localhost:6709/$1 [P,L]
# RewriteCond %{HTTP:Upgrade} !=websocket
# RewriteRule /airsensor-test/(.*) http://localhost:6709/$1 [P,L]
# ProxyPass /airsensor-test/ http://localhost:6709/
# ProxyPassReverse /airsensor-test/ http://localhost:6709/
#
# Header edit Location ^/ /airsensor-test/
# ProxyRequests Off
#
#
# Test these settings on CentOS with:    "sudo apachectl configtest"
# Reload these settings on CentOS with:  "sudo apachectl graceful"
#

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile
SERVICE_PATH=airsensor-dataviewer/v1
SERVICE_PATH_TEST=airsensor-dataviewer/test

# Assign version from golem_config.yml configuration
VERSION=`grep golem_version inst/golem-config.yml | cut -d':' -f2 | sed 's/ //g'`

show_version:
	echo $(VERSION)
	
# App configuration
clean:
	if [ -d logs ]; then sudo rm -Rf logs; mkdir logs; fi

# NOTE:  You need to manually edit these version numbers and that in docker/Dockerfile to match
base_build:
	cd docker; docker build --no-cache -t mazamascience/airsensor-dataviewer-base:1.0.5 -f Dockerfile-base .

base_publish:
	cd docker; docker login && docker push mazamascience/airsensor-dataviewer-base:1.0.5

# AirSensorShiny DESKTOP version -----------------------------------------------

# NOTE:  make the appropriate configure_app target first

desktop_build:
	-mkdir logs
	docker build -t airsensor-dataviewer-desktop:$(VERSION) \
		-t airsensor-dataviewer-desktop:latest -f docker/Dockerfile .

jon_up:
	docker-compose -f docker/docker-compose-jon.yml \
		-p airsensordataviewerdesktop up -d
desktop_up:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensordataviewerdesktop up -d

desktop_down:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensordataviewerdesktop down

desktop_container_logs:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensordataviewerdesktop logs -f

desktop_bounce: desktop_down desktop_up

desktop_reboot: desktop_build desktop_bounce


# AirSensordataviewer TEST version --------------------------------------------------

test_build: 
	sed -i 's%location\/.*\/ {%location\/$(SERVICE_PATH_TEST)\/ {%' shiny-server.conf
	###-mkdir airsensordataviewer/test
	docker build -t airsensor-dataviewer-test:$(VERSION) \
		-t airsensor-dataviewer-test:latest -f docker/Dockerfile .

test_build_osx: 
	sed -i '' 's%location\/.*\/ {%location\/$(SERVICE_PATH_TEST)\/ {%' shiny-server.conf
	###-mkdir airsensordataviewer/test
	docker build -t airsensor-dataviewer-test:$(VERSION) \
		-t airsensor-dataviewer-test:latest -f docker/Dockerfile .

test_up:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensordataviewertest up -d

test_down:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensordataviewertest down

test_container_logs:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensordataviewertest logs

test_trace_log:
	ls -t /var/www/html/logs/$(SERVICE_PATH_TEST)/app/*/TRACE.log | head -1 | xargs cat

test_debug_log:
	ls -t /var/www/html/logs/$(SERVICE_PATH_TEST)/app/*/DEBUG.log | head -1 | xargs cat

test_info_log:
	ls -t /var/www/html/logs/$(SERVICE_PATH_TEST)/app/*/INFO.log | head -1 | xargs cat

test_error_log:
	ls -t /var/www/html/logs/$(SERVICE_PATH_TEST)/app/*/ERROR.log | head -1 | xargs cat

test_bounce: test_down test_up

test_reboot: test_build test_bounce

# AirSensordataviewer JOULE version --------------------------------------------------

joule_test_build:
	sed -i 's%location\/.*\/ {%location\/$(SERVICE_PATH)\/ {%' shiny-server.conf
	###-mkdir airsensordataviewer/v1
	docker build -t airsensor-dataviewer-test:$(VERSION) \
		-t airsensor-dataviewer-test:latest -f docker/Dockerfile .

joule_test_up:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensordataviewertest up -d

joule_test_down:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensordataviewertest down

joule_test_container_logs:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensordataviewertest logs

joule_test_trace_log:
	ls -t /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH)/app/*/TRACE.log | head -1 | xargs cat

joule_test_debug_log:
	ls -t /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH)/app/*/DEBUG.log | head -1 | xargs cat

joule_test_info_log:
	ls -t /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH)/app/*/INFO.log | head -1 | xargs cat

joule_test_error_log:
	ls -t /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH)/app/*/ERROR.log | head -1 | xargs cat

joule_test_bounce: joule_test_down joule_test_up

joule_test_reboot: joule_test_build joule_test_bounce

# AirSensordataviewer DOCKER CORE ---------------------------------------------------

airsensordataviewer_build:
	docker build -t mazamascience/airsensordataviewer:$(VERSION) -f docker/Dockerfile .
