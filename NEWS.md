# AirSensorDataViewer 1.0.2

* Additional logging.
* Raw plot timezone now set to local time.
* Various package version updates.
* Removed barchart show/hide.
* Return to use of **worldmet** package for wind data.
* Video URLs now point to http://data.mazamascienc.com/PurpleAir/v1/videos
* Added calendar legend.
* Default timezone now set in `app_server.R`.
* Reduced usage of waiter spinner.

# AirSensorDataViewer 1.0.1

* Additional logging.

# AirSensorDataViewer 1.0.0

* Release Candidate 1.0.0!

# AiSensorDataViewer 0.9.10

* Updated the date selector
* Added visual tweaks
* Makefile tweaks
* Separated out base docker image
* Various cleanup to reduce R CMD check errors

# AirSensorDataViewer 0.9.9

* Removed **worldmet** dependency.

# AirSensorDataViewer 0.9.8

* Revamped codebase to follow `{golem}` shiny framework
* Added tiotemp R package for visualizations
* Added User R6 OOP to control state and data of client session
* ...

# AirSensor DataViewer 0.9.7

* Same look - New Great Taste Update!
* Resturctured directory
* Refactored all code to utlize Shiny Modules
* Rewritten to utilize promise/future async evaluation 
* New overview look
* New latest data look
* Updated help html
* Updated airdatepicker to be inline to avoid weird overlapping UI
* Resolved timezones

# AirSensor DataViewer 0.7

* Year 2020(Y2.02K) bug smashed :)
* Resolved menu selection bugs. 

# AirShiny 0.4

* Added bookmarks
* Summary plot converted to dygraph
* Update theme and looks 
* Updated calendar plot
* UI reorder
* Added new shinyjs
* Added new shinyWidgets
* Function improvements

# AirSensorShiny 0.3

* Added more URL queries
* Added new tabs: overview, animation, compare, patterns, and raw. 
* Added linear fit to `utils.R`.
* New colors 
* Added calendar plot 
* Added more reactive framework

# AirSensorShiny 0.2 

* Create URL freindly website, where queries can be passed in via URL
* Updated UI to support new features, such as tabs and more functionality
* Added internal `utils.R` script to support features
* Refactored `server.R` to add functionality

# AirSensorShiny 0.1
 
* Modified Makefile and docker files to get things working on
  a Mac desktop machine.

# AirSensorShiny 0.0
 
* Added docker directory to house all docker related files
* Error Sanitization in `server.R` (plotting, tables, etc.)
* Include initial Makefile for easy deployment 
