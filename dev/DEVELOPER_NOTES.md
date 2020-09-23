# Developer Notes

Background reading and guidance for anyone wanting to understand the DataViewer
code base well enough to make modifications.

## Learning Shiny and golem

[Mastering Shiny](https://mastering-shiny.org) -- 2020 online book by Hadley Wickham.

[Engineering Production-Grade Shiny Apps](https://engineering-shiny.org) --
2020 online book published by the creators of [golem](https://thinkr-open.github.io/golem/).

...


## Running the app in RStudio

The very first level of testing and debugging should take place in RStudio.

After making changes to the **AirSensorDataViewer** source code, perform the
following steps in RStudio:

1. ...
2. ...

To run the app, type into the console:

```
AirSensorDataViewer::run_app()
```

...


## Logging

### Background Reading

[Debugging Shiny applications](https://shiny.rstudio.com/articles/debugging.html) --
RStudio guide

[Shiny Server Error Logs ](https://support.rstudio.com/hc/en-us/articles/115003717168-Shiny-Server-Error-Logs)
-- RStudio support posts with additional links

[Shiny Server Admin documentation](https://docs.rstudio.com/shiny-server/) --
see sections on "Server Log" and "Logging and Analytics"

[shinyEventLogger](https://kalimu.github.io/shinyEventLogger/) -- package
dedicated to logging in Shiny apps

...
