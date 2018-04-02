# Weather Forecasting App

### How to use?

* Running with the source code: Download the repo and run open `ui.R` and run it 

* Running via Github: `runGitHub('amrrs/weather_forecasting_app')`

### Packages required:

* `library(shiny)`
* `library(plotly)`
* `library(highcharter)`
* `library(shinydashboard)`
* `library(ggplot2)`
* `library(dplyr)`
* `library(reshape2)`
* `library(TSstudio)`
* `library(forecast)`
* `library(googlesheets)`
* `library(dashboardthemes)`

If `shiny` package is missing, Use `install.packages('shiny')` to install `shiny` package. Similarly for other missing packages, except `dashboardthemes` which isn't available on CRAN yet. Install `dashboardthemes` using the following code:

```
library(devtools)
install_github("nik01010/dashboardthemes")

```

**Hosted** @ [shinyapps.io](https://amrrs.shinyapps.io/weather_forecasting/)

Sample Screenshot:

![Screenshot](app-screenshot1.png)
