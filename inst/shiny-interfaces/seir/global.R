# libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(tippy)
library(covoid)
library(shinyalert)
library(tidyverse)
library(shinyWidgets)
library(shinycustomloader)
library(ggiraph)
library(patchwork)
library(markdown)
library(ggformula)
library(ggrepel)
library(scales)
library(magrittr)
library(DT)
library(imputeTS)
library(shinydashboardPlus)
library(sparkline)
library(shinyBS)
library(visNetwork)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(googleVis)
library(prettyunits)
library(gganimate)
library(heatmaply)
library(formattable)

# Source extended EpiModel files
source_files <- c("prepData.R", "plotResults.R",
                  "plotResults.R", "animateResults.R",
                  "clickr-utils.R", "summariseResults.R")
for (source_file in source_files) {
    source(paste("R/", source_file, sep = ""))
}


# Internation Covid19 Data from COVID19 package
data("covid19_data")

