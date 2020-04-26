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


# Source extended EpiModel files
source_files <- c("prepData.R", "plotResults.R",
                  "plotResults.R", "animateResults.R")
for (source_file in source_files) {
    source(paste("R/", source_file, sep = ""))
}
