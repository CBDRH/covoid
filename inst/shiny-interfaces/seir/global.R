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


# Define list of countries
ctry1 <- data.frame(country = covoid::available_contact_matrices())
ctry2 <- data.frame(country = covoid::age_distributions_un())
ctry3 <- data.frame(country = unique(covid19_data$Country.Region))
ctry4 <- merge(ctry1, ctry2, by="country")
ctryList <- as.vector(merge(ctry3, ctry4, by="country"))
