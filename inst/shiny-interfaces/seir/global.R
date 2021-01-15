# libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(tippy)
library(covoid)
library(shinyalert)
#library(tidyverse)
library(dplyr)
library(tidyr)
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


# Internation Covid19 Data from coronavirus package
covid19_data <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE) %>%
    dplyr::mutate(country = ifelse(country == "United Arab Emirates", "UAE", country),
                  country = ifelse(country == "Mainland China", "China", country),
                  country = ifelse(country == "North Macedonia", "N.Macedonia", country),
                  country = trimws(country),
                  country = factor(country, levels = unique(country)),
                  date = as.Date(date))

# Define list of countries
ctry1 <- data.frame(country = covoid::available_contact_matrices(), stringsAsFactors = FALSE) %>% mutate(cm=1)
ctry2 <- data.frame(country = covoid::age_distributions_un(), stringsAsFactors = FALSE) %>% mutate(ad=1)
ctry3 <- data.frame(country = unique(covid19_data$country), stringsAsFactors = FALSE) %>% mutate(cov=1)

# Merge country list
ctry4 <- dplyr::full_join(ctry1, ctry2, by = 'country')
ctry5 <- dplyr::full_join(ctry4, ctry3, by = 'country') %>%
    mutate(sum = cm + ad + cov) %>%
    mutate(simpleName =
               case_when(
                   sum == 3 ~ country,
                   grepl("United Kingdom", country) ~ "United Kingdom",
                   grepl("Iran", country) ~ "Iran",
                   grepl("Russia", country) ~ "Russia",
                   grepl("Taiwan", country) ~ "Taiwan",
                   grepl("Bolivia", country) ~ "Bolivia",
                   grepl("Czech", country) ~ "Czech Republic",
                   grepl("Lao", country) ~ "Laos",
                   country == "Republic of Korea" ~ "South Korea",
                   country == "Korea, South" ~ "South Korea",
                   country == "United States of America" ~ "United States of America",
                   country == "US" ~ "United States of America",
                   grepl("Viet", country) ~ "Vietnam",
                   grepl("Venezuela", country) ~ "Venezuela"
               ))

# List of simplified country names for dropdown menu
ctryList <- ctry5 %>% filter(!is.na(simpleName)) %>% distinct(simpleName) %>% arrange(simpleName)

