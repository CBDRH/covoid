
# devtools::install_github("RamiKrispin/coronavirus")
# covid19_data <- coronavirus
# save(covid19_data, file = "data/covid19_data.rda")

# # Read United Nations population estimates from https://population.un.org/wpp/Download/Standard/Population/
# url1<-'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx'
# p1f <- tempfile()
# download.file(url1, p1f, mode="wb")
# total_population_un <- read_excel(path = p1f, sheet = "ESTIMATES", range = "A17:BZ306") %>%
#     mutate(country = `Region, subregion, country or area *`,
#            pop = as.numeric(`2020`)*1000) %>%
#     select(country, pop) %>%
#     filter(!is.na(pop))
#
# save(total_population_un, file = "data/total_population_un.rda")


covid19_data <- function() {
    tmp_env = new.env()
    data(covid19_data,envir = tmp_env)
    covid19_data = tmp_env$covid19_data
    covid19_data
}
