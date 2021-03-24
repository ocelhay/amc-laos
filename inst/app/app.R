# Load packages ----
library(bsplus)
library(DT)
library(glue)
library(highcharter)
library(janitor)
library(leaflet)
library(readxl)
library(rgdal)
library(shiny.i18n)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(validate)


version_app <- "v.0.1-alpha - Data last updated on 24/03/2021"

# Lao Translation
i18n <- Translator$new(translation_csvs_path = "./www/translation/")

# Data import ----

amc_dta <- read_excel(path = "./www/data/AMC_data_2021-03-10.xlsx")
amc_dta <- clean_names(amc_dta)

# sheet ACT: removed some content in columns outside the table.
amc_dic <- list(
  act = read_excel(path = "./www/data/AMC_Dictionary_2021-03-10.xlsx", sheet = "ACT") %>% clean_names(),
  act3 = read_excel(path = "./www/data/AMC_Dictionary_2021-03-10.xlsx", sheet = "ACT3 & ATC name") %>% clean_names(),
  atc5 = read_excel(path = "./www/data/AMC_Dictionary_2021-03-10.xlsx", sheet = "ACT3 & 5, substance and AWaRe") %>% clean_names()
)

coords <- read.csv("./www/data/hospital_geo_coordinates.csv", stringsAsFactors = FALSE)

shp_lao_provinces <- readOGR("./www/data/shapefiles/provinces.shp")


amc_dta <- amc_dta %>%
  mutate(hospital = case_when(
    provinces == "Luang Natha" ~ "Luang Namtha Provincial Hospital",
    provinces == "Xiengkhuang" ~ "Xiengkhuang Provincial Hospital",
    provinces == "Salavan" ~ "Salavan Provincial Hospital"
  ))

# Define UI ----
ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  title = "Lao Antimicrobial Consumption Dashboard",
  theme = shinytheme("flatly"),
  shinyWidgets::chooseSliderSkin('HTML5'),
  includeCSS("./www/styles.css"),

  fluidRow(
    # Sidebar ----
    column(width = 3,
           tags$a(href='https://www.tropmedres.ac/units/lomwru-lao-pdr', tags$img(src = 'LOMWRU.jpg', class = 'logo')),
           
           conditionalPanel(condition = "input.tabs == 'welcome'",
                            br(),
                            h4(i18n$t("Lao AMC Dashboard: Explore Antimicrobial Consumption in Laos")),
                            radioButtons('selected_language', label = NULL, choices = c("🇬🇧English" = "en", "🇱🇦ພາສາລາວ"= "la"), selected = "en", inline = TRUE),
           ),
           br(),
           p(version_app),
           
           conditionalPanel(condition = "input.tabs != 'welcome'",
                            div(id = "floatingfilter",
                                div(class = "box_outputs",
                                    h4(icon("filter"), i18n$t("Filter Data:")),
                                    p("Placeholder for month-year filter"),
                                    p("Placeholder for hospital filter"),
                                    p("Placeholder for ATC3 name filter"),
                                    p("Placeholder for Antimicrobial agent filter"),
                                    p("Placeholder for Route filter"),
                                    p("Placeholder for AWaRE filter"),
                                )
                            )
           )
    ),
    # Main Content ----
    column(width = 9,
           navbarPage(NULL, id = "tabs", collapsible = TRUE,
                      tabPanel(i18n$t("Welcome"), value = "welcome",
                               fluidRow(
                                 column(6,
                                        h4(i18n$t("About the Lao AMC Dashboard")),
                                        bs_accordion(id = "amc_info") %>%
                                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                                          bs_append(title = i18n$t("What is AMC?"), 
                                                    content = i18n$t("...")) %>%
                                          bs_append(title = i18n$t("Why is it needed?"), 
                                                    content = i18n$t("...")) %>%
                                          bs_append(title = i18n$t("Where are data from?"), 
                                                    content = i18n$t("...")) %>%
                                          bs_append(title = i18n$t("Acknowledgements and credits"), 
                                                    content = i18n$t("...")) %>%
                                          bs_append(title = i18n$t("Contact"), 
                                                    content = i18n$t("...")) %>%
                                          bs_append(title = i18n$t("Disclaimer"), 
                                                    content = i18n$t("...")),
                                 ),
                                 column(5, offset = 1,
                                        leafletOutput("welcome_map", height = 450)
                                 ),
                               )
                      ),
                      tabPanel(i18n$t("AMC in Laos"), value = "tab_1",
                               
                               # div(class = "box_outputs",
                               #     h4(i18n$t("Purpose of Antimicrobial Prescriptions")),
                               #     fluidRow(
                               #       column(6, 
                               #              highchartOutput("tab_1_purpose_IPD"),
                               #              formattableOutput("tab_1_table_purpose_IPD")),
                               #       column(6, highchartOutput("tab_1_purpose_OPD"),
                               #              formattableOutput("tab_1_table_purpose_OPD"))
                               #     ),
                               #     div(class = 'explanations', i18n$t("Unknown = reason for antimicrobial prescription could not be determined from patient records"))
                               # ),
                               # fluidRow(
                               #   column(6, 
                               #          div(class = "box_outputs",
                               #              h4(i18n$t("Prescribed Antimicrobial Agents per Patient")),
                               #              highchartOutput("tab_1_number_prescribed")
                               #          )
                               #   ),
                               #   column(6, 
                               #          div(class = "box_outputs",
                               #              h4(i18n$t("Route of Given Antimicrobials")),
                               #              highchartOutput("tab_1_route"))
                               #   )
                               # )
                      )
           )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  # Stop the shiny app when the browser window is closed.
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Here is where we update language in session
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(session, input$selected_language)
  })
}

# Return the App ----
shinyApp(ui = ui, server = server)
