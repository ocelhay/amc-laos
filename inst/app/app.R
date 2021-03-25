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

# Variables for filters ----
unique_year <- unique(amc_dta$data_collecting_year)
unique_hospital <- unique(amc_dta$hospital)
unique_act_3_name <- unique(amc_dta$act_3_name)
unique_substance <- unique(amc_dta$substance)
unique_route <- unique(amc_dta$route)
unique_a_wa_re <- unique(amc_dta$a_wa_re)

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
                                    
                                    prettyCheckboxGroup(inputId = "filter_year", label = i18n$t("Years:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_year, 
                                                        selected = unique_year),
                                    prettyCheckboxGroup(inputId = "filter_hospital", label = i18n$t("Years:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_hospital, 
                                                        selected = unique_hospital),
                                    pickerInput(inputId = "filter_act_3_name", label = i18n$t("ACT 3:"), multiple = TRUE,
                                                choices = unique_act_3_name, selected = unique_act_3_name,
                                                options = list(
                                                  `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                  `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                    pickerInput(inputId = "filter_substance", label = i18n$t("Substance:"), multiple = TRUE,
                                                choices = unique_substance, selected = unique_substance,
                                                options = list(
                                                  `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                  `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                    prettyCheckboxGroup(inputId = "filter_route", label = i18n$t("Route:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_route, 
                                                        selected = unique_route),
                                    prettyCheckboxGroup(inputId = "filter_a_wa_re", label = i18n$t("AWaRE Group:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_a_wa_re, 
                                                        selected = unique_a_wa_re)
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
                               div(class = "box_outputs",
                                   h4(i18n$t("Number of consumed antimicrobials by hospital by year")),
                                   highchartOutput("all_hosp_consum")
                               ),
                               fluidRow(
                                 column(6,
                                        div(class = "box_outputs",
                                            h4(i18n$t("Antimicrobial consumptions based on AWaRE group")),
                                            highchartOutput("aware")
                                        )
                                 ),
                                 column(6,
                                        div(class = "box_outputs",
                                            h4(i18n$t("Route of administration")),
                                            highchartOutput("route")
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(6,
                                        div(class = "box_outputs",
                                            h4(i18n$t("Common consumed antimicrobial group")),
                                            highchartOutput("consum_group")
                                        )
                                 ),
                                 column(6,
                                        div(class = "box_outputs",
                                            h4(i18n$t("Common consumed antimicrobial agents")),
                                            highchartOutput("consum_agent")
                                        )
                                 )
                               ),
                               DTOutput("list")
                      )
           )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive data management
  amc_dta_filter <- reactive(
    amc_dta %>%
      filter()
    )
  
  # Source code to generate outputs
  file_list <- list.files(path = "./www/outputs", pattern = "*.R")
  for (file in file_list) source(paste0("./www/outputs/", file), local = TRUE)$value
  
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
