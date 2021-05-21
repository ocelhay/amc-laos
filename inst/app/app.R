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

app_version <- "v.0.1-beta - Data last updated on 24/03/2021"

if(FALSE) {
  shiny::runApp()
}

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
  mutate(
    hospital = case_when(
      provinces == "Luang Natha" ~ "Luang Namtha Provincial Hospital",
      provinces == "Xiengkhuang" ~ "Xiengkhuang Provincial Hospital",
      provinces == "Salavan" ~ "Salavan Provincial Hospital"),
    act_3_name = recode(act_3_name, "OTHER BETA-LACTAM ANTIBACTERIALS" = "Cephalosporins and carbapenems (and any others this includes)")) %>%
  mutate(
    act_3_name = str_to_sentence(act_3_name),
    route = recode(route, "O" = "Oral", "P" = "Parenteral", "R" = "Rectal")
  )


# Variables for filters ----
unique_year <- unique(amc_dta$data_collecting_year)
unique_hospital <- unique(amc_dta$hospital)
unique_act_3_name <- unique(amc_dta$act_3_name)
unique_substance <- sort(unique(amc_dta$substance))
unique_route <- unique(amc_dta$route)

# Define UI ----
ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  title = "Lao Antimicrobial Consumption Dashboard",
  theme = shinytheme("flatly"),
  shinyWidgets::chooseSliderSkin("Big"),
  includeCSS("./www/styles.css"),
  
  fluidRow(
    # Sidebar ----
    column(width = 3,
           splitLayout(
             tags$img(src = 'MoH-logo.png', id = 'logo_moh'),
             tags$img(src = 'FDD-logo.png', id = 'logo_fdd')
           ),
           tags$a(href='https://www.tropmedres.ac/units/lomwru-lao-pdr', tags$img(src = 'LOMWRU.jpg', id = 'logo_lomwru')),
           
           conditionalPanel(condition = "input.tabs == 'welcome'",
                            br(),
                            h4(i18n$t("Lao AMC Dashboard: Explore Antimicrobial Consumption in Laos")),
                            radioButtons('selected_language', label = NULL, choices = c("🇬🇧English" = "en", "🇱🇦ພາສາລາວ"= "la"), selected = "en", inline = TRUE),
           ),
           br(),
           p(app_version),
           
           conditionalPanel(condition = "input.tabs != 'welcome'",
                            div(id = "floatingfilter",
                                div(class = "box_outputs",
                                    h4(icon("filter"), i18n$t("Filter Data:")),
                                    prettyCheckboxGroup(inputId = "filter_year", label = i18n$t("Years:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_year, 
                                                        selected = unique_year),
                                    prettyCheckboxGroup(inputId = "filter_hospital", label = i18n$t("Hospitals:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_hospital, 
                                                        selected = unique_hospital),
                                    pickerInput(inputId = "filter_act_3_name", label = i18n$t("Antibiotic class:"), multiple = TRUE,
                                                choices = unique_act_3_name, selected = unique_act_3_name,
                                                options = list(
                                                  `actions-box` = TRUE, 
                                                  `deselect-all-text` = "Select None",
                                                  `select-all-text` = "Select All", 
                                                  `none-selected-text` = "None Selected",
                                                  `selected-text-format` = paste0("count > ", length(unique_act_3_name) - 1), 
                                                  `count-selected-text` = "All Classes Selected")
                                    ),
                                    pickerInput(inputId = "filter_substance", label = i18n$t("Antibiotic:"), multiple = TRUE,
                                                choices = unique_substance, selected = unique_substance,
                                                options = list(
                                                  `actions-box` = TRUE, 
                                                  `deselect-all-text` = "Select None",
                                                  `select-all-text` = "Select All", 
                                                  `none-selected-text` = "None Selected",
                                                  `selected-text-format` = paste0("count > ", length(unique_substance) - 1), 
                                                  `count-selected-text` = "All Antibiotics Selected")),
                                    prettyCheckboxGroup(inputId = "filter_route", label = i18n$t("Route of administration:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = unique_route, 
                                                        selected = unique_route),
                                    prettyCheckboxGroup(inputId = "filter_a_wa_re", label = i18n$t("AWaRe Group:"), 
                                                        status = "primary", inline = TRUE,
                                                        choices = c("Access", "Watch", "Reserve"), 
                                                        selected = c("Access", "Watch", "Reserve"))
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
                                        h4(i18n$t("About the Lao AMC Dashboard")), br(),
                                        bs_accordion(id = "amc_info") %>%
                                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                                          bs_append(title = i18n$t("What do we know about antimicrobial consumption (AMC) in Laos?"), 
                                                    content = i18n$t("Antimicrobial resistance is a great public health concern in Laos now. High antimicrobial consumption (AMC) is likely to be an important factor of the worsening AMR situation. Food and Drug department, Ministry of Health of Lao PDR (Laos) has systematically collected antimicrobial consumption data in Laos in Sethathirad Hospital, Khammuane and Luang Prabang Provincial Hospitals since 2018 as national based line data.")) %>%
                                          bs_append(title = i18n$t("Why is AMC dashboard needed?"), 
                                                    content = i18n$t("This AMC Dashboard allows clinicians, pharmacists and nurses as well as policy makers to have easy access to antimicrobial consumption data in Laos. This AMC dashboard will automatically produce a report for all users.")) %>%
                                          bs_append(title = i18n$t("Where is surveillance being done?"), 
                                                    content = i18n$t("This AMC dashboard currently contains surveillance 2019 data from three provincial hospitals, including Salavan, Xienghuang and Luang Namtha provincial Hospitals. In the future, we will upload AMC data from other surveillance sites including Khammuan, Luang Prabang, Vientiane and Savannakhet provincial hospitals as well as some central hospitals in Laos.")) %>%
                                          bs_append(title = i18n$t("Acknowledgements and credits"), 
                                                    content = i18n$t("App development team: Olivier Celhay (https://olivier.celhay.net), Vilada Chansamouth, Elizabeth Ashley. With highly contribution from: Ms Vayouly Vidhamaly, Mr Kongchak and Ms Phouthavanh")) %>%
                                          bs_append(title = i18n$t("Contact"), 
                                                    content = i18n$t("For any inquiry on this application, please contact Vilada Chansamouth (vilada@tropmedres.ac) or Lao PDR Food and Drug Department http://www.fdd.gov.la/")) %>%
                                          bs_append(title = i18n$t("Disclaimer"), 
                                                    content = i18n$t("The information contained in this application is the property of surveyed hospitals/LOMWRU/Food and Drug Department, Ministry of Health and may not be reproduced or distributed in any manner without express written permission. While we have attempted to ensure the accuracy of the data it is reporting, it makes no representations or warranties, expressed or implied, as to the accuracy or completeness of the information reported. Likewise, we make no representations or warranties, expressed or implied, as to the accuracy of the comparative data provided herein. We assume no legal liability or responsibility for any errors or omissions in the information or for any loss or damage resulting from the use of any information contained on these pages. This report is not intended to provide medical advice")),
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
                                            h4(i18n$t("Antimicrobial consumptions based on AWaRe group")),
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
                                            h4(i18n$t("Most common antibiotic classes")),
                                            highchartOutput("consum_group")
                                        )
                                 ),
                                 column(6,
                                        div(class = "box_outputs",
                                            h4(i18n$t("Most common antibiotics")),
                                            highchartOutput("consum_agent")
                                        )
                                 )
                               ),
                               div(class = "box_outputs",
                                   h4(i18n$t("Breakdown of antimicrobial consumptions")),
                                   DTOutput("table")
                               )
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
      filter(data_collecting_year %in% input$filter_year,
             hospital %in% input$filter_hospital,
             act_3_name %in% input$filter_act_3_name,
             substance %in% input$filter_substance,
             route %in% input$filter_route,
             a_wa_re %in% input$filter_a_wa_re)
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
