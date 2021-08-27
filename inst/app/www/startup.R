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

app_version <- "v.0.2-beta - Data last updated on 24/03/2021"

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

# Access = Green; Watch = Yellow; Reserve = Red
aware_colors <- c("#18bc9c", "#f39c12", "#e74c3c")

# Variables for filters ----
unique_year <- unique(amc_dta$data_collecting_year)
unique_hospital <- unique(amc_dta$hospital)
unique_act_3_name <- sort(unique(amc_dta$act_3_name))
unique_substance <- sort(unique(amc_dta$substance))
unique_route <- unique(amc_dta$route)