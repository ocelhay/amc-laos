source('./www/startup.R', local = TRUE)

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
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_01_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_01_la.md")))) %>%
                                          bs_append(title = i18n$t("Why is the AMC dashboard needed?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_02_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_02_la.md")))) %>%
                                          bs_append(title = i18n$t("Where is surveillance being done?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_03_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_04_la.md")))) %>%
                                          bs_append(title = i18n$t("What do we know about Lao National AMC monitoring?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_04_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_04_la.md")))) %>%
                                          bs_append(title = i18n$t("What do we know about Lao hospital AMC monitoring?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_05_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_05_la.md")))) %>%
                                          bs_append(title = i18n$t("What is the Defined Daily Doses (DDDs) methodology?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_06_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_06_la.md")))) %>%
                                          bs_append(title = i18n$t("What antibiotics are included in this dashboard?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_07_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_07_la.md")))) %>%
                                          bs_append(title = i18n$t("What is the WHO AWaRe classification?"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_08_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_08_la.md")))) %>%
                                          bs_append(title = i18n$t("Acknowledgements and Credits"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_09_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_09_la.md")))) %>%
                                          bs_append(title = i18n$t("Contact"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_10_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_10_la.md")))) %>%
                                          bs_append(title = i18n$t("Disclaimer"), 
                                                    content = div(conditionalPanel("input.selected_language == 'en'", includeMarkdown("./www/markdown/welcome_11_en.md")),
                                                                  conditionalPanel("input.selected_language == 'la'", includeMarkdown("./www/markdown/welcome_11_la.md")))),
                                 ),
                                 column(5, offset = 1,
                                        leafletOutput("welcome_map", height = 450)
                                 ),
                               )
                      ),
                      tabPanel(i18n$t("AMC in Laos"), value = "tab_1",
                               div(class = "box_outputs",
                                   h4(i18n$t("Defined Daily Dose per patient encounter")),
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
