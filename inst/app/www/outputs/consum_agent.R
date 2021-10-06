output$consum_agent <- renderHighchart({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(substance) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum)) %>%
    top_n(10)
  
  hchart(dta, type = "column", hcaes(x = substance, y = consum)) %>%
    hc_yAxis(title = "Total DDD per patient encounter") %>% 
    hc_xAxis(title = "Substance") %>%
    hc_tooltip(headerFormat = "", pointFormat = "<strong>{point.substance}</strong><br>{point.consum} DDD per patient encounter")
})