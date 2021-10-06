output$consum_group <- renderHighchart({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(act_3_name) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum)) %>%
    top_n(5)
  
  hchart(dta, type = "column", hcaes(x = act_3_name, y = consum)) %>%
    hc_yAxis(title = "Total DDD per patient encounter") %>% 
    hc_xAxis(title = "ACT3 - Antibiotic Group") %>%
    hc_tooltip(headerFormat = "", pointFormat = "<strong>{point.act_3_name}</strong><br>{point.consum} DDD per patient encounter")
})