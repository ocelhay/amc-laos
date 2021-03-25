output$consum_group <- renderHighchart({
  dta <- amc_dta %>%
    group_by(act_3_name) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum))
  
  hchart(dta, type = "column", hcaes(x = act_3_name, y = consum)) %>%
    hc_yAxis(title = "Total DAD") %>% 
    hc_xAxis(title = "ACT3 - Antibiotic Group") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.consum} DAD")
})