output$all_hosp_consum <- renderHighchart({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(data_collecting_year, hospital) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop")
  
  hchart(dta, type = "column", hcaes(x = data_collecting_year, y = consum, group = hospital)) %>%
    hc_yAxis(title = "Total DAD") %>% 
    hc_xAxis(title = "Year, Hospital") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.consum} DAD<br> {point.hospital} {point.data_collecting_year}")
})