output$all_hosp_consum <- renderHighchart({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(data_collecting_year, hospital) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop")
  
  hchart(dta, type = "column", hcaes(x = data_collecting_year, y = consum, group = hospital)) %>%
    hc_yAxis(title = list(text = "Total DDD per patient encounter")) %>% 
    hc_xAxis(title = list(text = "Year, Hospital")) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.consum} DDD per patient encounter<br> {point.hospital} {point.data_collecting_year}")
})