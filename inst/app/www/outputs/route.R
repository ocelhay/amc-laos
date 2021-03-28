output$route <- renderHighchart({
  req(amc_dta_filter())
  
  dta <- amc_dta_filter() %>%
    group_by(route) %>%
    summarise(tot = sum(dad))
  
  
  highchart() %>%
    hc_add_series(dta, "pie", hcaes(name = route, y = tot),
                  dataLabels = list(enabled = TRUE, style = list(fontSize = "13px"),
                                    format = '{point.name} <br>{point.percentage:.2f} %')) %>%
    hc_title(text = "Route of administration") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.tot:.1f} DAD")
})