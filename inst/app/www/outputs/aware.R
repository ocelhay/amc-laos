output$aware <- renderHighchart({
  dta <- amc_dta %>%
    group_by(a_wa_re) %>%
    summarise(tot = sum(dad))
  
  
  highchart() %>%
    hc_add_series(dta, "pie", hcaes(name = a_wa_re, y = tot),
                  dataLabels = list(enabled = TRUE, style = list(fontSize = "13px"),
                                    format = '{point.name} <br>{point.percentage:.0f} %')) %>%
    hc_title(text = "AWaRe Group") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.tot:.1f} DAD")
})