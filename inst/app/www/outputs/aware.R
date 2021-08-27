output$aware <- renderHighchart({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    mutate(a_wa_re = factor(a_wa_re, levels = c("Access", "Watch", "Reserve"))) %>%
    group_by(a_wa_re) %>%
    summarise(tot = sum(dad)) %>%
    complete(a_wa_re, fill = list(tot = 0))
  
  highchart() %>%
    hc_add_series(dta, "pie", hcaes(name = a_wa_re, y = tot),
                  dataLabels = list(enabled = TRUE, style = list(fontSize = "13px"),
                                    format = '{point.name} <br>{point.percentage:.0f} %')) %>%
    hc_title(text = "AWaRe Group") %>%
    hc_colors(aware_colors) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.tot:.1f} DDD per patient encounter")
})