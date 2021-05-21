output$table <- renderDataTable({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(substance, act_3_name, route, a_wa_re) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum))
  
  datatable(
    dta,
    colnames = c("Antibiotic", "Antibiotic Class", "Route of administration", 
    "AWaRe", "DDD per patient encounter")
  )
})