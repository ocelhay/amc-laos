output$table <- renderDataTable({
  req(amc_dta_filter() %>% nrow() > 0)
  
  dta <- amc_dta_filter() %>%
    group_by(act_3_name, substance, route, a_wa_re) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum))
  
  datatable(
    dta,
    rownames = FALSE,
    colnames = c("Antibiotic Class", "Antibiotic", "Route of administration", 
    "AWaRe", "DDD per patient encounter")
  )
})