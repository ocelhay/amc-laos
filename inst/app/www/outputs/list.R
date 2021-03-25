output$list <- renderDataTable({
  dta <- amc_dta %>%
    group_by(act_3_name, substance, route, a_wa_re) %>%
    summarise(consum = round(sum(dad), 1), .groups = "drop") %>%
    arrange(desc(consum))
  
  datatable(dta)
})