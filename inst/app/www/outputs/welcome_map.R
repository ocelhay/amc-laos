output$welcome_map <- renderLeaflet({
  
  dta <- amc_dta %>%
    group_by(hospital) %>%
    summarise(nb_year = n_distinct(data_collecting_year)) %>%
    mutate(label = glue("{hospital}"))
    # mutate(label = glue("{hospital}: </br>{nb_year} year(s) of AMC data")) %>%
    # mutate(label = htmltools::HTML(label))
  
  dta <- left_join(dta, coords, by = c("hospital" = "hospcd"))
  
  return(
    leaflet(data = dta) %>% 
      addTiles() %>%
      addPolygons(data = shp_lao_provinces, fill = FALSE, dashArray = 1, weight = 0, color='black') %>%
      addMarkers(lng = ~lon, lat = ~lat, label = ~label, labelOptions = labelOptions(noHide = T))
  )
})