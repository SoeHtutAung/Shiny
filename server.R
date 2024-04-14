# create server
server <- function(input, output) {
  ## output object for selected region
  output$selected_region <- renderText({
    selected_regions <- input$region
    if (length(selected_regions) == 0) {
      return("No region selected")  # Display message if no regions are selected
    } else {
      # Concatenate selected region names with comma and space
      paste("Selected region:", paste(selected_regions, collapse = ", "))
    }
  })
  ## output object for selected range of conflict
  output$pop_minmax <- renderText({
    paste("Population range:",
          input$pop[1], "to", input$pop[2])
  })
  ## output object for selected range of conflict
  output$idp_minmax <- renderText({
    paste("IDP population up to:", input$idp)
  })
  ## output object for map page1
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = data_sr[data_sr$region %in% input$region, ],
                  fillColor = ~my_palette1(10)[cut(changes, 10)],
                  fillOpacity = 0.5, weight = .5,
                  label = ~paste("Region: ", region, "No. IDPs: ", idp_pop))
  })
  ## output object for plot
  output$plot1 <- renderPlotly({
    data_sr %>% filter (region %in% input$region & 
                          total >= input$pop[1] & total <= input$pop[2] &
                          idp_pop >= input$idp) %>%
      arrange(total) %>% # arrange by total population
      plot_ly(
        x = ~idp_pop, y = ~region, type = 'bar', name = 'IDP',marker = list(color = 'teal')) %>%
      add_trace(x = ~total, name = 'Total',marker = list(color = 'orange')) %>%
      layout(xaxis = list(title = 'Population'),
             yaxis = list(title = '',categoryorder = 'array', categoryarray = ~region))
  })
  ## output object for scatter plot
  output$plot3 <- renderPlotly({
    data_sr %>% as.data.frame() %>%
      filter (region %in% input$region &
                total >= input$pop[1] & total <= input$pop[2] &
                idp_pop >= input$idp) %>%
      plot_ly(x = ~idp_pop, y = ~changes,
              text = ~paste("Region: ", region, '<br>IDPs:', idp_pop), hoverinfo = 'text',
              type = 'scatter', mode = 'markers', size = ~idp_pop,
              color = ~changes, 
              colors = my_palette1(10)) %>%   # Maximum value of idp_pop
      layout(xaxis = list(title = 'IDP population', zeroline = FALSE),
             yaxis = list(title = '% increased poverty'),
             showlegend = FALSE)
  })
  ## output object for table
  output$table <- DT::renderDataTable({
    DTtable1 <- data_sr %>% as.data.frame() %>% # from shape file to data file
      filter (region %in% input$region & 
                total >= input$pop[1] & total <= input$pop[2] &
                idp_pop >= input$idp) %>%
      arrange(desc(total)) %>% 
      select(region, total, idp_pop, changes) %>%
      rename("Region" = region, "Total population" = total, "IDP population" = idp_pop,
             "% increased poverty" = changes)
    DT::datatable(DTtable1)
  })
  
  # reactive map update
  observeEvent(input$update,{
    updateCheckboxGroupInput(session, "region", selected = input$region, 
                             choices = unique(data_sr$region), inline = TRUE)
  })
  
  # Reactive dataframe for map
  sr_react <- reactive({
    req(input$region)  # Require input$region to be available
    
    filtered_sr <- data_sr %>% 
      filter (region %in% input$region & 
                total >= input$pop[1] & total <= input$pop[2] &
                idp_pop >= input$idp) %>%
      arrange(total) %>%
      select(region, idp_pop, changes) %>%
      distinct()
    
    return(filtered_sr)
  })
  
  # Update Leaflet map using leafletProxy
  observe({
    pal <- colorNumeric(palette = my_palette1(10), domain = sr_react()$changes)
    
    leafletProxy("map1", data = sr_react()) %>%
      clearShapes() %>%
      addPolygons(data = sr_react(),
                  fillColor = ~my_palette1(10)[cut(changes, 10)],
                  fillOpacity = 0.5, weight = .5,
                  label = ~paste("Region: ", region, "No. IDPs: ", idp_pop)) %>%
      addLegend("bottomleft", values = ~changes, pal = pal,
                title = "Increased poverty",
                labFormat = labelFormat(suffix = "%"),
                opacity = 0.2)
  })
  ###### page 2 ########
  ## output object for point map page2
  output$map2 <- renderLeaflet({
    map2_data <- data_dst_yr %>% group_by(Admin2) %>%
      summarize(Fatalities = sum(Fatalities, na.rm = TRUE),
                Events = sum(Events, na.rm = TRUE),
                long = first(long),
                lat = first(lat))
    
    leaflet(map2_data) %>%
      addTiles() %>%
      addCircles(~long, ~lat,
                 color = ~my_palette1(10)[cut(Fatalities, 10)],
                 popup = ~paste("District: ", Admin2, "<br> Fatalities: ", Fatalities),
                 radius = 15000, stroke = FALSE, fillOpacity = 0.8)
  })
  ## output object for poloygon map page2
  output$map3 <- renderLeaflet({
    map3_data <- data_dst_yr %>% group_by(Admin2) %>%
      summarize(Fatalities = sum(Fatalities, na.rm = TRUE),
                Events = sum(Events, na.rm = TRUE),
                geometry = first(geometry),
                long = first(long),
                lat = first(lat)) %>% st_as_sf()
    
    leaflet(map3_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~my_palette1(10)[cut(Fatalities, 10)],
                  fillOpacity = 0.5,
                  stroke = FALSE,
                  label = ~paste("District: ", Admin2, " Fatalities: ", Fatalities))
  })
  ## reactive both maps update
  observeEvent(input$update,{
    updateCheckboxGroupInput(session, "region_pg2", selected = input$region_pg2, 
                             choices = unique(data_dst_yr$Admin2), inline = TRUE)
  })
  ## Reactive dataframe for both maps
  dst_react <- reactive({
    req(input$region_pg2)  # Require input$region to be available
    
    selected_dst <- data_dst_yr %>% 
      filter (Admin1 %in% input$region_pg2 &
                Year >= input$year_from & Year <= input$year_to) %>%
      group_by(Admin2) %>%
      summarise(Events = sum(Events, na.rm = TRUE),
                Fatalities = sum(Fatalities, na.rm = TRUE),
                geometry = first(geometry),
                long = first(long),
                lat = first(lat))
    
    return(selected_dst)
  })
  ## Update Leaflet point map using leafletProxy
  observe({
    leafletProxy("map2", data = dst_react()) %>%
      clearShapes() %>%
      addCircles(~long, ~lat,
                 color = ~my_palette1(10)[cut(Fatalities, 10)],
                 popup = ~paste("District: ", Admin2, "<br> Fatalities: ", Fatalities),
                 radius = 15000, stroke = FALSE, fillOpacity = 0.8) 
  })
  ## Update Leaflet poloygon map using leafletProxy
  observe({
    leafletProxy("map3", data = st_as_sf(dst_react())) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~my_palette1(10)[cut(Fatalities, 10)],
                  fillOpacity = 0.5,
                  stroke = FALSE,
                  label = ~paste("District: ", Admin2, "<br> Fatalities: ", Fatalities)) 
  })
  ## output object for plot
  output$plot2 <- renderPlotly({
    filter_data <- data_dst_yr %>% filter (Admin1 %in% input$region_pg2 &
                                             Year >= input$year_from & Year <= input$year_to) %>%
      group_by(Admin2,Year) %>%
      summarise(Events = sum(Events, na.rm = TRUE),
                Fatalities = sum(Fatalities, na.rm = TRUE),
                geometry = first(geometry),
                long = first(long),
                lat = first(lat))
    plot_ly(filter_data, x = ~Year, y = ~Fatalities, type = 'scatter', mode = 'lines',
            text = ~paste("District: ", Admin2, "<br>Year: ", Year,
                          "<br>Fatalities: ", Fatalities),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = 'Year',tickmode = 'linear', dtick = 1),
             yaxis = list(title = 'Civilian fatalities'),
             hovermode = 'closest', showlegend = FALSE)
  })
  ## to prevent error with year entry - TBD
  
  ###### page 3 ######
  ## output object for poloygon map page3
  output$map4 <- renderLeaflet({
    leaflet(data_tsp) %>%
      addTiles() %>%
      addPolygons(fillColor = ~my_palette1(8)[cut(Vulnerability.Band, 8)],
                  fillOpacity = 0.5,
                  stroke = FALSE,
                  label = ~paste("Township: ", TS, "Vulnerability: ", Vulnerability.Band))
  })
  ## reactive both maps update
  observeEvent(input$update,{
    updateSelectizeInput(session, "region_pg3", selected = input$region_pg3, 
                         choices = unique(data_tsp$ST), inline = TRUE)
  })
  ## Reactive dataframe for both maps
  tsp_react <- reactive({
    req(input$region_pg3)  # Require input$region to be available
    
    selected_tsp <- data_tsp %>% 
      filter (ST %in% input$region_pg3 &
                Vulnerability.Band >= input$vulscore[1] &
                Vulnerability.Band <= input$vulscore[2] &
                Approximate.Vulnerable.Population >= input$vulpop)
    return(selected_tsp)
  })
  ## Update Leaflet map using leafletProxy
  observe({
    leafletProxy("map4", data = tsp_react()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~my_palette1(8)[cut(Vulnerability.Band, 8)],
                  fillOpacity = 0.5,
                  stroke = FALSE,
                  label = ~paste("Township: ", TS, "Vulnerability: ", Vulnerability.Band))
  })
}
