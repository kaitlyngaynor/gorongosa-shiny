# Gorongosa Camera Trap Data Shiny App

# Server

server <- function(input, output, session) {
  

# SINGLE SPECIES ----------------------------------------------------------
  
  # Subset based on widgets -------------------------------------------------
  
  # create reactive object records_subset that changes based on delta time and date range
  records_subset <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      filter(Species == input$species_select)
  })

  # Summarize subset data ---------------------------------------------------
  
  # summarize species counts across cameras
  species_summary <- reactive({
    records_subset() %>%
      group_by(Camera) %>%
      summarise(count = n())
  })
  
  # calculate RAI
  rai <- reactive({
    rai.calculate(records_subset(), camera_operation_matrix, input$date_range[1], input$date_range[2])
  })
  
  # calculate monthly RAI
  monthly_rai <- reactive({
    rai.monthly(records_subset(), camera_operation_matrix, input$date_range[1], input$date_range[2])
  })
  
  # combine RAI and metadata
  rai_metadata <- reactive({
    left_join(rai(), camera_metadata)
  })


  # Map outputs -------------------------------------------------------------
  
  # merge hexes with RAI
  hexes_rai <- reactive({
    full_join(hexes, rai())
  })
  
  # make color palette for map
  pal <- reactive({
    colorNumeric(palette = "viridis", domain = hexes_rai()$RAI)
  })
  pal_log <- reactive({
    colorNumeric(palette = "viridis", domain = log(hexes_rai()$RAI + 0.001))
  })
  
  # create map labels
  map_labels <- reactive({
    sprintf(
    "<strong>Camera: %s</strong><br/>Detections: %i<br/>Days operating: %i<br/>RAI: %g",
    hexes_rai()$Camera, hexes_rai()$Detections, hexes_rai()$Operation, hexes_rai()$RAI, 0) %>% 
      lapply(htmltools::HTML)
  })
  
  # generate leaflet map
  output$rai_map <- renderLeaflet({
    
    if(input$log_select_map == 1) {
      
      leaflet(hexes_rai()) %>%
        
        setView(34.42, -18.95, 11) %>%
        addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
        
        addPolygons(
          data = hexes_rai(),
          fillColor = ~pal()(hexes_rai()$RAI),
          fillOpacity = 1, 
          weight = 1, # stroke weight of lines
          color = "gray", # color of lines
          label = map_labels(),
          highlight = highlightOptions(
            weight = 2,
            color = "white",
            fillOpacity = 1,
            bringToFront = TRUE)
        ) %>% 
        
        addLegend_decreasing(pal = pal(), 
                             values = ~RAI,
                             opacity = 1, 
                             title = "RAI",
                             position = "topleft",
                             decreasing = TRUE)
    } else {
      leaflet(hexes_rai()) %>%
        
        setView(34.42, -18.95, 11) %>%
        addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
        
        addPolygons(
          data = hexes_rai(),
          fillColor = ~pal_log()(log(hexes_rai()$RAI + 0.001)),
          fillOpacity = 1, 
          weight = 1, # stroke weight of lines
          color = "gray", # color of lines
          label = map_labels(),
          highlight = highlightOptions(
            weight = 2,
            color = "white",
            fillOpacity = 1,
            bringToFront = TRUE)
        ) %>% 
        
        addLegend_decreasing(pal = pal_log(), 
                             values = ~log(RAI + 0.001),
                             opacity = 1, 
                             title = "log(RAI)",
                             position = "topleft",
                             decreasing = TRUE)    
    }
    
  })
  
  # Render outputs ----------------------------------------------------------
  
  # render a reactive table that shows a summary by species
  output$species_table <- renderTable({
    species_summary()
  })

  # render a reactive table that shows RAI of selected species at each camera
  output$rai_table <- renderTable({
    hexes.df.rai()
  })

  # render a reactive table that shows monthly RAI of selected species  
  output$monthly_rai_table <- renderTable({
    monthly_rai() %>%
      filter(Camera == "All")
  })
  
  # render a reactive graph with RAI against other variable
  output$rai_metadata <- renderPlotly({
    ggplotly(ggplot(data = rai_metadata(),
           aes_string(x = input$metadata_select, y = "RAI", label = "Camera")) +
      geom_point() +
      geom_smooth(method = "lm", col = "gray") +
      theme_bw()
      )
  })
  
  # render a reactive graph with RAI every month
  output$monthly_rai_hist <- renderPlotly({
    ggplotly(ggplot(data = (monthly_rai() %>% filter(Camera == "All")),
           aes(x = Month_Year, y = RAI, fill = Season)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values=c("#999999", "#F8766D", "#00BFC4")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      )
  })
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot <- renderPlot({
    timeplot(records_subset()$Time.Sun)
  })

# DATASET COMPARISON ----------------------------------------------------------
  

# Subset based on widgets comparison ------------------------------------------

  # create reactive object records_subset that changes based on delta time and date range
  records_subset_A <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range_A[1], Date <= input$date_range_A[2]) %>%
      filter(Species == input$species_select_A)
  })
  records_subset_B <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range_B[1], Date <= input$date_range_B[2]) %>%
      filter(Species == input$species_select_B)
  })

  # calculate RAI
  rai_A <- reactive({
    cbind(rai.calculate(records_subset_A(), camera_operation_matrix, input$date_range_A[1], input$date_range_A[2]),
          Subset = "A") 
  })
  rai_B <- reactive({
    cbind(rai.calculate(records_subset_B(), camera_operation_matrix, input$date_range_B[1], input$date_range_B[2]),
          Subset = "B")
  })
  rai_AB <- reactive({
    bind_rows(rai_A(), rai_B()) %>%
    select(Camera, RAI, Subset) %>%
    spread(key = Subset, value = RAI)
  })
  
  # calculate monthly RAI
  monthly_rai_A <- reactive({
    cbind(rai.monthly(records_subset_A(), camera_operation_matrix, input$date_range_A[1], input$date_range_A[2]),
          Subset = "A")
  })
  
  monthly_rai_B <- reactive({
    cbind(rai.monthly(records_subset_B(), camera_operation_matrix, input$date_range_B[1], input$date_range_B[2]),
          Subset = "B")
  })
  monthly_rai_AB <- reactive({
    bind_rows(monthly_rai_A(), monthly_rai_B())
  })
  
# Render outputs for comparison -------------------------------------------
    
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot_compare <- renderPlot({
    overlapPlot2(records_subset_A()$Time.Sun, records_subset_B()$Time.Sun)
    legend('top', c("Subset A", "Subset B"), lty=c(1,1), col = c("#F8766D", "#00BFC4"), bty='n')
  })  
  
  # calculate overlap value
  output$activity_overlap <- renderText({
    paste("Overlap of daily activity density (ranging from 0 to 1) = ", round(overlapEst(records_subset_A()$Time.Sun, records_subset_B()$Time.Sun, type = "Dhat4"), digits = 3))
  })
  
  # render a reactive graph with both RAI against each other
  # switch to log scale based on radio button

  output$rai_AB <- renderPlotly({
    
    if(input$log_select == 1) {
      ggplotly(ggplot(data = rai_AB(),
                      aes(x = A, y = B, label = Camera)) +
                 geom_point() +
                 geom_smooth(method = "lm", col = "gray") +
                 theme_bw())
    } else {
      ggplotly(ggplot(data = rai_AB(),
                      aes(x = log(A + 0.001), y = log(B + 0.001), label = Camera)) +
                 geom_point() +
                 geom_smooth(method = "lm", col = "gray") +
                 theme_bw())      
    }
    

  })
  
  # render a reactive graph with side-by-side barplot
  output$rai_monthly_AB <- renderPlotly({
    ggplotly(ggplot(data = (monthly_rai_AB() %>% filter(Camera == "All")),
                    aes(x = Month_Year, y = RAI, fill = Subset)) +
               geom_bar(stat = "identity", position = "dodge") +
               scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })

}



# ggplot(data = (RAI.table %>% filter(Species == "Baboon", Camera == "All")),
# aes(x = Month_Year, y = RAI)) +
#   geom_bar(stat = "identity")
