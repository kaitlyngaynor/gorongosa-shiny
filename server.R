# Gorongosa Camera Trap Data Shiny App

# Server

server <- function(input, output, session) {
  

# SINGLE SPECIES ----------------------------------------------------------
  
  # Subset based on widgets -------------------------------------------------
  
  # create reactive object records_subset that changes based on delta time and date range
  records_subset <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range1[1], Date <= input$date_range1[2])
  })
  
  # create reactive object species_subset that changes based on species_select widget selection 
  species_subset <- reactive({
    records_subset() %>%
      filter(Species == input$species_select)
  })
  
  # Summarize subset data ---------------------------------------------------
  
  # summarize species counts across cameras
  species_summary <- reactive({
    species_subset() %>%
      group_by(Camera) %>%
      summarise(count = n())
  })
  
  # calculate RAI
  rai <- reactive({
    rai.calculate(records_subset(), camera_operation_matrix, input$date_range1[1], input$date_range1[2])
  })
  
  # subset to species
  rai_species <- reactive({
    rai() %>% filter(Species == input$species_select) %>% select(Camera, Count, Operation, RAI)
  })
  
  # calculate monthly RAI
  monthly_rai <- reactive({
    rai.monthly(records_subset(), camera_operation_matrix, input$date_range1[1], input$date_range1[2])
  })
  
  # combine RAI and metadata
  rai_metadata <- reactive({
    left_join(rai_species(), camera_metadata)
  })


  # Map outputs -------------------------------------------------------------
  
  # merge hexes with RAI
  hexes_rai <- reactive({
    full_join(hexes, rai_species())
  })
  
  # make color palette for map
  pal <- reactive({
    colorNumeric(palette = "viridis", domain = hexes_rai()$RAI)
  })
  
  output$rai_map <- renderLeaflet({
    
    leaflet(hexes_rai()) %>%
      setView(34.42, -18.95, 11) %>%
      addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
      addPolygons(
        data = hexes_rai(),
        fillColor = ~pal()(hexes_rai()$RAI),
        fillOpacity = 1, 
        weight = 1, # stroke weight of lines
        color = "gray" # color of lines
      ) %>% 
      
      addLegend_decreasing(pal = pal(), 
                           values = ~RAI,
                           opacity = 1, 
                           title = "RAI",
                           position = "topleft",
                           decreasing = TRUE)
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
      filter(Species == input$species_select, Camera == "All")
  })
  
  # render a reactive graph with RAI against other variable
  output$rai_metadata <- renderPlotly({
    x_axis <- input$metadata_select
    ggplotly(ggplot(data = rai_metadata(),
           aes_string(x = x_axis, y = "RAI", label = "Camera")) +
      geom_point() +
      geom_smooth(method = "lm", col = "gray") +
      theme_bw()
      )
  })
  
  # render a reactive graph with RAI every month
  output$monthly_rai_hist <- renderPlotly({
    ggplotly(ggplot(data = (monthly_rai() %>% filter(Species == input$species_select, Camera == "All")),
           aes(x = Month_Year, y = RAI)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      )
  })
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot <- renderPlot({
    timeplot(species_subset()$Time.Sun)
  })

# DATASET COMPARISON ----------------------------------------------------------
  

# Subset based on widgets -------------------------------------------------

  # create reactive object records_subset that changes based on delta time and date range
  records_subset_A <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range1_A[1], Date <= input$date_range1_A[2])
  })
  records_subset_B <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
             Date >= input$date_range1_B[1], Date <= input$date_range1_B[2])
  })
  
  # create reactive object species_subset that changes based on species_select widget selection 
  species_subset_A <- reactive({
    records_subset_A() %>%
      filter(Species == input$species_select_A)
  })
  species_subset_B <- reactive({
    records_subset_B() %>%
      filter(Species == input$species_select_B)
  })  
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot_compare <- renderPlot({
    overlapPlot2(species_subset_A()$Time.Sun, species_subset_B()$Time.Sun)
  })  
  
}



# ggplot(data = (RAI.table %>% filter(Species == "Baboon", Camera == "All")),
# aes(x = Month_Year, y = RAI)) +
#   geom_bar(stat = "identity")
