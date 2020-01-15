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
  
  # merge camera DF with RAI
  hexes.df.rai <- reactive({left_join(hexes.df, rai_species(), by = c("id" = "Camera"))})
  
  output$rai_map <- renderPlotly({
    ggplotly(ggplot(hexes.df.rai(), aes(long, lat, group = group, fill = RAI, label = id)) + 
      geom_polygon() +
      coord_equal() +
      scale_fill_viridis(name='RAI') +
      theme_void() +
      theme(legend.position=c(0.05, 0.85)))
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
           aes_string(x = x_axis, y = "RAI")) +
      geom_point())
  })
  
  # render a reactive graph with RAI every month
  output$monthly_rai_hist <- renderPlotly({
    ggplotly(ggplot(data = (monthly_rai() %>% filter(Species == input$species_select, Camera == "All")),
           aes(x = Month_Year, y = RAI)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
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
