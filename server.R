# Gorongosa Camera Trap Data Shiny App

# Server

server <- function(input, output, session) {
  
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
      filter(CommName_Full == input$species_select)
  })
  
  # create reactive object camera_subset that changes based on camera_select widget selection 
  camera_subset <- reactive({
    records_subset() %>%
      filter(Camera == input$camera_select)
  })
  
  # Summarize subset data ---------------------------------------------------
  
  
  # summarize species counts across cameras
  species_summary <- reactive({
    species_subset() %>%
      group_by(Camera) %>%
      summarise(count = n())
  })
  

  # summarize species counts across species
  camera_summary <- reactive({
    camera_subset() %>%
      group_by(CommName_Full) %>%
      summarise(count = n())
  })
  
  # calculate RAI
  rai <- reactive({
    rai.calculate(records_subset(), camera_operation_matrix, input$date_range1[1], input$date_range1[2])
  })


  # Map outputs -------------------------------------------------------------

  # not sure how to do this because the map should be reactive... hmmmmm. May need to try outside of map card.
  
  # richness map
#  callModule(
#    card_map,
#    "richness_map",
#    data = species_summary(),
#    field = "count",
#    legend_title = "Species Richness",
#    popup_title = "Count:",
#    popup_add_field = "Camera",
#    popup_add_field_title = ""
#  )
  
  # Render outputs ----------------------------------------------------------
  
  # render a reactive table that shows a summary by species
  output$species_table <- renderTable({
    species_summary()
  })
  
  # render a reactive table that shows a summary by camera
  output$camera_table <- renderTable({
    camera_summary()
  })

  # render a reactive table that shows RAI of selected species at each camera
  output$rai_table <- renderTable({
    rai() %>%
      filter(Species == input$species_select)
  })
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot <- renderPlot({
    timeplot(species_subset()$Time.Sun)
  })
  
}
