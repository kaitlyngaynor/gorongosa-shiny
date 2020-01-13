#' 
#' Map Card Module
#'
#' This script contains two functions:
#' \code{map_ui} generates the user interface for each card
#' \code{card_map} generates the plot shown in a card
#' 
#' The functions together generate a box within a fluid row that has a width 
#' of 12 (the whole page), title text, source text, a select input if applicable, 
#' and an interactive map built using leaflet. 
#' 

## Load libraries ##
library(shiny)
library(shinydashboard)
library(leaflet)

#'
#' @title map_ui function
#' 
#' @description Generates the ui for cards.
#' 
#' @return A fluidRow containing a tagList and a box with items specified by 
#' the function inputs.
#' 
#' @details
#' \code{map_ui} needs the text that will surround the map on 
#' the card as well as information about the select inputs if 
#' applicable.
#' 
#' @param id character. Used to match to ui function to the associated
#' server function. Must be unique across cards.
#' @param title_text character, optional. Title text for map.
#' @param sub_title_text character, optional. Subtitle text for map if applicable.
#' @param select_type character, optional.  Determines the type of shiny input to be used. 
#' Must be one of the following:
#' \itemize{
#'   \item "radio"
#'   \item "drop_down"
#'   \item "checkboxes"
#'   \item NULL
#' }
#' @seealso \url{https://shiny.rstudio.com/reference/shiny/latest/selectInput.html}, 
#' @seealso \url{https://shiny.rstudio.com/reference/shiny/latest/checkboxInput.html},
#' @seealso \url{https://shiny.rstudio.com/reference/shiny/latest/radioButtons.html}
#' @param select_location character, optional. Determines if the select input will
#' be located above or below the map. Must be one of the following:
#' \itemize{
#'   \item "above"
#'   \item "below"
#'   \item NULL
#' } 
#' @param select_choices list, optional. List of choices for the select input. Can be 
#' defined in the global.r file as a variable in passed here 
#' (e.g. in global.r: choices <- sort(unique(df$column)), in ui.r: select_choices=choices).
#' Can also be a defined list (e.g. select_choices = list("Choice 1" = "choice_1", "Choice 2" = "choice_2")). 
#' See \url{https://shiny.rstudio.com/reference/shiny/latest/selectInput.html}, for more information.
#' @param select_label character, optional, text for above select input (e.g. "Select Gender")
#' @param selected list, optional - unless using checkbox input, Defines the default selections 
#' for the map. Default checkbox inputs must be defined otherwise the map will default to
#' no data.
#' @param source_text character or list, optional. Data source information for map. If 
#' additional notes need to be added beyond data source information below the map 
#' they can be added by passing a list or html elements to this function 
#' (e.g. source_text = list(p("Note 1), p("Note 2"), p("Data Source: 2010 Census")))
#' 

## Card UI Function ##
map_ui <- function(id, 
                   title_text = "Relative Activity Index across cameras",
                   sub_title_text = "RAI for selected species and date range for each camera grid cell. Click on a grid cell to see the score. Areas with no color indicate inoperable cameras",
                   select_type = c(NULL, "radio", "drop_down", "checkboxes"),
                   select_location = c(NULL, "above", "below"),
                   select_choices = c(""),
                   select_label = NULL, 
                   selected = NULL,
                   source_text = "An empty region indicates that the camera was inoperable for this date range and so no RAI value was calculated") {
  
  ns <- NS(id)
  
  # Output w/o selection
  if (missing(select_type) == TRUE) {
    items <- leafletOutput(ns("plot"), height=450)
  } 
  else {
    # Select layout
    if (select_type == "radio") {
      select <- radioButtons(ns("select"),
                             choices = select_choices,
                             label = p(select_label),
                             selected = selected,
                             inline = TRUE)
    } else if (select_type == "drop_down") {
      select <- selectInput(ns("select"),
                            choices = select_choices,
                            label = p(select_label),
                            selected = selected)
    } else {
      select <- checkboxGroupInput(ns("select"),
                                   choices = select_choices,
                                   label = p(select_label),
                                   selected = selected)
    }
    
    # Chart layout
    if (select_location == "above") {
      items <- list(select,
                    leafletOutput(ns("plot"), height=450))
    } else if (select_location == "below") {
      items <- list(leafletOutput(ns("plot"), height=450),
                    select)
    }
  }
  
  # Put together in box
  box_content <- list(h4(title_text), p(sub_title_text), items, p(source_text))
  
  # return box
  tagList(box(box_content))
}

#'
#' @title card_map function
#' 
#' @description Generates the leaflet map on each card.
#' 
#' @return A leaflet map object.
#' 
#' @details
#' \code{card_map} needs the data, fields, colors, and labels for the map
#' 
#' @param input autofilled by shiny.
#' @param output autofilled by shiny.
#' @param session character, must be the exact same as the id passed to the card_ui function.
#' @param data name of dataset to be mapped by the OHI regions. 
#' @param field character, this field is set up to handle one of two scenarios - the map changes
#' based on user input or it doesn't.
#' \itemize{
#'   \item{If there is an input, this field is set to "input" and the field the user selects will be used
#'   to select the data.}
#'   \item{If there is no input, this field is set to the name of the column being visualized.}
#' }
#' @param filter_field column name, this field defines the column on which to filter. Use this when `field` is set to "input"
#' @param display_field column name, this field defines the column containing data to display
#' @param color_palette character, defines the color palette for the map. The palette options come 
#' from the RColorBrewer library, and the options can be viewed here: \url{http://www.datavis.ca/sasmac/brewerpal.html}
#' @param legend_title character, optional. Defines the legend title for the map.
#' @param popup_title character, optional. Defines the popup title for the map.
#' @param popup_add_field column name, optional. Option to put another field in the popups. The field
#' must be defined with the name of the dataframe and a "$" symbol (e.b. popup_add_field = tracts$neighborhood)
#' @param popup_add_field_title character, title text for additional pop-up field. 
#' 

## Card Server Function ##
card_map <- function(input,
                     output,
                     session,
                     data,
                     field,
                     filter_field = NULL,
                     display_field = NULL,
                     color_palette = myPalette,
                     legend_title = NULL,
                     popup_title = NA,
                     popup_add_field = NA,
                     popup_add_field_title = NA) {
  
  # attach data to camera hex shapefile
  data_shp <- hexes %>%
    full_join(data)
  
  # get color pal
  pal <- colorNumeric(palette = color_palette,
                      domain = c(100, 0),
                      na.color = "#00000000")
  
  if (field != "input") {
    output$plot <- renderLeaflet({
      
      # get popup
      popup_text <- paste("<h5><strong>", popup_title, "</strong>" , data_shp[[field]], "</h5>",
                          "<h5><strong>", popup_add_field_title, "</strong>", data_shp[[popup_add_field]], "</h5>", sep=" ")
      
      leaflet(data_shp,
              options = leafletOptions()) %>%
        addPolygons(color = "#A9A9A9", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 1,
                    fillColor = pal(data_shp[[field]]),
                    popup = popup_text, 
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE)) %>% 
        
        addLegend_decreasing("bottomright",
                             pal = pal,
                             values = c(0:100),
                             title = legend_title,
                             opacity = 0.85,
                             layerId = "colorLegend",
                             decreasing = TRUE) %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-70.0589, 41.5, zoom = 6)
    })
    
  } else {
    
    filter_field <- enquo(filter_field)
    
    selected_data <- reactive({
      
      df <- data_shp %>% filter(!!filter_field == input$select)
      
      return(df)
      
    })
    
    output$plot <- renderLeaflet({
      
      # get popup
      popup_text <- paste("<h5><strong>", popup_title, "</strong>" , selected_data()[[display_field]], "</h5>",
                          "<h5><strong>", popup_add_field_title, "</strong>", selected_data()[[popup_add_field]], "</h5>", sep=" ")
      
      leaflet(selected_data(),
              options = leafletOptions()) %>%
        addPolygons(color = "#A9A9A9", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 0.85,
                    fillColor = pal(selected_data()[[display_field]]),
                    popup = popup_text, 
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE)) %>% 
        addLegend("bottomright",
                  pal = pal,
                  values = c(0:100),
                  title = legend_title,
                  opacity = 0.85,
                  layerId = "colorLegend") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-70.0589, 41.5, zoom = 6)
    })
    
  }
  
}
