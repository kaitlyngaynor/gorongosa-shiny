library(broom)
library(rgdal)
library(tidyverse)
library(magrittr)

hexes <- readOGR("shapefile", "CameraGridHexes") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) 

# records and camera_operation use "Camera" not "StudySite" so this allows them to join
hexes@data %<>%
  rename(Camera = StudySite) 

# convert to dataframe
hexes.df <- broom::tidy(hexes, region = "Camera")

records <- read_csv("recordtable_allrecordscleaned_speciesmetadata.csv")
records$Date <- as.Date(records$Date)

# make a test RAI dataframe analagous to the reactive dataframe after species and date range is selected
RAI.test <- records %>% 
  rai.calculate(camera_operation_matrix, "2017-01-01", "2018-01-01") %>%
  filter(Species == "Waterbuck")



# try to add basemap
library(ggmap)
#register_google(key = "AIzaSyCeiiz8jQeVhriiIFF1jtaPavi9mEhuEDQ") # can't get Google API to work

sbbox <- make_bbox(lon = hexes.df$long, lat = hexes.df$lat, f = .1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "stamen")
plot(sq_map)


# merge camera DF with RAI
hexes.df.rai <- left_join(hexes.df, RAI.test, by = c("id" = "Camera"))


ggplotly(
    ggmap(sq_map) + 
    geom_polygon(hexes.df.rai, mapping = aes(long, lat, group = group, fill = RAI)) +
    coord_equal() +
    scale_fill_viridis(name='RAI') +
    theme_void() +
    theme(legend.position=c(0.05, 0.85))
)


# testing leaflet ---------------------------------------------------------

library(broom)
library(rgdal)
library(tidyverse)
library(magrittr)
library(leaflet)
library(sf)

# note that this isn't accurate because it includes ALL records and not just the ones within the subset dates; the rai.calculate function works with an already-subset record table
RAI.test <- records %>% 
  rai.calculate(camera_operation_matrix, "2017-01-01", "2018-01-01") %>%
  filter(Species == "Waterbuck")

# another way of importing
hexes <- read_sf("shapefile", "CameraGridHexes") %>%
  rename(Camera = StudySite) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# join with RAI
hexes_rai <- full_join(hexes, RAI.test)

# color brewer
#pal <- reactive({
pal <-  colorNumeric(palette = "viridis", domain = hexes_rai$RAI)
#})

# create map labels
#map_labels <- reactive({
map_labels <-  sprintf(
    "<strong>Camera: %s</strong><br/>Days operating: %i<br/>Detections: %i<br/>RAI: %g",
    hexes_rai$Camera, hexes_rai$Operation, hexes_rai$Count, hexes_rai$RAI, 0) %>% lapply(htmltools::HTML)
#})

leaflet(hexes_rai) %>%
  setView(34.42, -18.95, 11) %>%
  addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
  addPolygons(
    data = hexes_rai,
    fillColor = ~pal(hexes_rai$RAI),
    fillOpacity = 1, 
    weight = 1, # stroke weight of lines
    color = "gray", # color of lines
    label = map_labels,
    highlight = highlightOptions(
      weight = 2,
      color = "white",
      fillOpacity = 1,
      bringToFront = TRUE)
    ) %>% 
  addLegend_decreasing(pal = pal, 
            values = ~RAI,
            opacity = 1, 
            title = "RAI",
            position = "topleft",
            decreasing = TRUE)




