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



