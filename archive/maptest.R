library(broom)
library(rgdal)

hexes <- readOGR("shapefile", "CameraGridHexes")

# convert to dataframe
hexes.df <- broom::tidy(hexes, region = "Camera")

# make a test RAI dataframe analagous to the reactive dataframe after species and date range is selected
RAI.test <- records %>% 
  rai.calculate(camera_operation_matrix, "2017-01-01", "2018-01-01") %>%
  filter(Species == "Waterbuck")

