#### Open packages
library(tidyverse)
library(tidytransit)
library(here)
library(ggthemes)
library(tigris)

#### Displaying transit routes in R
jma_county = c("031", "019", "003", "089", "109")

jma_tracts <- tracts(state = "FL", county = jma_county)

jma_gtfs <- read_gtfs("/Users/limengyao/Desktop/R/tbf_jacksonville/data/jta-gtfs.zip")

route_shape <- shapes_as_sf(jma_gtfs$shapes)

ggplot() +
  geom_sf(data = jma_tracts,
          fill = "cornsilk", color = "gray", size = 0.3) +
  geom_sf(data = route_shape,
          aes(color = shape_id)) +
  theme_map() +
  theme(legend.position = "none")

ggsave("images/public_transit_map.jpg", height = 4.25, width = 6.5, units = "in")

#### Importing Excel sheets into R
library(realxl)

IVTT <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "In-Vehicle Time") 










