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
library(knitr)

fare <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Fare") 
IVTT <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "In-Vehicle Time") 
transfer_wait_time <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Transfer Wait Time") 
access_walk_time <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Access Walk Time") 
egress_walk_time <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Egress Walk Time") 
total_time <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Total Time") 
transfers <- read_xlsx("data/jta-rta-skims.xlsx", sheet = "Number of Transfers") 

fare_long <- fare %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, fare = value)

IVTT_long <- IVTT %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, IVTT = value)

transfer_wait_time_long <- transfer_wait_time %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, transfer_wait_time = value)

access_walk_time_long <- access_walk_time %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, access_walk_time = value)

egress_walk_time_long <- egress_walk_time %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, egress_walk_time = value)

total_time_long <- total_time %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, total_time = value)

transfers_long <- transfers %>%
  pivot_longer(cols = -GEOID) %>%
  mutate(value = as.numeric(value)) %>%
  rename(from_GEOID = GEOID, to_GEOID = name, n_transfers = value)

full_skim <- full_join(fare_long, IVTT_long) %>%
  full_join(transfer_wait_time_long) %>%
  full_join(access_walk_time_long) %>%
  full_join(egress_walk_time_long) %>%
  full_join(total_time_long) %>%
  full_join(transfers_long)

head(full_skim) %>%
  kable()

#### Visualization: Joining to census tracts
from_poor <- full_skim %>%
  filter(from_GEOID == "12031000600") %>%
  select(-from_GEOID) %>%
  rename(GEOID = to_GEOID)

from_poor_tracts <- left_join(jma_tracts, from_poor)

ggplot(from_poor_tracts) +
  geom_sf(aes(fill = total_time), size = 0.3) +
  geom_sf(data = route_shape, size = 0.3) +
  scale_fill_viridis_c(name = "Total travel time\nby transit from\nCensus Tract 6") +
  theme_map()

ggsave("images/from_poor.jpg", height = 4.25, width = 6.5, units = "in")







