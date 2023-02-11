#### Open packages
library(tidyverse)
library(tidytransit)
library(here)
library(ggthemes)
library(tigris)

#### Displaying transit routes in R
jma_county = c("031", "019", "003", "089", "109")

jma_tracts <- tracts(state = "FL", county = jma_county)

jma_rta_gtfs <- here("data",
                     "jma_gtfs1.zip") %>%
  read_gtfs()