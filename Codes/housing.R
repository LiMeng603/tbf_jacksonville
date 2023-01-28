### This file is to analyze Jacksonville Metropolitan Area (JMA)'s housing characteristics.
### We use census tracts as traffic analysis zones.
### JMA includes Duval (031), Clay (019), Baker (003), Nassau (089), and St. Johns (109) counties.
### In total, there are 340 census tracts. 

library(tidyverse)
library(tidycensus)
library(dplyr)
library(sf)
library(ggspatial)
library(cartogram)
library(ggthemes)

# Downloading Census Data
view(load_variables(2021, "acs5"))

hh_vars = c(total_hhs = "B08201_001",
            
            no_veh = "B08201_002",
            hh_1vehicle = "B08201_003",
            hh_2vehicle = "B08201_004",
            hh_3vehicle = "B08201_005",
            hh_4vehicle_plus = "B08201_006",
            
            inc_lt_10k = "B19001_002",
            inc_btw_10k_15k = "B19001_003",
            inc_btw_15k_20k = "B19001_004",
            inc_btw_20k_25k = "B19001_005",
            inc_btw_25k_30k = "B19001_006",
            inc_btw_30k_35k = "B19001_007",
            inc_btw_35k_40k = "B19001_008",
            inc_btw_40k_45k = "B19001_009",
            inc_btw_45k_50k = "B19001_010",
            inc_btw_50k_60k = "B19001_011",
            inc_btw_60k_75k = "B19001_012",
            inc_btw_75k_100k = "B19001_013",
            inc_btw_100k_125k = "B19001_014",
            inc_btw_125k_150k = "B19001_015",
            inc_btw_150k_200k = "B19001_016",
            inc_gt_200k = "B19001_017",
            
            hh_1person = "B08201_007",
            hh_2person = "B08201_013",
            hh_3person = "B08201_019",
            hh_4person_plus = "B08201_025")

jma_county = c("031", "019", "003", "089", "109")

census <- get_acs(geography = "tract",
                  year = 2021,
                  state = "FL",
                  county = jma_county,
                  variables = hh_vars,
                  output = "wide",
                  geometry = TRUE) %>%
  filter(!st_is_empty(geometry))

# Housing Characteristics
sum(census$total_hhsE)

hh_number_map <- ggplot(census) +
  geom_sf(aes(fill = total_hhsE), color=NA) +
  scale_fill_viridis_c(
    name = "Number of\nHouseholds",
    breaks = seq(0, 6000, by = 1000),
    labels = formatC(seq(0, 6000, by = 1000),
                     big.mark = ",", format = "f", digits = 0)) +
  theme_map() +
  theme(legend.background = element_blank())
hh_number_map

## In total, there are 603,305 households. 






























