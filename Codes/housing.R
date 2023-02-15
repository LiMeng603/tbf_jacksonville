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
library(lwgeom)
library(treemapify)
library(leaflet)
library(htmlwidgets)
library(here)

# Downloading Census Data
## view(load_variables(2021, "acs5"))

hh_vars = c(total_pop = "B01003_001",
            total_hhs = "B08201_001",
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

census$area <- st_area(census)
census$pop_den = census$total_popE/census$area
census$pop_den1 = as.vector(census$pop_den)*10^6

write_csv(census, "data/jta-census.csv")

# Household number
sum(census$total_hhsE)
mean(census$total_hhsE)
median(census$total_hhsE)
max(census$total_hhsE)

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
ggsave("images/hh_number_map.jpg", height = 4.25, width = 6.5, units = "in")

hh_number_hist <- ggplot(census, 
                         aes(x = total_hhsE)) +
  geom_histogram() +
  scale_x_continuous(name = "Number of Total Households")
hh_number_hist
ggsave("images/hh_number_hist.jpg", height = 4.25, width = 6.5, units = "in")

## In total, there are 603,305 households. 

# Household vehicle availability
sum(census$no_vehE)
mean(census$no_vehE)
median(census$no_vehE)
max(census$no_vehE)

## Tree Map
vars <- c(`Zero vehicles` = 'B08201_002',
          `One vehicle` = 'B08201_003',
          `Two vehicles` = 'B08201_004',
          `Three vehicles` = 'B08201_005',
          `Four or more vehicles` = 'B08201_006')

total_hhs <- 'B08201_001'

region_vehs <- get_acs(geography = "cbsa", variables = vars, summary_var = total_hhs, geometry = FALSE) %>%
  filter(NAME == "Jacksonville, FL Metro Area") %>%
  mutate(pct = estimate / summary_est) %>%
  select(variable, pct)

ggplot(region_vehs, aes(area = pct, fill= variable)) +
  geom_treemap(show.legend = FALSE, color = NA) +
  geom_treemap_text(aes(label = paste(variable, "\n",
                                      prettyNum(pct * 100, digits = 1),
                                      "%",sep = "")), 
                    color = "white") +
  scale_fill_brewer(palette = "Set2")
ggsave("images/hh_vehicle_treemap.jpg", height = 4.25, width = 6.5, units = "in")

## Map of number of households without vehicles
hh_vehicle_map <- ggplot(census) +
  geom_sf(aes(fill = no_vehE), color=NA) +
  scale_fill_viridis_c(
    name = "Number of\nHouseholds\nwithout Vehicles",
    breaks = seq(0, 600, by = 100)) +
  theme_map() +
  theme(legend.background = element_blank())
hh_vehicle_map
ggsave("images/hh_vehicle_map.jpg", height = 4.25, width = 6.5, units = "in")

hh_vehicle_hist <- ggplot(census,
                          aes(x = no_vehE)) +
  geom_histogram() +
  scale_x_continuous(name = "Number of Households without Vehicles")
hh_vehicle_hist
ggsave("images/hh_vehicle_hist.jpg", height = 4.25, width = 6.5, units = "in")

# Household income 
sum(census$inc_lt_10kE)
mean(census$inc_lt_10kE)
median(census$inc_lt_10kE)
max(census$inc_lt_10kE)

hh_income_lt_10k_map <- ggplot(census) +
  geom_sf(aes(fill = inc_lt_10kE), color=NA) +
  scale_fill_viridis_c(
    name = "Number of Households\nEarning less than 10K") +
  theme_map() +
  theme(legend.background = element_blank())
hh_income_lt_10k_map
ggsave("images/hh_income_lt_10k_map.jpg", height = 4.25, width = 6.5, units = "in") 

hh_income_lt_10k_hist <- ggplot(census,
                          aes(x = inc_lt_10kE)) +
  geom_histogram() +
  scale_x_continuous(name = "Number of Households Earning less than 10K")
hh_income_lt_10k_hist
ggsave("images/hh_income_lt_10k_hist.jpg", height = 4.25, width = 6.5, units = "in")

sum(census$inc_gt_200kE)
mean(census$inc_gt_200kE)
median(census$inc_gt_200kE)
max(census$inc_gt_200kE)

hh_income_gt_200k_map <- ggplot(census) +
  geom_sf(aes(fill = inc_gt_200kE), color=NA) +
  scale_fill_viridis_c(
    name = "Number of Households\nEarning more than 200K") +
  theme_map() +
  theme(legend.background = element_blank())
hh_income_gt_200k_map
ggsave("images/hh_income_gt_200k_map.jpg", height = 4.25, width = 6.5, units = "in") 

hh_income_gt_200k_hist <- ggplot(census,
                                aes(x = inc_gt_200kE)) +
  geom_histogram() +
  scale_x_continuous(name = "Number of Households Earning more than 200K")
hh_income_gt_200k_hist
ggsave("images/hh_income_gt_200k_hist.jpg", height = 4.25, width = 6.5, units = "in")

# Household size
sum(census$hh_1personE)
sum(census$hh_2personE)
sum(census$hh_3personE)
sum(census$hh_4person_plusE)

hh_1person_pts <- st_sample(census, 
                            size = ceiling(census$hh_1personE/100))
hh_2person_pts <- st_sample(census, 
                            size = ceiling(census$hh_2personE/100))
hh_3person_pts <- st_sample(census, 
                            size = ceiling(census$hh_3personE/100))
hh_4person_pts <- st_sample(census, 
                            size = ceiling(census$hh_4person_plusE/100))

hh_1person_df <- tibble(hh_size = rep("1 person", 
                                      length(hh_1person_pts))) %>%
  st_sf(geom = hh_1person_pts)
hh_2person_df <- tibble(hh_size = rep("2 people", 
                                      length(hh_2person_pts))) %>%
  st_sf(geom = hh_2person_pts)
hh_3person_df <- tibble(hh_size = rep("3 people", 
                                      length(hh_3person_pts))) %>%
  st_sf(geom = hh_3person_pts)
hh_4person_df <- tibble(hh_size = rep("4 or more people", 
                                      length(hh_4person_pts))) %>%
  st_sf(geom = hh_4person_pts)
hh_size_pts <- rbind(hh_1person_df, hh_2person_df, hh_3person_df, hh_4person_df)

ggplot(census) + 
  geom_sf(color = "white") +
  geom_sf(data = hh_size_pts, 
          aes(color = hh_size), 
          alpha = 0.3,
          size = 0.1) +
  scale_color_brewer("Household size\n(each points represents\n100 households)",
                     palette = "Set1") +
  theme_void()  +
  guides(color = guide_legend(override.aes = list(size=5, alpha = 0.6)))
ggsave("images/hh_size.jpg", height = 4.25, width = 6.5, units = "in")

# Population density
sum(census$total_popE)
sum(census$area)

pop_den_map <- ggplot(census) +
  geom_sf(aes(fill = pop_den1), color=NA) +
  scale_fill_viridis_c(option = "A",
    name = "Population Density\n(People Per Square Kilometer)") +
  theme_map() +
  theme(legend.background = element_blank())
pop_den_map
ggsave("images/pop_den_map.jpg", height = 4.25, width = 6.5, units = "in") 

# Create a Leaflet map
map <- census %>%
  st_transform("WGS84") %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup = ~GEOID,
              weight = 1,
              opacity = 1,
              highlightOptions =
                highlightOptions(fillColor = "red"))

saveWidget(map, file = here('boundaries.html'))

















