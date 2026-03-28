### Lab 2 - Info-609 ###
### Author: Danielle Stemper
### Pratt Institute | Spring 2026

# Load packages
library(tidyverse)  ## collection of data science packages
library(sf)  ## spatial data handling and geometric operations
library(tigris)
library(dplyr)
library(tibble)

# Set working directory
setwd("~/Desktop/609_lab2")

### Energy Price Data ###

# Load data by creating an object with public link to data (stored in Google Sheets)
energy_path <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTTs4tKW9KBiVbJMXVtZNOjN1kJ3SCro0a_UayEMNco6aPhiOO5EYmNTtXeSfSUEUu9SvK--7aau1EV/pub?gid=1866048950&single=true&output=csv"
energy <- read_csv(energy_path)

# Get US state geometries (polygons)
states_sf <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Join energy data to state geometries by name
energy_geo <- states_sf %>%
  left_join(energy, by = c("NAME" = "state"))

# Add centroids for point coordinates
energy_geo <- energy_geo %>%
  mutate(centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

# Rename energy price columns from "year" to "price_year" for clarity
energy_geo <- energy_geo %>%
  rename_with(~ paste0("p_", .), starts_with("20"))

# Create calculated field: percent change in energy price 2020-2025
energy_geo <- energy_geo %>%
  mutate(pct_change = ((p_2025 - p_2020) / p_2020) * 100)

# Create calculated field: volatility (standard deviation across years)
energy_geo <- energy_geo %>%
  mutate(volatility = apply(pick(p_2020, p_2021, p_2022, p_2023, p_2024, p_2025), 1, sd))

# Export geocoded energy price dataframe as a shapefile and csv for records
st_write(energy_geo, "energy_geo.shp")
write_csv(energy_geo, "energy_geo.csv")

### Data Center Data ###

# Load data by creating an object with public link to data (stored in Google Sheets)
dc_path <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTZy6wyLy_5ak8IqVRU6Y6Il53E6fTIJE_8GREgLich5Je8Gk9oJzhTWTpgu_pdeO5PUE2Myx52yeoj/pub?gid=1459013728&single=true&output=csv"
dc <- read_csv(dc_path)

# Convert state abbreviations to full state name
abb_to_name <- c(setNames(state.name, state.abb), "DC" = "District of Columbia")

dc <- dc %>%
  mutate(state = abb_to_name[state])

# Create new column with unique row identifier
dc <- dc %>%
  rowid_to_column("id")

# Create calculated column with average energy usage
dc_geo <- dc_geo %>%
  mutate(est_pwr_use_avg = rowMeans(pick(est_pwr_use_low, est_pwr_use_high), na.rm = TRUE))


# Get all US country geometries
counties_sf <- counties(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Join datasets
dc_geo <- counties_sf %>%
  left_join(dc, by = c("STATE_NAME" = "state", "NAME" = "county"),
            relationship = "many-to-many")

dc_geo <- dc_geo %>%
  mutate(centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

# Export geocoded data center location dataframe as a shapefile
st_write(dc_geo, "dc_geo.shp")
write_csv(dc_geo, "dc_geo.csv")

# map data center locations

ggplot(dc_geo) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.1) +
  geom_point(
    data = dc_geo %>% filter(!is.na(brand)),
    aes(x = lon, y = lat),
    color = "blue", size = 1.5, alpha = 0.7
  ) +
  labs(
    title = "U.S. Data Centers by County",
    caption = "Source: Your dataset"
  ) +
  theme_void()

###
####
#### Map: state-level energy price % change (2020–2025) with data center locations overlaid
####

ggplot() +
  # Layer 1: state choropleth shaded by pct_change
  geom_sf(
    data = energy_geo,
    aes(fill = pct_change),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "lightyellow",
    high = "firebrick",
    midpoint = median(energy_geo$pct_change, na.rm = TRUE),
    name = "% Change\n(2020–2025)",
    na.value = "grey80"
  ) +
  # Layer 2: county borders for context (no fill)
  geom_sf(
    data = dc_geo,
    fill = NA,
    color = "white",
    linewidth = 0.05,
    alpha = 0.3
  ) +
  # Layer 3: data center point locations
  geom_point(
    data = dc_geo %>% filter(!is.na(brand)),
    aes(x = lon, y = lat),
    color = "black",
    size = .75,
    alpha = 0.7
  ) +
  labs(
    title = "U.S. Data Center Locations vs. Energy Price Change (2020–2025)",
    subtitle = "State shading = % change in electricity price; dots = data center locations",
    caption = "Sources: EIA; Business Insider"
  ) +
  theme_void() +
  theme(legend.position = "right")

####
#### Map: state-level price volatility (2020-2025) with data center locations overlaid
####

ggplot() +
  # Layer 1: state choropleth shaded by volatility
  geom_sf(
    data = energy_geo,
    aes(fill = volatility),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "firebrick",
    name = "Volatility\n(Std Dev)",
    na.value = "grey80"
  ) +
  # Layer 2: county borders for context (no fill)
  geom_sf(
    data = dc_geo,
    fill = NA,
    color = "white",
    linewidth = 0.05,
    alpha = 0.3
  ) +
  # Layer 3: data center point locations
  geom_point(
    data = dc_geo %>% filter(!is.na(brand)),
    aes(x = lon, y = lat),
    color = "black",
    size = .75,
    alpha = 0.7
  ) +
  labs(
    title = "U.S. Data Center Locations vs. Energy Price Volatility (2020–2025)",
    subtitle = "State shading = std dev of electricity price; dots = data center locations",
    caption = "Sources: EIA; Business Insider"
  ) +
  theme_void() +
  theme(legend.position = "right")

####
#### Map: estimated average power use 
####

ggplot() +
  # Layer 1: county choropleth shaded by avg estimated power use
  geom_sf(
    data = dc_geo,
    aes(fill = est_pwr_use_avg),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "firebrick",
    name = "Avg Est.\nPower Use (MW)",
    na.value = "grey90"
  ) +
  # Layer 2: data center point locations
  geom_point(
    data = dc_geo %>% filter(!is.na(brand)),
    aes(x = lon, y = lat),
    color = "black",
    size = .5,
    alpha = 0.7
  ) +
  labs(
    title = "U.S. Data Center Locations vs. Avg Estimated Power Use by County",
    subtitle = "County shading = avg of low/high est. power use; dots = data center locations",
    caption = "Source: Your data center dataset"
  ) +
  theme_void() +
  theme(legend.position = "right")

####
#### Leaflet Map test:
####

library(leaflet)
library(sf)

# Transform both datasets to WGS84 (required for leaflet)
energy_geo_wgs <- st_transform(energy_geo, 4326)
dc_geo_wgs <- st_transform(dc_geo, 4326)

# Re-extract lon/lat after reprojection for accurate marker placement
dc_points <- dc_geo_wgs %>%
  filter(!is.na(brand)) %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[,1],
    lat = st_coordinates(st_centroid(geometry))[,2]
  )

# Define color palette for volatility
pal <- colorNumeric(
  palette = c("lightyellow", "firebrick"),
  domain = energy_geo_wgs$volatility,
  na.color = "grey80"
)

# Build leaflet map
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Layer 1: state choropleth shaded by volatility
  addPolygons(
    data = energy_geo_wgs,
    fillColor = ~pal(volatility),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>", NAME, "</b><br>",
      "Volatility (Std Dev): ", round(volatility, 2)
    )
  ) %>%
  # Layer 2: county borders for context
  addPolygons(
    data = dc_geo_wgs,
    fillColor = NA,
    fillOpacity = 0,
    color = "white",
    weight = 0.3,
    options = pathOptions(interactive = FALSE)
  ) %>%
  # Layer 3: data center markers
  addCircleMarkers(
    data = dc_points,
    lng = ~lon,
    lat = ~lat,
    radius = 4,
    color = "black",
    fillColor = "black",
    fillOpacity = 0.7,
    weight = .75,
    popup = ~paste0(
      "<b>", brand, "</b><br>",
      NAME, ", ", STATE_NAME
    )
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = energy_geo_wgs$volatility,
    title = "Volatility<br>(Std Dev)",
    opacity = 0.7
  )

#####
###### Average Price
#####

# First, calculate average price across all years
energy_geo <- energy_geo %>%
  mutate(avg_price = rowMeans(pick(p_2020, p_2021, p_2022, p_2023, p_2024, p_2025), na.rm = TRUE))

# Transform both datasets to WGS84 (required for leaflet)
energy_geo_wgs <- st_transform(energy_geo, 4326)
dc_geo_wgs <- st_transform(dc_geo, 4326)

# Re-extract lon/lat after reprojection for accurate marker placement
dc_points <- dc_geo_wgs %>%
  filter(!is.na(brand)) %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[,1],
    lat = st_coordinates(st_centroid(geometry))[,2]
  )

# Define color palette for avg_price
pal <- colorNumeric(
  palette = c("lightyellow", "firebrick"),
  domain = energy_geo_wgs$avg_price,
  na.color = "grey80"
)

# Build leaflet map
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Layer 1: state choropleth shaded by avg_price
  addPolygons(
    data = energy_geo_wgs,
    fillColor = ~pal(avg_price),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>", NAME, "</b><br>",
      "Avg Energy Price (2020–2025): ", round(avg_price, 2), " cents/kWh"
    )
  ) %>%
  # Layer 2: county borders for context
  addPolygons(
    data = dc_geo_wgs,
    fillColor = NA,
    fillOpacity = 0,
    color = "white",
    weight = 0.3,
    options = pathOptions(interactive = FALSE)
  ) %>%
  # Layer 3: data center markers
  addCircleMarkers(
    data = dc_points,
    lng = ~lon,
    lat = ~lat,
    radius = 4,
    color = "black",
    fillColor = "black",
    fillOpacity = 0.7,
    weight = 1,
    popup = ~paste0(
      "<b>", brand, "</b><br>",
      NAME, ", ", STATE_NAME
    )
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = energy_geo_wgs$avg_price,
    title = "Avg Energy Price<br>(cents/kWh)",
    opacity = 0.7
  )
