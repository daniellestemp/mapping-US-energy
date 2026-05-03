################################################################################
# geocode_to_geopackage.R
#
# Geocodes two datasets to US Census TIGER geometries and writes them as
# GeoPackage files using the same CRS and schema conventions as the reference
# county_poverty_rates_ACS5_2023.gpkg (ESRI:102003 – USA Contiguous Albers
# Equal Area Conic; GEOID, STUSPS, state_fips, county_fips attributes).
#
# Inputs  (set paths in the CONFIG section below):
#   1. us_data_centers_BI_2025_*.csv  – columns: brand, state, county,
#                                       est_pwr_use_low, est_pwr_use_high
#   2. us_res_electricity_bill_*.csv  – columns: state (full name),
#                                       2020, 2021, 2022, 2023, 2024, 2025
#
# Outputs:
#   1. us_data_centers_geocoded.gpkg   – one row per brand × county combo,
#                                        geometry = county polygon centroid
#                                        (POINT, ESRI:102003)
#   2. us_electricity_geocoded.gpkg    – one row per state,
#                                        geometry = state polygon
#                                        (MULTIPOLYGON, ESRI:102003)
#
# Census geometry source:
#   tigris::counties() / tigris::states() — downloads from the US Census
#   Bureau TIGER/Line shapefiles (requires internet on first run; cached
#   afterwards via the tigris cache mechanism).
#
# Requirements:
#   install.packages(c("tidyverse", "sf", "tigris", "dplyr"))
################################################################################


# ── 0. PACKAGES ──────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, readr, stringr, tidyr
  library(sf)          # spatial operations and GeoPackage I/O
  library(tigris)      # Census TIGER geometry downloads
})

options(tigris_use_cache = TRUE)   # cache downloaded shapefiles locally


# ── 1. CONFIG ────────────────────────────────────────────────────────────────

setwd('/Users/daniellestemper/Desktop/4.15.26 data')

# Edit these paths to match your local file locations
DATA_CENTERS_CSV  <- "us_data_centers_BI_2025_-_Copy_of_us_data_centers__2_.csv"
ELECTRICITY_CSV   <- "us_res_electricity_bill_-_state_2020-25_average__3_.csv"

OUT_DATA_CENTERS  <- "us_data_centers_geocoded.gpkg"
OUT_ELECTRICITY   <- "us_electricity_geocoded.gpkg"

# Target CRS: ESRI:102003 – USA Contiguous Albers Equal Area Conic
# (matches the reference county_poverty_rates_ACS5_2023.gpkg)
TARGET_CRS <- "ESRI:102003"


# ── 2. HELPER: STATE ABBREVIATION ↔ FIPS LOOKUP ─────────────────────────────

# Built-in R dataset `state.abb`, `state.name`, `state.fips` don't cover DC,
# so we build a comprehensive lookup table manually.

state_lookup <- tibble(
  state_name = c(state.name, "District of Columbia"),
  state_abb  = c(state.abb,  "DC"),
  state_fips = c(
    "01","02","04","05","06","08","09","10","12","13",   # AL-GA
    "15","16","17","18","19","20","21","22","23","24",   # HI-MD
    "25","26","27","28","29","30","31","32","33","34",   # MA-NJ
    "35","36","37","38","39","40","41","42","44","45",   # NM-SC
    "46","47","48","49","50","51","53","54","55","56",   # SD-WY
    "11"                                                  # DC
  )
)


# ── 3. DOWNLOAD CENSUS TIGER GEOMETRIES ──────────────────────────────────────

message("Downloading TIGER county geometries from US Census Bureau …")
counties_sf <- tigris::counties(
  cb    = TRUE,   # use cartographic boundary (generalised, smaller file)
  year  = 2023,
  class = "sf"
) |>
  select(
    GEOID,           # 5-digit FIPS  (state + county)
    NAME,            # county name (no " County" suffix)
    STUSPS,          # state abbreviation
    state_fips = STATEFP,
    county_fips = COUNTYFP
  ) |>
  st_transform(TARGET_CRS)

message("Downloading TIGER state geometries from US Census Bureau …")
states_sf <- tigris::states(
  cb    = TRUE,
  year  = 2023,
  class = "sf"
) |>
  select(
    GEOID      = GEOID,       # 2-digit state FIPS
    NAME       = NAME,        # full state name
    STUSPS     = STUSPS,      # abbreviation
    state_fips = STATEFP
  ) |>
  st_transform(TARGET_CRS)


# ── 4. DATA CENTERS → COUNTY-LEVEL GPKG ──────────────────────────────────────

message("Processing data centers …")

dc_raw <- read_csv(DATA_CENTERS_CSV, show_col_types = FALSE)

# Normalise county name: strip trailing " County", " Parish", etc. to match
# the NAME field in TIGER (which stores only the base name, e.g. "Jackson"
# not "Jackson County").
strip_county_suffix <- function(x) {
  str_remove(x, regex(
    "\\s+(county|parish|borough|census area|municipality|city and borough)\\s*$",
    ignore_case = TRUE
  ))
}

dc_clean <- dc_raw |>
  mutate(
    county_join = strip_county_suffix(county),
    state_abb   = str_to_upper(str_trim(state))
  )

# Join to TIGER counties on (state abbreviation, normalised county name)
dc_joined <- dc_clean |>
  left_join(
    counties_sf |>
      st_drop_geometry() |>
      mutate(county_join = NAME),          # NAME already stripped in TIGER
    by = c("state_abb" = "STUSPS", "county_join" = "county_join")
  )

# Report unmatched rows so the user can investigate
unmatched_dc <- dc_joined |>
  filter(is.na(GEOID)) |>
  distinct(state_abb, county) |>
  arrange(state_abb, county)

if (nrow(unmatched_dc) > 0) {
  message(sprintf(
    "WARNING: %d data-center county name(s) could not be matched to TIGER:\n%s",
    nrow(unmatched_dc),
    paste0("  ", unmatched_dc$state_abb, " / ", unmatched_dc$county, collapse = "\n")
  ))
}

# Compute county centroids and attach; drop rows with no geometry
county_centroids <- counties_sf |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  select(GEOID, centroid)

dc_geo <- dc_joined |>
  filter(!is.na(GEOID)) |>
  left_join(county_centroids, by = "GEOID") |>
  # Build the final attribute set matching the reference schema pattern
  select(
    brand,
    state_abb,
    county_name  = county,
    GEOID,
    NAME,
    STUSPS,
    state_fips,
    county_fips,
    est_pwr_use_low,
    est_pwr_use_high,
    geom         = centroid        # POINT geometry (centroid of county polygon)
  ) |>
  st_as_sf(sf_column_name = "geom")

st_crs(dc_geo) <- st_crs(TARGET_CRS)

message(sprintf("Writing %d rows to %s …", nrow(dc_geo), OUT_DATA_CENTERS))
st_write(dc_geo, OUT_DATA_CENTERS, layer = "us_data_centers",
         driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)


# ── 5. ELECTRICITY → STATE-LEVEL GPKG ────────────────────────────────────────

message("Processing electricity rates …")

elec_raw <- read_csv(ELECTRICITY_CSV, show_col_types = FALSE)

# The electricity data contains "South Atlantic" (a Census division, not a
# single state).  We keep it in the attribute table but cannot attach a single
# state geometry to it; those rows will be flagged with NA geometry and
# excluded from the spatial layer.
non_states <- c("South Atlantic")   # extend this vector if needed

elec_clean <- elec_raw |>
  mutate(state_name_clean = str_trim(state)) |>
  # Attach state abbreviation and FIPS from our lookup
  left_join(state_lookup, by = c("state_name_clean" = "state_name"))

unmatched_elec <- elec_clean |>
  filter(is.na(state_abb), !state_name_clean %in% non_states) |>
  pull(state_name_clean)

if (length(unmatched_elec) > 0) {
  message(sprintf(
    "WARNING: electricity rows not matched to a US state:\n%s",
    paste0("  ", unmatched_elec, collapse = "\n")
  ))
}

if (any(elec_clean$state_name_clean %in% non_states)) {
  message(sprintf(
    "NOTE: Non-state row(s) found and excluded from spatial layer: %s",
    paste(non_states[non_states %in% elec_clean$state_name_clean], collapse = ", ")
  ))
}

# Join to TIGER state polygons
elec_geo <- elec_clean |>
  filter(!state_name_clean %in% non_states, !is.na(state_abb)) |>
  left_join(
    states_sf,
    by = c("state_fips" = "state_fips")
  ) |>
  # Build final attribute set
  select(
    NAME,                        # full state name (from TIGER)
    STUSPS,                      # abbreviation   (from TIGER)
    GEOID,                       # 2-digit state FIPS (from TIGER)
    state_fips,
    avg_rate_2020 = `2020`,
    avg_rate_2021 = `2021`,
    avg_rate_2022 = `2022`,
    avg_rate_2023 = `2023`,
    avg_rate_2024 = `2024`,
    avg_rate_2025 = `2025`,
    geometry
  ) |>
  st_as_sf()

st_crs(elec_geo) <- st_crs(TARGET_CRS)

message(sprintf("Writing %d rows to %s …", nrow(elec_geo), OUT_ELECTRICITY))
st_write(elec_geo, OUT_ELECTRICITY, layer = "us_electricity",
         driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)


# ── 6. VALIDATION SUMMARY ────────────────────────────────────────────────────

cat("\n──────────────────────────────────────────────────\n")
cat("Output summary\n")
cat("──────────────────────────────────────────────────\n")

cat(sprintf("\n[1] %s\n", OUT_DATA_CENTERS))
cat(sprintf("    Layer  : us_data_centers\n"))
cat(sprintf("    Rows   : %d\n", nrow(dc_geo)))
cat(sprintf("    CRS    : %s\n", st_crs(dc_geo)$input))
cat(sprintf("    Geom   : %s (county centroids)\n", unique(st_geometry_type(dc_geo))))
cat(sprintf("    Fields : %s\n", paste(setdiff(names(dc_geo), "geom"), collapse = ", ")))

cat(sprintf("\n[2] %s\n", OUT_ELECTRICITY))
cat(sprintf("    Layer  : us_electricity\n"))
cat(sprintf("    Rows   : %d\n", nrow(elec_geo)))
cat(sprintf("    CRS    : %s\n", st_crs(elec_geo)$input))
cat(sprintf("    Geom   : %s (state polygons)\n",
            paste(unique(st_geometry_type(elec_geo)), collapse = "/")))
cat(sprintf("    Fields : %s\n",
            paste(setdiff(names(elec_geo), "geometry"), collapse = ", ")))

cat("\nDone.\n")
