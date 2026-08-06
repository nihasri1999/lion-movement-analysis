# ==============================================================================
# Script: 01_data_prep.R
# Purpose: Clean raw Movebank lion data, reproject CRS, and build amt tracks
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse) # Data wrangling & piping
library(lubridate) # Date-time parsing
library(sf)        # Spatial transformations
library(amt)       # Animal Movement Tools

# 2. Read Full Dataset
raw_file <- "data/raw/kalahari_lions.csv" 
lion_raw <- read_csv(raw_file)

# 3. Clean & Filter Data
lion_clean <- lion_raw %>%
  # Use backticks for headers containing hyphens or colons
  filter(!is.na(`location-long`), !is.na(`location-lat`)) %>%
  filter(visible == TRUE) %>%
  # Rename columns to clean, standard names
  select(
    animal_id = `individual-local-identifier`,
    date_time = timestamp,
    lon       = `location-long`,
    lat       = `location-lat`,
    dop       = `gps:dop`
  ) %>%
  # Parse/ensure date_time is POSIXct UTC safely without dropping midnight fixes
  mutate(date_time = as_datetime(date_time, tz = "UTC")) %>%
  # Arrange chronologically per animal
  arrange(animal_id, date_time)

# 4. Convert to Spatial sf Object & Reproject to UTM Zone 34S
# Kalahari Game Reserve lies in UTM Zone 34S (EPSG: 32734)
lion_sf <- st_as_sf(
  lion_clean, 
  coords = c("lon", "lat"), 
  crs = 4326
) %>%
  st_transform(32734) # Reprojects coordinates into metric distance (meters)

# 5. Create amt Track Object
lion_track <- make_track(
  lion_clean,
  .x = lon,
  .y = lat,
  .t = date_time,
  id = animal_id,
  crs = 4326 # WGS84 starting CRS
)
# Explicitly reproject track coordinates to meters (UTM Zone 34S)
lion_track <- transform_coords(lion_track, 32734)

# 6. Inspect Sampling Intervals Across Individuals
lion_track %>% 
  nest(data = -id) %>% 
  mutate(sr = map(data, summarize_sampling_rate)) %>% 
  unnest(sr)
# output shows varied units, aka gps pings every 1 hour for some lions,
# every 30 mins for others
# need to make uniform

# Step 7: Filter to 30-Minute Lions & Resample Tracks
# Vector of IDs with 30-minute median sampling rates
ids_30min <- c(1001, 1002, 1004, 1005, 1006, 1007, 1008, 1012, 1013)

# Filter, resample at 30 mins (tolerance: 3 mins), and purge short bursts
lion_resampled_30m <- lion_track %>%
  filter(id %in% ids_30min) %>%
  nest(data = -id) %>%
  mutate(
    resampled = map(data, ~ .x %>%
                      track_resample(
                        rate = minutes(30),
                        tolerance = minutes(3)
                      ) %>%
                      # Keep bursts with at least 3 continuous steps
                      filter_min_n_burst(min_n = 3)
    )
  ) %>%
  select(id, resampled) %>%
  unnest(cols = resampled)

# Save processed fine-scale dataset for Phase 2 (Movement Metrics & Modeling)
saveRDS(lion_resampled_30m, file = "data/processed/lion_resampled_30m.rds")

# Inspect final clean fix count per individual
lion_resampled_30m %>% 
  count(id)