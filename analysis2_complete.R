# Intro ####

# In this exercise, we will explore the  TWIG treatment index and combine it
# with some income data from the US Census Bureau to answer the question:

# Do Colorado counties with higher incomes have more fuel treatments?

# Setup ####

## load packages ####

# Start by loading the required packages: tidyverse, ggrepel, tidycensus, curl,
# sf, mapview, leaflet,  and units. Use install.packages() to install missing
# packages if needed. Use library() to load them.

library(tidyverse) # for data manipulation and visualization
library(ggrepel) # for tidy plot labels
library(tidycensus) # for accessing US Census data
library(curl) # for downloading files
library(sf) # for handling spatial data
library(mapview) # for interactive maps
library(leaflet) # for fine-tuning those interactive maps
library(units) # for handling spatial units
library(raster)
library(terra)

## Load Data ####

# At this point, we are ready to read the data into R. TWIG distributed as an
# ESRI file geodatabase. It can be loaded with st_read() from the sf package.
flg_treatm <- st_read("data/treatment_index_09202026.gdb",
                   layer = "treatment_index")

flg_wildfire <- st_read("data/Perimiteres_09202026.gdb")

### TWIG ####

# Check for potentially problematic records by filter()ing on the "error"
# column. To speed this up, you may want to drop the geometry field with
# st_drop_geometry first. Then, group_by() error type and summarize.

flg_treatm %>%
  st_drop_geometry %>%
  filter(!is.na(error)) %>%
  group_by(error) %>%
  summarize(n = n())

# Let's exclude those duplicates, again using filter(). Keep everything that is
# NA or NOT "DUPLICATE-DROP".

flg_treatm <- flg_treatm %>% filter(error != "DUPLICATE-DROP" | is.na(error))

# Update mapview baselayer for viewing
mapview::mapviewOptions(
  basemaps = c("Esri.WorldGrayCanvas",
               "OpenStreetMap",
               "Esri.WorldImagery",
               "OpenTopoMap")
)

# Filter for mechanical treatments from the treatment index data downloaded
#   from TWIG, selecting only the category and polygons
mech_df <- flg_treatm |>
  filter(twig_categ == "Mechanical") |>
  # inheritance issue requires explicit dplyr call
  dplyr::select(category = twig_categ, Shape)

# Select category and polygons from the wildfire perimeter dataset downloaded
#   from TWIG
wildfire_df <- flg_wildfire |>
  mutate(category = "Fire") |>
  dplyr::select(category, Shape)

# Combine fires from the treatment index with wildfire perimeters to gather all
#   fire polygons into a single variable
fire_df <- flg_treatm |>
  filter(twig_categ == "Planned Ignition" |
           twig_categ == "Unplanned Ignition") |>
  mutate(category = "Fire") |>
  dplyr::select(category, Shape) |>
  bind_rows(wildfire_df)

# First create a single merged polygon; this will reduce computational load
#   when calling st_difference() to isolate areas that received only mechanical
#   or only burning
fire_poly_union <- st_union(fire_df)
mech_poly_union <- st_union(mech_df)

# Then create list of shapes to preserve category type, potentially use these
#   as the base of st_difference()
# burn_poly_multi <- st_union(fire_df, by_feature = TRUE)
# mech_poly_multi <- st_union(mech_df, by_feature = TRUE)


# twig_mech_rast_templ <- rast(ext(mech_df),
#                              resolution = 1,
#                              crs = crs(mech_df))
# twig_mech_rast_poly <- rasterize(mech_df,
#                                  twig_mech_rast_templ,
#                                  field = "treatment_year")

# Plot the result
# plot(twig_mech_rast_poly)


mech_only_diff <- st_difference(mech_poly_union, fire_poly_union) |>
  st_make_valid()
fire_only_diff <- st_difference(fire_poly_union, mech_poly_union) |>
  st_make_valid()
mech_fire_inter <- st_intersection(mech_poly_union, fire_poly_union) |>
  st_make_valid()
glimpse(mech_only_diff)

mapview(
  list(mech_only_diff, fire_only_diff, mech_fire_inter),
  labFormat = labelFormat(big.mark = ""),
  burst = TRUE
)
mapview(fire_only_diff)

# Survey ####

# How did you like this exercise? What worked well? What didn't work well?
# Please share your feedback at the following link:
# https://forms.office.com/r/wByidJWzZA
# Thank you!!
