# Setup ####

## load packages ####

# Start by loading the required packages: tidyverse, curl, httr, sf, and mapview
library(tidyverse)
library(tidycensus)
library(curl)
library(sf)
library(mapview)

## load data ####

# make a "data" directory if it doesn't already exist. This is where we will
# download and store the raw data files. You can do this manually or use R's
# dir.create() function.
if (!dir.exists("data")) {
  dir.create("data")
}

### TWIG ####

# TWIG can be downloaded directly from the web using the curl_download function.
# Alternatively, you can download the data manually from the provided URL and
# place it in a local "data" directory. The direct url is:

# https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index.zip.

# This link can also be reached by clicking the "Full Treatment Index" hyperlink
# at https://reshapewildfire.org/resources/twig-data-resources.

curl_download(
  "https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index.zip",
  destfile = "data/treatment_index.zip"
)

# Unzip the downloaded file. Use unzip() or do this manually in your file
# browser. Put the unzipped files in the "data" directory.
unzip("data/treatment_index.zip", exdir = "data")

# At this point, we are ready to read the data into R. TWIG distributed as an
# ESRI file geodatabase. It can be loaded with st_read() from the sf package.
twig <- st_read("data/treatment_index.gdb", layer = "treatment_index")

# This is a large dataset, so it will take a minute or two to load. Once loaded,
# we can check the size of the object in memory using the object.size()
# function.
object.size(twig) %>% print(units = "GB") # 7.3 Gb

# Datasets of this size can be difficult to work with, so we will filter it down
# to Colorado only. This is easy to do with the filter() function from dplyr.
# Apply the filter to the state attribute, which contains the two-letter
# abbreviation for the US state where the management activity occured.
twig_co <- twig %>%
  filter(state == "CO")

# check the size of the filtered dataset
object.size(twig_co) %>% print(units = "GB") # 0.3 Gb

# At this point, we can clear out the original dataset with rm() and then run
# garbage collection with gc() to free up memory.
rm(twig)
gc()

### TWIG alternate source ####

# If the TWIG data link breaks, you can also access the CO subset
# of data through the following link
# TODO add a second source for the full dataset in case the first link breaks.
# TODO add a source to directly download the CO subset of the data.

### Income Data ####

# To complete this analysis, we'll also need some income data. This data is from
# the American Community Survey, which comes from the US Census Bureau.

# access tidy census data with get_acs() from the tidycensus package. the
# variable name for median income is "B19013_001". be sure to set geometry =
# TRUE to get the spatial data.
income_data <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CO",
  year = 2020,
  geometry = TRUE
)

# Explore ####
twig_co
names(twig_co)

income_data
names(income_data) # check the column names
# TODO add some stuff to explore the data. Column names, summary statistics,
# maps.

# Analyze ####

# The research question is: are treatments more common in high income areas?

## Reproject income data ####

# First, check projection of both datasets and reproject if necessary. Use
# st_crs() to check the projection and st_transform() to reproject.
st_crs(twig_co) # WGS84 CRS, Pseudo-Mercator projection (typical web map)
st_crs(income_data) # NAD83 CRS, unprojected
income_data <- st_transform(income_data, st_crs(twig_co))

## Spatial join ####

# start by getting the centroids of the treatment polygons. Some polygons may be
# multipart, get the centroid of the largest part in this case.
twig_centroids <- st_centroid(twig_co, of_largest_polygon = TRUE)

# add an area column to the income data
income_data <- income_data %>%
  mutate(area_m2 = st_area(geometry)) # area in m^2

# use a spatial join (st_join) to assign a county  to each treatment centroid.
county_join <- st_join(twig_centroids, income_data, join = st_intersects) %>%
  # pull out the unique_id, NAME, median income and area_m2 columns. rename if necessary.
  select(unique_id,
         county = NAME,
         county_area_m2 = area_m2,
         county_income = estimate) %>%
  # drop the geometry column
  st_drop_geometry()

# join the county_join data with the original treatment data
twig_co <- twig_co %>%
  left_join(county_join, by = "unique_id")

# calculate acres treated by county. Use the st_area() function from sf.
acres_treated <- twig_co %>%
  group_by(county) %>%
  summarise(acres_treated = sum(shape_Area),
            county_area = first(county_area_m2),
            county_income = first(county_income)) %>%
  mutate(prop_treated = acres_treated / county_area) # convert m^2 to acres


# plot the results
library(units)
ggplot(acres_treated, aes(x = county_income, y = perc_treated)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Percentage of County Treated vs. Median Income",
    x = "Median Income",
    y = "Proportion of County Treated"
  ) +
  geom_label(
    aes(label = county),
    nudge_x = 1000,
    nudge_y = 0.01,
    check_overlap = TRUE
  ) +
  theme_minimal()

# some heinous statistical crimes
model <- lm(perc_treated ~ county_income, data = acres_treated %>% drop_units)
summary(model)

