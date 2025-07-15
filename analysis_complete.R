# Intro ####

# In this exercise, we will explore the  TWIG treatment index and combine it
# with some income data from the US Census Bureau to answer the question:

# Do counties with higher incomes have more fuel treatments?

# Setup ####

## load packages ####

# Start by loading the required packages: tidyverse, tidycensus, curl, sf,
# mapview, leaflet,  and units. Use install.packages() to install missing
# packages if needed. Use library() to load them.

library(tidyverse) # for data manipulation and visualization
library(tidycensus) # for accessing US Census data
library(curl) # for downloading files
library(sf) # for handling spatial data
library(mapview) # for interactive maps
library(leaflet) # for fine-tuning those interactive maps
library(units) # for handling spatial units

## load data ####

# make a "data" directory if it doesn't already exist. This is where we will
# download and store the raw data files. You can do this manually or use R's
# dir.create() function.
if (!dir.exists("data")) {
  dir.create("data")
}

### TWIG ####

# TWIG can be downloaded directly from the web using the curl_download()
# function. Alternatively, you can download the data manually from the provided
# URL and place it in a local "data" directory. The direct url is:

# https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index.zip.

# This link can also be reached by clicking the "Full Treatment Index" hyperlink
# at https://reshapewildfire.org/resources/twig-data-resources.

curl_download(
  "https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index.zip",
  destfile = "data/treatment_index.zip",
  quiet = FALSE
)

# TODO replace above with link to CO-only subset.

# Unzip the downloaded file. Use unzip() or do this manually in your file
# browser. Put the unzipped files in the "data" directory using the exdir
# argument.

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
# Apply the filter to the state attribute to select where state == "CO".

twig_co <- twig %>% filter(state == "CO")

# check the size of the filtered dataset

object.size(twig_co) %>% print(units = "GB") # 0.3 Gb

# At this point, we can clear out the original dataset with rm() and then run
# garbage collection with gc() to free up memory.

rm(twig)
gc()

# write out workspace to an .rdata file so we can load it later without having
# to re-download the raw data.

save(twig_co, file = "data/twig_co.rdata")

# to read it back in:
# load("data/twig_co.rdata")
# If you get stuck, you can always come back to this spot and start over.

### TWIG alternate source ####
# TODO add a second source for the full dataset in case the first link breaks.

### Income Data ####

# To complete this analysis, we'll also need some income data. This data is from
# the American Community Survey, which comes from the US Census Bureau.

# access tidy census data with get_acs() from the tidycensus package. the
# variable name for median income is "B19013_001". be sure to set geometry =
# TRUE to get the spatial data. set state = "CO" and year = 2020.
income_data <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CO",
  year = 2020,
  geometry = TRUE
)

# Explore ####

## TWIG ####

# examine the object with glimpse(), head(), or summary()

glimpse(twig_co)
head(twig_co)
summary(twig_co)

# check for potentially problematic records by filter()ing on the "error"
# column. to speed this up, you may want to drop the geometry field with
# st_drop_geometry first. then, group_by() error type and summarize.

twig_co %>%
  st_drop_geometry %>%
  filter(!is.na(error)) %>%
  group_by(error) %>%
  summarize(n = n())

# let's exclude those duplicates, again using filter(). Keep everything that is
# NA or NOT "DUPLICATE-DROP".

twig_co <- twig_co %>% filter(error != "DUPLICATE-DROP" | is.na(error))

# how many records of each type? make a barplot using the twig_category column.

twig_co %>%
  ggplot(aes(x = twig_category)) +
  geom_bar() +
  labs(
    title = "Number of Treatments by Category",
    x = "Treatment Category",
    y = "Number of Treatments"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# optional: base r plot

barplot(table(twig_co$twig_category, useNA = "ifany"))


# check the NA values in the twig_category column with filter %>% glimpse

twig_co %>%
  st_drop_geometry %>%
  filter(is.na(twig_category)) %>%
  glimpse()

# note that we're constantly working to improve the data. if you find something
# that looks weird, feel free to ask about it! we may be able to recommend a
# solution or implement a bug fix.

# how many records in each year? mutate() to create a "treatment_year" column,
# using year() to summarize the treatment_date column. you may want to use
# st_drop_geometry() to drop the geometry column for speed.

twig_co %>%
  st_drop_geometry() %>%
  mutate(treatment_year = year(treatment_date)) %>%
  # group_by() the new column and count the number of records with summarize()
  group_by(treatment_year) %>%
  summarise(n = n()) %>%
  # plot the results with ggplot2
  ggplot(aes(x = treatment_year, y = n)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Number of Treatments by Year",
    x = "Year",
    y = "Number of Treatments"
  )

# how many records in each year and twig_category? add twig_category as a group,
# and plot groups by color.

twig_co %>%
  st_drop_geometry() %>%
  mutate(treatment_year = year(treatment_date)) %>%
  # group_by() the new column and count the number of records with summarize()
  group_by(treatment_year, twig_category) %>%
  summarise(n = n()) %>%
  # plot the results with ggplot2
  ggplot(aes(x = treatment_year, y = n, color = twig_category)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Number of Treatments by Year",
    x = "Year",
    y = "Number of Treatments"
  )

# make a map of all the "planned ignition" treatments in Colorado. Use filter to
# create a data subset, then use mapview() to create a map. if feature creation
# fails, you'll need to use st_make_valid to fix errant geometries first.

co_planned_ignitions <- twig_co %>%
  filter(twig_category == "Planned Ignition") %>%
  mutate(year = year(treatment_date)) %>%
  st_make_valid

mapview(co_planned_ignitions,
        zcol = "year",
        labFormat = labelFormat(big.mark = ""))

##### Income Data ####

# glimpse(), head(), or summay() the income data. "B19013_001" is the Census
# Bureau variable code for median income in the past 12 months, all households,
# dollars.

glimpse(income_data)

# let's map those incomes.
# ggplot version:

ggplot(income_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  labs(
    title = "Median Income by County in Colorado (2020)",
    fill = "Median Income ($)"
  ) +
  theme_minimal()

# mapview version:

mapview(income_data,
        zcol = "estimate",
        legend = TRUE)


# Analyze ####

# The research question is: are treatments more common in high income areas?

## Reproject income data ####

# First, check projection of both datasets and reproject if necessary. Use
# st_crs() to check the projection and st_transform() to reproject.

st_crs(twig_co) # WGS84 CRS, Pseudo-Mercator projection (typical web map)
st_crs(income_data) # NAD83 CRS, unprojected

income_data <- st_transform(income_data, st_crs(twig_co))

## Spatial join ####

# TWIG does not have a "county" field, so we need to do some wrangling to
# determine which county each fuel treatment is in.

# start by getting the centroids of the treatment polygons using st_centroid().
# Some polygons may be multipart - get the centroid of the largest part in this
# case (of_largest_polyon = TRUE).

twig_centroids <- st_centroid(twig_co, of_largest_polygon = TRUE)

# add an area column to the income data. (this will be useful later on.)

income_data <- income_data %>%
  mutate(area_m2 = st_area(geometry)) # area in m^2

# use a spatial join (st_join) to assign a county to each treatment centroid.

county_join <- st_join(twig_centroids, income_data, join = st_intersects) %>%
  # select() the unique_id, NAME, median income and area_m2 columns. rename if
  # necessary.
  select(unique_id,
         county = NAME,
         county_area_m2 = area_m2,
         county_income = estimate) %>%
  # drop the geometry column
  st_drop_geometry()

# lef_join() the county_join data to twig_co. use the "unique_id" column as the
# join key.

twig_co <- twig_co %>% left_join(county_join, by = "unique_id")

# calculate acres treated in county. Use the st_area() function from sf.

acres_treated <- twig_co %>%
  group_by(county) %>%
  summarise(acres_treated = sum(shape_Area),
            county_area = first(county_area_m2),
            county_income = first(county_income)) %>%
  mutate(prop_treated = acres_treated / county_area) # convert m^2 to acres


# plot the results
ggplot(acres_treated, aes(x = county_income, y = prop_treated)) +
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
model <- lm(prop_treated ~ county_income, data = acres_treated %>% drop_units)
summary(model)

