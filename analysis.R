# Intro ####

# In this exercise, we will explore the  TWIG treatment index and combine it
# with some income data from the US Census Bureau to answer the question:

# Do counties with higher incomes have more fuel treatments?

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

## Load Data ####

# Make a "data" directory if it doesn't already exist. This is where we will
# download and store the raw data files. You can do this manually or use R's
# dir.create() function.
if (!dir.exists("data")) {
  dir.create("data")
}

### TWIG ####

# TWIG can be downloaded directly from the web using the curl_download()
# function. Alternatively, you can download the data manually from the provided
# URL and place it in a local "data" directory. Here is the direct URL to a
# Colorado-only subset of TWIG:
# https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index_co.zip

# If you are unable to download the data from the above URL, you can try this
# backup link:
# TODO add link to backup source

# To download the full dataset, you can use the following link (don't try this
# in the workshop, it's >7GB of data!):
# https://reshapewildfire.org/resources/twig-data-resources.
current_path = rstudioapi::getActiveDocumentContext()$path # get source file loc
setwd(dirname(current_path )) # set working director to source file location
curl_download(
  "https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index_co.zip",
  destfile = "data/treatment_index.zip",
  quiet = FALSE
)
# Unzip the downloaded file. Use unzip() or do this manually in your file
# browser. Put the unzipped files in the "data" directory using the exdir
# argument.

unzip("data/treatment_index.zip", exdir = "data")

# At this point, we are ready to read the data into R. TWIG distributed as an
# ESRI file geodatabase. It can be loaded with st_read() from the sf package.

twig_co <- st_read("data/treatment_index_co.gdb", layer = "treatment_index_co")

# Write out the workspace to an .rdata file so we can load it later (if needed)
# without having to re-download the raw data.

save(twig_co, file = "data/twig_co.rdata")

# To read it back in:
# load("data/twig_co.rdata")
# If you get stuck, you can always come back to this spot and start over.

### Income Data ####

# To complete this analysis, we'll also need some income data. This data is from
# the American Community Survey, which comes from the US Census Bureau.

# Access tidy census data with get_acs() from the tidycensus package. The
# variable name for median income is "B19013_001". Set state = "CO" and year =
# 2020. Be sure to set geometry = TRUE to get the spatial data.

income_data <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CO",
  year = 2020,
  geometry = TRUE
)

# Explore ####

## TWIG ####

# Examine the object with glimpse(), head(), or summary()

glimpse(twig_co)
head(twig_co)
summary(twig_co)

# Check for potentially problematic records by filter()ing on the "error"
# column. To speed this up, you may want to drop the geometry field with
# st_drop_geometry first. Then, group_by() error type and summarize.

NULL





# Let's exclude those duplicates, again using filter(). Keep everything that is
# NA or NOT "DUPLICATE-DROP".
# Note: if you didn't find any duplicates, you can skip over this.
twig_co <- NULL

# How many records in each twig_category? Make a barplot using the twig_category
# column.

twig_co %>%
  ggplot(aes(x = twig_category)) +
  geom_bar() +
  labs(
    title = "Number of Treatments by Category",
    x = "Treatment Category",
    y = "Number of Treatments"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Optional: base r plot

barplot(table(twig_co$twig_category, useNA = "ifany"))


# Check the NA values in the twig_category column with filter %>% glimpse

NULL




# Note that we're constantly working to improve the data. If you find something
# that looks weird, feel free to ask about it! We may be able to recommend a
# solution or implement a bug fix.

# How many records in each year? mutate() to create a "treatment_year" column,
# using year() to summarize the treatment_date column. You can first use
# st_drop_geometry() for speed.

twig_co %>%
  st_drop_geometry() %>%
  mutate(treatment_year = year(treatment_date)) %>%
  # Group_by() the new column and count the number of records with summarize().
  group_by(treatment_year) %>%
  summarise(n = n()) %>%
  # Plot the results with ggplot2.
  ggplot(aes(x = treatment_year, y = n)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Number of Treatments by Year",
    x = "Year",
    y = "Number of Treatments"
  )

# How many records in each year and twig_category? Add twig_category as a group,
# and plot groups by color.

twig_co %>%
  st_drop_geometry() %>%
  mutate(treatment_year = year(treatment_date)) %>%
  # group_by() the new column and count the number of records with summarize().
  group_by(treatment_year, twig_category) %>%
  summarise(n = n()) %>%
  # Plot the results with ggplot2.
  ggplot(aes(x = treatment_year, y = n, color = twig_category)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Number of Treatments by Year",
    x = "Year",
    y = "Number of Treatments"
  )

# Make a map of all the "planned ignition" treatments in Colorado. Use filter()
# to create a data subset, then use mapview() to create a map. If feature
# creation fails, you'll need to use st_make_valid to fix errant geometries
# first.

co_planned_ignitions <- NULL




mapview(co_planned_ignitions,
        zcol = "year",
        labFormat = labelFormat(big.mark = ""))

##### Income Data ####

# glimpse(), head(), or summay() the income data. "B19013_001" is the Census
# Bureau variable code for median income in the past 12 months, all households,
# dollars.

glimpse(income_data)

# Let's map those incomes.
# ggplot version:

ggplot(income_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  labs(
    title = "Median Income by County in Colorado (2020)",
    fill = "Median Income ($)"
  ) +
  theme_minimal()

# Mapview version:

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

income_data <- NULL

## Spatial join ####

# TWIG does not have a "county" field, so we need to do some wrangling to
# determine which county each fuel treatment is in.

# Start by getting the centroids of the treatment polygons using st_centroid().
# Some polygons may be multipart - get the centroid of the largest part in this
# case (of_largest_polyon = TRUE).

twig_centroids <- NULL

# Add an area_m2 column to the income data. (this will be useful later on.)

income_data <- NULL


# Use a spatial join (st_join) to assign a county to each treatment centroid.

county_join <- NULL
  # select() the unique_id, NAME, median income and area_m2 columns. Rename if
  # necessary.




  # drop the geometry column


# left_join() the county_join data to twig_co. use the "unique_id" column as the
# join key.

twig_co <- NULL


# There were a few records that weren't matched to a county. Let's check them
# out using filter(). We also need st_make_valid() to fix any invalid geometries
# before we can plot.

unmatched <- NULL



mapview(unmatched, col.region = "red")

# These could be treatments that are partially inside of Colorado, but with a
# centroid falling outside the state. It's also possible (though far less
# likely) that the "State" field is incorrect in the original data source.

# Calculate acres treated in county. Use the st_area() function from sf.

acres_treated <- NULL







# Plot the results.

acres_treated %>%
  st_drop_geometry %>%
  drop_units() %>%
  ggplot(aes(x = county_income, y = prop_treated)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Proportion of County Treated vs. Median Income",
    x = "Median Income",
    y = "Proportion of County Treated"
  ) +
  geom_label_repel(aes(label = str_sub(county, end = -18))) +
  geom_point() +
  theme_minimal()

# Some heinous statistical crimes below.

model <- NULL #lm()...
summary(model)


# Survey ####

# How did you like this exercise? What worked well? What didn't work well?
# Please share your feedback at the following link:
# https://forms.office.com/r/wByidJWzZA
# Thank you!!
