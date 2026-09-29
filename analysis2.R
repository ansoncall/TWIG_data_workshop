# ----------------------------------------------------------------------------#
## Intro - TWIG Data Workshop Exercise 2 ####

# In this exercise, we will explore the TWIG treatment index and combination
#   treatments. Two ideas have emerged in research: (1) some areas that have
#   experienced fire exclusion over the past several decades have reached a
#   state where simply reintroducing fire could be catastrophic; more precise
#   mechanical treatments are a necessary precursor (i.e., we need to thin,
#   then burn some parts of the landscape), and (2) combining mechanical and
#   fire-based treatments on the landscape can reduce risks of future
#   catastrophic fire more than one or the other by itself.
#
# Using Flagstaff, Arizona as an example, this exercise will showcase how to
#   perform some basic spatial joins (intersections, differences, etc.) to
#   visualize where this landscape has seen mechanical treatments, fire-based
#   treatments, or both. This type of activity can help decision-makers see
#   where different combinations of treatments have occurred, and help
#   prioritize where future treatments, both mechanical and fire-based, should
#   go.
# ----------------------------------------------------------------------------#
## Setup ####

# Start by loading the required packages: tidyverse, ggrepel, tidycensus, curl,
#   sf, mapview, leaflet,  and units. Use install.packages() to install missing
#   packages if needed. Use library() to load them.

library(tidyverse) # for data manipulation and visualization
library(ggrepel) # for tidy plot labels
library(curl) # for downloading files
library(sf) # for handling spatial data
library(mapview) # for interactive maps

# ----------------------------------------------------------------------------#
### Part 1 - Load Data ####

# Make a "data" directory if it doesn't already exist. This is where we will
#   download and store the raw data files. You can do this manually or use R's
#   dir.create() function.
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path)) # set working director to source file location

if (!dir.exists("data")) {
  dir.create("data")
}
if (!file.exists("data/treatment_index_flg.gdb")) {
  # TWIG can be downloaded directly from the web using the curl_download()
  #   function. Alternatively, you can download the data manually from the given
  #   URL and place it in a local "data" directory. Here is the direct URL to a
  #   Colorado-only subset of TWIG:
  # https://sweri-treament-index.s3.us-west-2.amazonaws.com/treatment_index_co.zip
  t_url <- "https://sweri-treatment-index.s3.us-west-2.amazonaws.com/treatment_index_flagstaff_area.zip"
  curl_download(t_url, destfile = "data/treatment_index_flg.zip", quiet = FALSE)
  unzip("data/treatment_index.zip", exdir = "data")
}

if (!file.exists("data/Perimeters_flg.gdb")) {
  # Similarly, read the wildfire perimeters data. These will be combined with
  #   fire-based treatments from flg_treatm to aggregate all polygons that show
  #   where fire has burned on the landscape
  p_url <- "https://sweri-treatment-index.s3.us-west-2.amazonaws.com/Perimeters_flagstaff_area.zip"
  curl_download(p_url, destfile = "data/Perimeters_flg.zip", quiet = FALSE)
  unzip("data/Perimeters_flg.zip", exdir = "data")
}

# At this point, we are ready to read the data into R. TWIG is distributed as an
#   ESRI file geodatabase. It can be loaded with st_read() from the sf package.
flg_treatm <- st_read("data/treatment_index_flg.gdb", layer = "treatment_index")

# The same is the case for wildfire Perimeters.
flg_wildfire <- st_read("data/Perimeters_flg.gdb", layer = "Perimeters")

# ----------------------------------------------------------------------------#
### Part 2 - Wrangling geospatial data from TWIG ####

# Because this exercise focuses on the geospatial distribution of treatments
#   and specific combinations of treatment types/categories, we will remove
#   other columns to reduce the clutter, selecting only the twig_categ and Shape
#   columns. We will also rename the twig_categ attribute to a more
#   reader-friendly name: "category"
polys_all <- flg_treatm |> select(category = twig_categ, Shape)

# Let's investigate the treatment index we received. As you recall from the
#   intro, we filtered the map extent by treatment category, focusing on
#   fire-based and mechanical treatments. Using summarise(), let's see how many
#   of each type of treatment exist in this subset of the database.
polys_all |>
  st_drop_geometry() |>
  group_by(category) |>
  summarise(n = n())

# Now, create a dataframe of all mechanical treatments using filter()
polys_mech <- NULL

# Creating a dataframe of all recorded fire on the landscape is slightly more
#   complicated. We have to combine data from both the flg_treatm and
#   flg_wildfire datasets. First, take a look at the columns of the wildfire
#   perimeters dataset
glimpse(flg_wildfire)

# Of the 122 columns in the dataset, this exercise only needs the polygons.
#   Create a new dataframe with only the Shape column (we will add category in
#   a moment).
polys_wildfire <- NULL

# Second, do the same for the fire-based treatments. Use filter() to select all
#   Planned or Unplanned Ignitions, and select only the Shape column.
polys_treatm_fire <- NULL

# Third, combine fires from the treatment index with wildfire perimeters using
#   bindrows() and add a "category" column with the value of "Fire" for all
#   entries using mutate() to gather all fire polygons into a single variable.
polys_fire_all <- bind_rows(polys_treatm_fire, polys_wildfire) |>
  mutate(category = "Fire")

# ----------------------------------------------------------------------------#
### Part 3 - Spatial operations ####

# Now that we have separate dataframes for mechanical and fire-based treatments,
#   we can perform spatial operations on them to build a venn diagram of areas
#   that have seen (1) only mechanical treatments, (2) only fire-based
#   treatments, and (3) areas that have seen both categories.

# We could call the st_difference() function with polys_mech and polys_fire_all
#   as parameters to get the areas with only one or the other, but this will
#   be computationally intensive due to the many-to-many comparisons required
#   if using the individual polygons of each data set. We will reduce this load
#   by creating masks (a single merged mutipolygon to serve as the geographic
#   extent), so we can instead compare many-to-one between polygons and the
#   single merged mask.

# First, create masks for fire-based treatments and mechanical treatments. Use
#   st_union() to accomplish this.
mask_fire <- NULL
mask_mech <- NULL

# Second, create a mask for where both treatments have occurred by using
#   st_intersection() and st_union() together. Calls to st_collection_extract()
#   and st_cast() are due to st_intersection() returning a geometry collection,
#   rather than a multipolygon, so we cast it to this type to maintain type
#   consistency when using the masks in the next set of commands.
mask_both <- st_intersection(mask_mech, mask_fire) |>
  st_collection_extract() |>
  st_cast("MULTIPOLYGON") |>
  st_union()

# Third, create polygon sets for each of the scenarios we are interested in,
#   beginning with areas that have seen only mechanical treatments, using
#   st_difference(), with the mechanical treatment polygons as the first
#   argument, and the mask of fire treatments as the second. We will also mutate
#   the category field to reflect this change. The call to st_make_valid() will
#   recitfy any problematic geometry as a result of the operation.
polys_mech_only <- st_difference(polys_mech, mask_fire) |>
  st_make_valid() |>
  mutate(category = "Mechanical Only")

# Create a polygon set for fire-based treatments in the same manner, but with
#   the category set to "Fire Only"
polys_fire_only <- NULL

# Create a polygon set for the intersection of mechanical and fire-based
#   treatments, with the category set to "Both".
polys_both <- NULL

# ----------------------------------------------------------------------------#
### Part 4 - Viewing polygons on a map ####

# Update mapview baselayer for viewing
mapviewOptions(
  basemaps = c("Esri.WorldGrayCanvas",
               "OpenStreetMap",
               "Esri.WorldImagery",
               "OpenTopoMap")
)

# We've got our polygons and categories, but we don't want to display them
#   alphabetically, instead having "Mechanical Only" first, "Fire Only" second,
#   and "Both" third. We'll use this list in the next command.
trt_levels <- c("Mechanical Only", "Fire Only", "Both")

# Like we did with the fire-based treatment and wildfire dataframes, use rbind()
#   to collect all polygons into a single variable and use mutate() to make
#   the "category" column a factor using trt_levels.
map_polys <- NULL

# View all the polygons on a map. Use "col.regions" parameter to change fill
#   color of polygons.
mapview(map_polys)

# ----------------------------------------------------------------------------#
### Part 5 - Feedback for us on the workshop ####

# How did you like this exercise? What worked well? What didn't work well?
# Please share your feedback at the following link:
# https://forms.office.com/r/wByidJWzZA
# Thank you!
