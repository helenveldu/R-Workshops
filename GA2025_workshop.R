# Learning - R Workshop
# Introduction to Data Visualization and Analysis for Environmental Justice

# By: Chris Oates and Helen Velasquez
#     PhD Students, Biological and Agricultural Engineering (BAE)
#     North Carolina State University (NSCU)
# Geoscience Alliance 2025 Conference
# Raleigh/Durham, North Carolina


#### Setup R Session

# First, we are going to set our working directory. When you open an R session (especially in RStudio), 
# R needs to know where to look for your files and where to save your work. This location is called the 
# **working directory**.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This code uses the location of the script as a working directory



###############################################################################
#                  Tabular Data Inspection and Visualization                  #
###############################################################################



# Part 1A. ------------------------------------------------------------------------
#### Install and load packages

# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse") # Run this only once
library(tidyverse) 





# ------------------------------------------------------------------------
#### Read and inspect data

# Read the .csv file and store the data in an object called `water_quality`
water_quality <- read_csv("data/stream_water_quality.csv")

### TODO: borrar 
# write_csv(water_quality, "data/water_quality.csv" )

# head() prints the first 6 rows of our dataset
head(water_quality)
tail(water_quality) # prints the last 6 rows!

# dim() prints the dimensions of the dataset
dim(water_quality)

# The summary() function prints a summary of the variables in your dataset. Note that for continuous data, we get summary statistics.  
summary(water_quality)

# The colnames() function prints all the column names in your dataset.
colnames(water_quality)


# We can use "$" to subset a column from your 'water_quality' dataset
water_quality$temp_c # creates a vector of all the values in the state column
temp <- water_quality$temp_c # saves vector as an object

# unique() prints all the unique values found in a specific column (subset of your data)
unique(water_quality$stream_type)
unique(water_quality$site) # print what sites can be found in our data





# Part 2A. ------------------------------------------------------------------------
### Let's start plotting!


# Layer 1
ggplot(data = water_quality, aes(x = temp_c, y = DO_mgL))

#  Now we will create a scatter plot by adding a geom layer. 
ggplot(data = water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_point()


#### Aesthetic mapping features

ggplot(water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_point(aes(color = site, shape = stream_type))

#### Facets

# Add a third layer to our plot! `facet_wrap()`
ggplot(water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_jitter(aes(color = site, shape = stream_type), alpha = 0.7) +
  facet_wrap(~ season)

# Now lets try `facet_grid()`
ggplot(water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_jitter(aes(color = site), alpha = 0.7) +
  facet_grid(season ~ stream_type)

#### Themes

# Try changing up the theme!
ggplot(water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_point(aes(color = site, shape = site), alpha = 0.7) +
  theme_bw()

#### Labels
ggplot(water_quality, aes(x = temp_c, y = DO_mgL)) +
  geom_point(aes(color = site, shape = stream_type), alpha = 0.7) +
  theme_bw() +
  labs(
    title = "Temperature vs. Dissolved Oxygen by Site and Stream Type",
    subtitle = "",
    x = "Temperature (C°)",
    y = "Dissolved Oxygen (mg/L)",
    color = "Site",
    shape = "Stream Type"
  )

#### Save your plot `ggsave()`

ggsave("temp_do_plot.pdf", width = 6, height = 5, units = "in")


# Part 3A. ------------------------------------------------------------------------
### More plots!

# Boxplot Geometry
ggplot(water_quality, aes(x = site, y = turbidity_NTU)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.3, aes(color = site)) +
  theme_bw() +
  labs(x = 'Stream',
       y = 'Turbidity (NTU)',
       color = 'Site')

# Density Geometry
ggplot(water_quality, aes(x = turbidity_NTU, fill = site)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(x = "Turbidity (NTU)", 
       y = "Density", 
       fill = "Sites")








###############################################################################
#                  Geospatial Data Analysis and Visualization                 #
###############################################################################


# Part 1B. ------------------------------------------------------------------------
#### Install and load packages

# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("sf") # Run this only once
library(sf) 



# Part 2B. ------------------------------------------------------------------------
# Read geospatial data


# Load watershed (HUC 8-scale) shapefile
huc8 <- st_read(dsn = ".", layer = "GA_2025_HUC_Population_Data") # option 1
huc8 <- read_sf("GA_2025_HUC_Population_Data/NC_8-Digit_and_10-Digit_HUC_with_Calculated_Population.shp") #option 2

# Load census tract SVI shapefile 
nc_svi <- st_read(dsn = ".", layer = "GA_2025_NC_SVI") #option 1
nc_svi <- read_sf("GA_2025_NC_SVI/nc_svi.shp") #option 2


# Change the shapefiles' coordinate system to NAD83 (if not already)
huc8 <- st_transform(huc8, crs = 4269)
nc_svi <- st_transform(nc_svi, crs = 4269)

# Print columns in the NC SVI shapefile
names(nc_svi)


# Part 3B. ------------------------------------------------------------------------
# Data transformation

# Create new columns for statistics of note
nc_svi <- nc_svi %>%
  mutate(
    pct_minority       = 100 * E_MINRTY / E_TOTPOP,     # Minority % of total pop
    pct_poverty        = 100 * E_POV150 / E_TOTPOP,     # Below 150% poverty line
    pct_unemployed     = 100 * E_UNEMP / E_TOTPOP,      # Unemployed
    pct_uninsured      = 100 * E_UNINSUR / E_TOTPOP,    # No health insurance
    pct_no_hs_diploma  = 100 * E_NOHSDP / E_TOTPOP,     # No high school diploma
    pct_age65          = 100 * E_AGE65 / E_TOTPOP,      # Age 65+
    pct_cost_burdened  = 100 * E_HBURD / E_TOTPOP       # Housing cost-burden
  )


# Part 4B. ------------------------------------------------------------------------
# Visualize geospatial data!


# Let's plot of some these new columns
# Minority percentage 
ggplot(nc_svi) +
  geom_sf(aes(fill = pct_minority), color = NA) +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "gray90") +
  labs(
    title = "Minority Percent by Census Tract, North Carolina",
    fill = "Minority %",
    caption = "Source: CDC SVI 2022, ACS 2018–2022"
  ) +
  theme_minimal()

# Poverty percentage
ggplot(nc_svi) +
  geom_sf(aes(fill = pct_poverty), color = NA) +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "gray90") +
  labs(
    title = "Poverty Percent by Census Tract, North Carolina",
    fill = "Poverty %",
    caption = "Source: CDC SVI 2022, ACS 2018–2022"
  ) +
  theme_minimal()

# Copy the aforementioned code to plot other NC metrics!^



# Part 5B. ------------------------------------------------------------------------
# Geospatial analysis and transformations


# Merge NC SVI data with shapefile data to look at watershed-level metrics

# Spatially intersect census tracts with HUC8s (watersheds)
nc_huc_intersection <- st_intersection(nc_svi, huc8)

# Calculate area of intersection for weighting
nc_huc_intersection <- nc_huc_intersection %>%
  mutate(intersect_area = as.numeric(st_area(.)))

# Group and calculate area-weighted stats by watershed (this will take some time to load)
watershed_stats <- nc_huc_intersection %>%
  group_by(HUC_8 = HUC_8) %>%  
  summarize(
    area_weighted_minority  = weighted.mean(pct_minority, intersect_area, na.rm = TRUE),
    area_weighted_poverty   = weighted.mean(pct_poverty, intersect_area, na.rm = TRUE),
    area_weighted_uninsured = weighted.mean(pct_uninsured, intersect_area, na.rm = TRUE),
    total_population         = sum(E_TOTPOP, na.rm = TRUE),
    .groups = "drop"
  )

# Add these new watershed stats back to the original watershed shapefile
watershed_stats_df <- st_drop_geometry(watershed_stats)
huc8_stats <- left_join(huc8, watershed_stats_df, by = "HUC_8")  

# Plot watersheds and their demographics. Let's most minority percentage 
ggplot(huc8_stats) +
  geom_sf(aes(fill = area_weighted_minority), color = "white", size = 0.3) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    na.value = "black",
    name = "Minority %"
  ) +
  labs(
    title = "Minority Percent by Watershed (HUC 8-scale)",
    caption = "Source: CDC SVI 2022 & U.S Geological Survey"
  ) +
  theme_minimal(base_size = 13) 

# Let's add census tracts to the watersheds to provide more locational context
ggplot(huc8_stats) +
  geom_sf(aes(fill = area_weighted_minority), color = "white", size = 0.3) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    na.value = "black",
    name = "Minority %"
  ) +
  labs(
    title = "Minority Percent by Watershed (HUC 8-scale)",
    caption = "Source: CDC SVI 2022 & U.S Geological Survey"
  ) +
  theme_minimal(base_size = 13) + 
  geom_sf(data = nc_svi, fill = NA, color = "black", size = 0.1)


# Pull the watersheds with the high minority percentage 
top10 <- huc8_stats %>%
  st_drop_geometry() %>%
  slice_max(order_by = area_weighted_minority, n = 10)

# Plot this list
ggplot(top10, aes(x = reorder(SUBBASIN, area_weighted_minority), y = area_weighted_minority)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Watersheds by Minority Percentage",
    x = "Watershed",
    y = "Minority Percentage"
  ) +
  theme_minimal()

# Let's look at census tracts in North Carolina's southern watersheds 
southern_hucs <- huc8_stats %>%
  filter(SUBBASIN %in% c(
    "Black", "Lumber",  "Little Pee Dee", "Lower Pee Dee",
    "Waccamaw", "Upper Pee Dee", "Lower Cape Fear", "White Oak",
    "Coastal Carolina"
  ))
tracts_in_the_south <- st_intersection(nc_svi, southern_hucs)

# Plot these watershed
ggplot() +
  geom_sf(data = tracts_in_the_south, aes(fill = pct_minority), color = NA) +
  geom_sf(data = southern_hucs, fill = NA, color = "black", size = 1) +
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray90") +
  labs(
    title = "Tract-Level Minority Percentage within Southern NC Watersheds",
    fill = "%",
    caption = "Clipped to the Black, Lumber, Little Pee Dee, Lower Pee Dee,
  Waccamaw, Upper Pee Dee, Lower Cape Fear, Coastal Carolina, and White Oak Watersheds"
  ) +
  theme_minimal()

# Let's add a few surrounding counties for reference
southern_tracts <- nc_svi %>%
  filter(COUNTY %in% c(
    "Robeson", "Columbus", "Bladen", "Cumberland", "Scotland", "Richmond", "Hoke", "Moore",
    "Harnett", "New Hanover", "Brunswick", "Pender", "Duplin", "Sampson", "Anson", "Union",
    "Staley", "Montgomery", "Randolph", "Chatham", "Stanly", "Sampson", "Wayne", "Johnston",
    "Wake"
  ))
# Plot these watershed
ggplot() +
  geom_sf(data = southern_tracts, fill = NA, color = "gray80", size = 0.2) +
  geom_sf(data = tracts_in_the_south, aes(fill = pct_minority), color = NA) +
  geom_sf(data = southern_hucs, fill = NA, color = "black", size = 1 )+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray90") +
  labs(
    title = "Tract-Level Minority Percentage within Southern NC Watersheds",
    fill = "%",
    caption = "Clipped to the Black, Lumber, Little Pee Dee, Lower Pee Dee,
  Waccamaw, Upper Pee Dee, Lower Cape Fear, and White Oak Watersheds"
  ) +
  theme_minimal()




###############################################################################
#           CONGRATS!!! YOU HAVE ANALYZED AND VISUALIZED DATA IN R            #
###############################################################################






