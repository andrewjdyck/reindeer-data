
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)


# Priority Species ranges
# https://open.canada.ca/data/en/dataset/c3d1771f-fa10-4a09-ba6e-dece0ceb5f37?_gl=1*eggf0w*_ga*MTg1NjEwNjkzNy4xNzI4Njc4OTUy*_ga_S9JG8CZVYZ*MTczNDMyMjkzMy4xNi4xLjE3MzQzMjMwNTIuMy4wLjA.


# Zipped file
priority_species_zip <- 'https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fplansreports%2Fpriority-species-for-species-at-risk%2FPrioritySpecies.gdb.zip'
gdb_path <- './Data/PrioritySpecies/PrioritySpecies.gdb'

# Inspect the layers in the GDB. There is just one layer in this.
# layers <- st_layers(gdb_path)
# print(layers)

# Read the priority species layer
gdb_data <- st_read(gdb_path, layer = 'PrioritySpecies')

# Select only the Caribou ranges from the Priority Species dataset
caribou_ranges <- gdb_data %>%
  filter(CommName_E %in% c("Caribou", "Barren-ground Caribou", "Woodland Caribou", "Peary Caribou")) %>%
  mutate(area = st_area(.))



# Download Canada provinces/territories boundaries
## This uses the Natural Earth package
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Inspect the data
print(canada_provinces)

# Plot the map of Canada boundaries.
ggplot() +
  geom_sf(data = canada_provinces, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Canada's Provincial and Territorial Boundaries")

# Simplify the boundaries dataset and transform to have the same CRS as the Priority Specis dataset.
canada_boundaries_simp <- st_transform(canada_provinces, st_crs(gdb_data)) %>%
  select(name, geometry) %>%
  mutate(area = st_area(.)) # Calculates the area of each province (and territory) in m^2

# Plot the transformed canada boundaries.
ggplot(data=canada_boundaries_simp) +
  geom_sf() +
  theme_minimal()

# Calculate how much of Canada's landmass is covered by caribou range.
100*(sum(caribou_ranges$area) / sum(canada_boundaries_simp$area)) # = 80.7%

# Main plot of Reindeer / Caribou ranges in Canada on top of the Canada admin boundaries 
ggplot() +
  # Base map layer
  geom_sf(data = canada_boundaries_simp, fill = "gray90", color = "black", size = 0.5) +
  # GDB layer
  geom_sf(data = caribou_ranges, fill='lightblue', size = 0.5, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Reindeer (Caribou) Habitat Covers 80% of Canada's Landmass",
    color = "Legend"
  )



