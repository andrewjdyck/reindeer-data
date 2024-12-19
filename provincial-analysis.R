# Province-by-province analysis
# In this section I subset Canada's admin boundaries to just a single province, then get the intersection of this and the caribou ranges dataset.

# This code is an ugly copy-paste 10x for each province. I'm sure there is a way to simplify it but left this for the future if I have the time.


# Before this analysis can be run, the following datasets from federal-analysis.R in this repo must be created and exist in the working environment.
# canada_boundaries_simp - simplified administrative boundaries for Canada
# caribou_ranges - the Canadian range for reindeer / caribou in Canada


## British Columbia
bc_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'British Columbia') %>% 
  select(name, area, geometry)

bc_caribou <- st_intersection(caribou_ranges, bc_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = bc_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = bc_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting British Columbia",
    color = "Legend"
  )

## Ontario 
on_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Ontario') %>% 
  select(name, area, geometry)

on_caribou <- st_intersection(caribou_ranges, on_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = on_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = on_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Ontario",
    color = "Legend"
  )

## Saskatchewan
sk_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Saskatchewan') %>% 
  select(name, area, geometry)

sk_caribou <- st_intersection(caribou_ranges, sk_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = sk_boundaries, alpha = 0.5) +  # BC boundary
  geom_sf(data = sk_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Saskatchewan",
    color = "Legend"
  )

## Alberta
ab_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Alberta') %>% 
  select(name, area, geometry)

ab_caribou <- st_intersection(caribou_ranges, ab_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = ab_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = ab_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Alberta",
    color = "Legend"
  )

## Manitoba
mb_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Manitoba') %>% 
  select(name, area, geometry)

mb_caribou <- st_intersection(caribou_ranges, mb_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = mb_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = mb_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Manitoba",
    color = "Legend"
  )


## Quebec
qc_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Québec') %>% 
  select(name, area, geometry)

qc_caribou <- st_intersection(caribou_ranges, qc_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = qc_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = qc_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Quebec",
    color = "Legend"
  )

## New Brunswick
nb_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'New Brunswick') %>% 
  select(name, area, geometry)

nb_caribou <- st_intersection(caribou_ranges, nb_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = nb_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = nb_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting New Brunswick",
    color = "Legend"
  )


## Quebec
ns_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Nova Scotia') %>% 
  select(name, area, geometry)

ns_caribou <- st_intersection(caribou_ranges, ns_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = ns_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = ns_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting Nova Scotia",
    color = "Legend"
  )

## PEI
pe_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Prince Edward Island') %>% 
  select(name, area, geometry)

pe_caribou <- st_intersection(caribou_ranges, pe_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = pe_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = pe_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting P.E.I.",
    color = "Legend"
  )


## PEI
nl_boundaries <- canada_boundaries_simp %>% 
  filter(name == 'Newfoundland and Labrador') %>% 
  select(name, area, geometry)

nl_caribou <- st_intersection(caribou_ranges, nl_boundaries) %>%
  mutate(area = st_area(.))

ggplot() +
  geom_sf(data = nl_boundaries, alpha = 0.5) +  # Prov boundary
  geom_sf(data = nl_caribou, fill = "lightblue", size = 0.5) +  # Intersected caribou layer
  theme_minimal() +
  labs(
    title = "Proportions of Reindeer / Caribou Range Intersecting P.E.I.",
    color = "Legend"
  )

# Compare areas of reindeer pop by prov

# This puts the provincial results into a data.frame
prov_share <- as.data.frame(list(
  "BC" = 100*(sum(bc_caribou$area) / bc_boundaries$area),
  "AB" = 100*(sum(ab_caribou$area) / ab_boundaries$area),
  "SK" = 100*(sum(sk_caribou$area) / sk_boundaries$area),
  "MB" = 100*(sum(mb_caribou$area) / mb_boundaries$area),
  "ON" = 100*(sum(on_caribou$area) / on_boundaries$area),
  "QC" = 100*(sum(qc_caribou$area) / qc_boundaries$area),
  "NB" = 100*(sum(nb_caribou$area) / nb_boundaries$area),
  "NS" = 100*(sum(ns_caribou$area) / ns_boundaries$area),
  "PE" = 100*(sum(pe_caribou$area) / pe_boundaries$area),
  "NL" = 100*(sum(nl_caribou$area) / nl_boundaries$area)
)) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "Province", 
    values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(Province = as.factor(Province)) %>%
  arrange(-Value)


# Re-order the data.frame so that the barchart will be ordered from highest to lowest.
prov_share$Province <- factor(prov_share$Province, levels = prov_share$Province[order(-prov_share$Value)])


# This plots a barchart of the 10 provinces and the share of their landmass covered by caribou range.
ggplot(prov_share, aes(x=Province, y=Value)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Share of Canadian Province with Reindeer habitat",
    x = "Province",
    y = "Caribou habitat share of province (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






