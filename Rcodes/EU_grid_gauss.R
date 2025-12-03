library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)


# Define your region names
regions <- c( 'West3', 'West4', 
              'Central1', 'Central2', 
              'East1', 'East2', 'East3', 'East4', 
              'East5', 'East6')

# Create file paths
file_paths <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")

# # Read all shapefiles and combine them
# combined_shp <- lapply(file_paths, st_read) %>%
#   bind_rows()
# 
# # Write the combined shapefile
# st_write(combined_shp, "Data/EU_Grid25km_combined.shp")



combined_shp <- st_read("Data/EU_Grid25km_combined.shp")

# test csv

csv1 <- read.csv("Data/EU_Grid25km_RH50_gauss_Central1.csv")





# Define region names
regions <- c( 'West3', 'West4', 
              'Central1', 'Central2', 
              'East1', 'East2',  'East3', 'East4', 
              'East5', 'East6' )

# Read and merge all CSV files
csv_files <- paste0("Data/EU_Grid25km_RH50_gauss_", regions, ".csv")
combined_csv <- lapply(csv_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE, colClasses = c(
    grid_id = "character",
    region = "character"
  ))
  # Ensure character columns even if empty
  if(nrow(df) == 0) return(NULL)
  df$grid_id <- as.character(df$grid_id)
  df$region <- as.character(df$region)
  return(df)
}) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows() %>%
  filter(rh50_x10_count > 1000) %>%
  mutate(
    rh50_median = rh50_x10_median / 10,
    rh_non_ground50_median = rh_non_ground50_x10_median / 10,
    delta = rh_non_ground50_median - rh50_median
  ) %>%
  select(grid_id, numeric_id, region, rh50_median, rh_non_ground50_median, delta)


combined_csv <- tibble(combined_csv)

# Read and merge all shapefiles
shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, st_read) %>%
  bind_rows()

# Merge shapefile with CSV data
merged_data <- combined_shp %>%##st_drop_geometry() %>%
  left_join(combined_csv, by = c("numeric_id", "region"))

# Create map with diverging palette
ggplot(merged_data) +
  geom_sf(aes(fill = delta), color = NA) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red",
    midpoint = 0,
    name = "Delta\n(m)"
  ) +
  theme_minimal() +
  labs(title = "Difference between Non-Ground and Raw RH50 Median") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


# Create map with viridis palette and horizontal legend
ggplot(merged_data) +
  geom_sf(aes(fill = delta), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",  # or "magma", "plasma", "inferno", "cividis"
    name = "Delta (m)"
  ) +
  theme_minimal() +
  labs(title = "Difference between Non-Ground and Raw RH50 Median - Gaussian") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


# ============================================================================
# BIOGEOGRAPHIC REGIONS ANALYSIS
# ============================================================================

# Read biogeographic regions shapefile
biogeo <- st_read("Data/BiogeoRegions2016.shp")

# Check the structure and column names
print("=== Biogeographic Regions Info ===")
print(names(biogeo))
print(head(biogeo))

# Ensure both datasets have the same CRS
merged_data <- st_transform(merged_data, st_crs(biogeo))

# Perform spatial join to assign biogeographic region to each grid cell
merged_data_biogeo <- st_join(merged_data, biogeo, join = st_intersects)

# Check which column contains the region codes/names
# Adjust 'code' to the actual column name in your shapefile
print("=== After spatial join ===")
print(names(merged_data_biogeo))
print(table(merged_data_biogeo$code))  # Replace 'code' with actual column name

# Summary statistics by biogeographic region
biogeo_summary <- merged_data_biogeo %>%
  st_drop_geometry() %>%
  group_by(code) %>%  # Replace 'code' with actual column name
  summarise(
    n_cells = n(),
    mean_delta = mean(delta, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    min_delta = min(delta, na.rm = TRUE),
    max_delta = max(delta, na.rm = TRUE),
    mean_rh50 = mean(rh50_median, na.rm = TRUE),
    mean_rh_ng = mean(rh_non_ground50_median, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_delta))

print("\n=== Summary by Biogeographic Region ===")
print(biogeo_summary)

# Plot delta by biogeographic region - Faceted map
ggplot(merged_data_biogeo) +
  geom_sf(aes(fill = delta), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Delta (m)"
  ) +
  facet_wrap(~code) +  # Replace 'code' with actual column name
  theme_minimal() +
  labs(title = "Delta RH50 by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Boxplot of delta values by biogeographic region
ggplot(merged_data_biogeo %>% st_drop_geometry(), 
       aes(x = reorder(code, delta, FUN = median), y = delta, fill = code)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta RH50 by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta (m)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Violin plot
ggplot(merged_data_biogeo %>% st_drop_geometry(), 
       aes(x = reorder(code, delta, FUN = median), y = delta, fill = code)) +
  geom_violin() +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta RH50 by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta (m)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Map with biogeographic region boundaries overlaid
ggplot() +
  geom_sf(data = merged_data_biogeo, aes(fill = delta), color = NA) +
  geom_sf(data = biogeo, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Delta (m)"
  ) +
  theme_minimal() +
  labs(title = "Delta RH50 with Biogeographic Region Boundaries") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

# Density plot by biogeographic region
ggplot(merged_data_biogeo %>% st_drop_geometry(), 
       aes(x = delta, fill = code)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Density Distribution of Delta RH50 by Biogeographic Region",
    x = "Delta (m)",
    y = "Density",
    fill = "Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )




# Filter out unwanted biogeographic regions
regions_to_remove <- c("NA", "Steppic", "BlackSea", "Anatolian", "Outside")

merged_data_biogeo_filtered <- merged_data_biogeo %>%
  drop_na(delta)|>
  filter(!code %in% regions_to_remove)%>%
  filter(!is.na(code))

# Update summary statistics with filtered data
biogeo_summary_filtered <- merged_data_biogeo_filtered %>%
  st_drop_geometry() %>%
  group_by(code) %>%
  summarise(
    n_cells = n(),
    mean_delta = mean(delta, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    min_delta = min(delta, na.rm = TRUE),
    max_delta = max(delta, na.rm = TRUE),
    mean_rh50 = mean(rh50_median, na.rm = TRUE),
    mean_rh_ng = mean(rh_non_ground50_median, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_delta))

print("\n=== Summary by Biogeographic Region (Filtered) ===")
print(biogeo_summary_filtered)

# Plot delta by biogeographic region - Faceted map (filtered)
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = delta), color = NA) +
  scale_fill_viridis_b(
    # low = "blue",
    # mid = "white",
    # high = "red",
    # midpoint = 0,
    name = "Delta (m)"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Delta RH50 by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Boxplot (filtered)
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, delta, FUN = median), y = delta, fill = code)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta RH50 by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta (m)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Violin plot (filtered)
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, delta, FUN = median), y = delta, fill = code)) +
  geom_violin() +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta RH50 by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta (m)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Density plot (filtered)
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = delta, fill = code)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Density Distribution of Delta RH50 by Biogeographic Region",
    x = "Delta (m)",
    y = "Density",
    fill = "Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Map with filtered biogeographic region boundaries
biogeo_filtered <- biogeo %>%
  filter(!code %in% regions_to_remove)

ggplot() +
  geom_sf(data = merged_data_biogeo_filtered, aes(fill = delta), color = NA) +
  geom_sf(data = biogeo_filtered, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Delta (m)"
  ) +
  theme_minimal() +
  labs(title = "Delta RH50 with Biogeographic Region Boundaries") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )
