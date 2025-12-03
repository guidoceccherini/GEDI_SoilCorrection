library(sf)
library(dplyr)

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
# 
# csv1 <- read.csv("Data/EU_Grid25km_RH50_gauss_Central1.csv")
# 




# Define region names
regions <- c( 'West3', 'West4', 
              'Central1', 'Central2', 
              'East1',
              'East2', 
              'East3',
              'East4', 
              'East5', 'East6' )


# regions <- c( 'West3', 'West4', 
#               'Central1', 'Central2', 
#               'East1',   'East3', 
#               'East5' )



# same for rh100
 csv1 <- read.csv("Data/EU_Grid25km_RH50_gauss_MOMENTS_v2_Central1.csv")
 csv2 <- read.csv("Data/EU_Grid25km_RH50_gauss_MOMENTS_RAW_Central1.csv")
 
 
 csv_filesNG <- paste0("Data/EU_Grid25km_RH50_gauss_MOMENTS_v2_", regions, ".csv")
 combined_csvNG <- lapply(csv_filesNG, function(f) {
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
  filter(rh_ng_kurtosis_x10_count > 1000) %>%
   mutate(
     rh_ng_kurtosis_x10_mean = rh_ng_kurtosis_x10_mean / 10,
     rh_ng_skewness_x10_mean = rh_ng_skewness_x10_mean / 10
     # delta = rh_non_ground50_mean - rh50_mean
   ) %>%
  select(grid_id, numeric_id, region, rh_ng_kurtosis_x10_mean, rh_ng_skewness_x10_mean)


combined_csvNG <- tibble(combined_csvNG)

# raw


csv_filesRAW <- paste0("Data/EU_Grid25km_RH50_gauss_MOMENTS_RAW_", regions, ".csv")
combined_csvRAW <- lapply(csv_filesRAW, function(f) {
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
  filter(rh_ng_kurtosis_x10_count > 1000) %>%
  mutate(
    rh_RAW_kurtosis_x10_mean = rh_ng_kurtosis_x10_mean/10,
    rh_RAW_skewness_x10_mean = rh_ng_skewness_x10_mean/10
  # delta = rh_non_ground50_mean - rh50_mean
  ) %>%
  select(grid_id, numeric_id, region, rh_RAW_kurtosis_x10_mean, rh_RAW_skewness_x10_mean)


combined_csvRAW <- tibble(combined_csvRAW)


# Read and merge all shapefiles
shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, st_read) %>%
  bind_rows()

# Merge shapefile with CSV data
merged_data <- combined_shp %>%##st_drop_geometry() %>%
  left_join(combined_csvNG, by = c("numeric_id", "region"))%>%
  left_join(combined_csvRAW, by = c("numeric_id", "region"))
  
merged_data <- merged_data |>
  mutate(
    Delta_kurtosis = rh_ng_kurtosis_x10_mean - rh_RAW_kurtosis_x10_mean,
    Delta_skewness = rh_ng_skewness_x10_mean - rh_RAW_skewness_x10_mean
  )|>
  drop_na(rh_ng_kurtosis_x10_mean)


# Create map with viridis palette and horizontal legend
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_kurtosis_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "kurtosis Corrected"
  ) +
  theme_minimal() +
  labs(title = "kurtosis mean") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


ggplot(merged_data) +
  geom_sf(aes(fill = rh_RAW_kurtosis_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "kurtosis RAW"
  ) +
  theme_minimal() +
  labs(title = "kurtosis mean") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


# Create map with viridis palette and horizontal legend
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_skewness_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "skewness"
  ) +
  theme_minimal() +
  labs(title = "skewness mean Corrected") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggplot(merged_data) +
  geom_sf(aes(fill = rh_RAW_skewness_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "skewness"
  ) +
  theme_minimal() +
  labs(title = "skewness mean RAW") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggplot(merged_data) +
  geom_sf(aes(fill = Delta_skewness), color = NA) +
  scale_fill_gradient2(high = "darkred", low = "darkblue", mid = "white", midpoint = 0,
                       name = "Delta\n(skewness)") +
    # option = "viridis",  # or "magma", "plasma", "inferno", "cividis"
  #   name = "skewness"
  # ) +
  theme_minimal() +
  labs(title = "Delta skewness") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))



ggplot(merged_data) +
  geom_sf(aes(fill = Delta_kurtosis), color = NA) +
  scale_fill_gradient2(high = "darkred", low = "darkblue", mid = "white", midpoint = 0,
                       name = "Delta\n(kurtosis)") +
  # option = "viridis",  # or "magma", "plasma", "inferno", "cividis"
  #   name = "skewness"
  # ) +
  theme_minimal() +
  labs(title = "Delta kurtosis") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))




# ============================================================================
# BIOGEOGRAPHIC REGIONS ANALYSIS - KURTOSIS AND SKEWNESS
# ============================================================================

# Read biogeographic regions shapefile
biogeo <- st_read("Data/BiogeoRegions2016.shp")

# Ensure both datasets have the same CRS
merged_data <- st_transform(merged_data, st_crs(biogeo))

# Perform spatial join to assign biogeographic region to each grid cell
merged_data_biogeo <- st_join(merged_data, biogeo, join = st_intersects)

# Filter to keep only the 5 main biogeographic regions
merged_data_biogeo_filtered <- merged_data_biogeo %>%
  filter(code %in% c("Alpine", "Atlantic", "Continental", "Mediterranean", "Pannonian"))

# Summary statistics by biogeographic region
biogeo_summary_moments <- merged_data_biogeo_filtered %>%
  st_drop_geometry() %>%
  group_by(code) %>%
  summarise(
    n_cells = n(),
    # Kurtosis statistics
    mean_kurtosis_ng = mean(rh_ng_kurtosis_x10_mean, na.rm = TRUE),
    median_kurtosis_ng = median(rh_ng_kurtosis_x10_mean, na.rm = TRUE),
    sd_kurtosis_ng = sd(rh_ng_kurtosis_x10_mean, na.rm = TRUE),
    mean_kurtosis_raw = mean(rh_RAW_kurtosis_x10_mean, na.rm = TRUE),
    mean_delta_kurtosis = mean(Delta_kurtosis, na.rm = TRUE),
    median_delta_kurtosis = median(Delta_kurtosis, na.rm = TRUE),
    # Skewness statistics
    mean_skewness_ng = mean(rh_ng_skewness_x10_mean, na.rm = TRUE),
    median_skewness_ng = median(rh_ng_skewness_x10_mean, na.rm = TRUE),
    sd_skewness_ng = sd(rh_ng_skewness_x10_mean, na.rm = TRUE),
    mean_skewness_raw = mean(rh_RAW_skewness_x10_mean, na.rm = TRUE),
    mean_delta_skewness = mean(Delta_skewness, na.rm = TRUE),
    median_delta_skewness = median(Delta_skewness, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_delta_kurtosis))

print("\n=== Summary by Biogeographic Region (Kurtosis & Skewness) ===")
print(biogeo_summary_moments)

# ============================================================================
# FACETED MAPS BY BIOGEOGRAPHIC REGION
# ============================================================================

# Kurtosis Corrected by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = rh_ng_kurtosis_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis\nCorrected"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Kurtosis (Corrected) by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Kurtosis RAW by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = rh_RAW_kurtosis_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis\nRAW"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Kurtosis (RAW) by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Delta Kurtosis by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = Delta_kurtosis), color = NA) +
  scale_fill_gradient2(
    high = "darkred",
    low = "darkblue",
    mid = "white",
    midpoint = 0,
    name = "Delta\nKurtosis"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Delta Kurtosis by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Skewness Corrected by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = rh_ng_skewness_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness\nCorrected"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Skewness (Corrected) by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Skewness RAW by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = rh_RAW_skewness_x10_mean), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness\nRAW"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Skewness (RAW) by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Delta Skewness by region
ggplot(merged_data_biogeo_filtered) +
  geom_sf(aes(fill = Delta_skewness), color = NA) +
  scale_fill_gradient2(
    high = "darkred",
    low = "darkblue",
    mid = "white",
    midpoint = 0,
    name = "Delta\nSkewness"
  ) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(title = "Delta Skewness by Biogeographic Region") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# ============================================================================
# BOXPLOTS BY BIOGEOGRAPHIC REGION
# ============================================================================

# Boxplot - Delta Kurtosis
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, Delta_kurtosis, FUN = median), y = Delta_kurtosis, fill = code)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta Kurtosis by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta Kurtosis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Boxplot - Delta Skewness
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, Delta_skewness, FUN = median), y = Delta_skewness, fill = code)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Delta Skewness by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Delta Skewness"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ============================================================================
# VIOLIN PLOTS
# ============================================================================

# Violin plot - Kurtosis Corrected
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, rh_ng_kurtosis_x10_mean, FUN = median), 
           y = rh_ng_kurtosis_x10_mean, fill = code)) +
  geom_violin() +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Kurtosis (Corrected) by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Kurtosis"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Violin plot - Skewness Corrected
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = reorder(code, rh_ng_skewness_x10_mean, FUN = median), 
           y = rh_ng_skewness_x10_mean, fill = code)) +
  geom_violin() +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Skewness (Corrected) by Biogeographic Region",
    x = "Biogeographic Region",
    y = "Skewness"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ============================================================================
# DENSITY PLOTS
# ============================================================================

# Density plot - Delta Kurtosis
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = Delta_kurtosis, fill = code)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Density Distribution of Delta Kurtosis by Biogeographic Region",
    x = "Delta Kurtosis",
    y = "Density",
    fill = "Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Density plot - Delta Skewness
ggplot(merged_data_biogeo_filtered %>% st_drop_geometry(), 
       aes(x = Delta_skewness, fill = code)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Density Distribution of Delta Skewness by Biogeographic Region",
    x = "Delta Skewness",
    y = "Density",
    fill = "Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ============================================================================
# COMBINED MAPS WITH BIOGEOGRAPHIC BOUNDARIES
# ============================================================================

# Filter biogeographic regions shapefile
biogeo_filtered <- biogeo %>%
  filter(code %in% c("Alpine", "Atlantic", "Continental", "Mediterranean", "Pannonian"))

# Delta Kurtosis with boundaries
ggplot() +
  geom_sf(data = merged_data_biogeo_filtered, aes(fill = Delta_kurtosis), color = NA) +
  geom_sf(data = biogeo_filtered, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(
    high = "darkred",
    low = "darkblue",
    mid = "white",
    midpoint = 0,
    name = "Delta\nKurtosis"
  ) +
  theme_minimal() +
  labs(title = "Delta Kurtosis with Biogeographic Region Boundaries") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

# Delta Skewness with boundaries
ggplot() +
  geom_sf(data = merged_data_biogeo_filtered, aes(fill = Delta_skewness), color = NA) +
  geom_sf(data = biogeo_filtered, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(
    high = "darkred",
    low = "darkblue",
    mid = "white",
    midpoint = 0,
    name = "Delta\nSkewness"
  ) +
  theme_minimal() +
  labs(title = "Delta Skewness with Biogeographic Region Boundaries") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )
