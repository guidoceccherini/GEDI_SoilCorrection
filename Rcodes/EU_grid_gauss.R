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


