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
