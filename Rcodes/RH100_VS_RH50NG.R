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
              'East1', 'East2',  'East3', 'East4', 
              'East5', 'East6' )


# regions <- c( 'West3', 'West4', 
#               'Central1', 'Central2', 
#               'East1',   'East3', 
#               'East5' )


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


# same for rh100
# csv1 <- read.csv("Data/EU_Grid25km_RH100_Central1.csv")
csv_files100 <- paste0("Data/EU_Grid25km_RH100_", regions, ".csv")
combined_csv100 <- lapply(csv_files100, function(f) {
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
  filter(rh100_x10_count > 1000) %>%
  mutate(
    rh100_median = rh100_x10_median / 10,
    # rh_non_ground50_median = rh_non_ground50_x10_median / 10,
    # delta = rh_non_ground50_median - rh50_median
  ) %>%
  select(grid_id, numeric_id, region, rh100_median)


combined_csv100 <- tibble(combined_csv100)




# Read and merge all shapefiles
shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, st_read) %>%
  bind_rows()

# Merge shapefile with CSV data
merged_data <- combined_shp %>%##st_drop_geometry() %>%
  left_join(combined_csv, by = c("numeric_id", "region"))%>%
  left_join(combined_csv100, by = c("numeric_id", "region"))
  
# Create map with diverging palette
# ggplot(merged_data) +
#   geom_sf(aes(fill = delta), color = NA) +
#   scale_fill_gradient2(
#     low = "blue", 
#     mid = "white", 
#     high = "red",
#     midpoint = 0,
#     name = "Delta\n(m)"
#   ) +
#   theme_minimal() +
#   labs(title = "Difference between Non-Ground and Raw RH50 Median") +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "right"
#   )


# Create map with viridis palette and horizontal legend
ggplot(merged_data) +
  geom_sf(aes(fill = rh100_median), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",  # or "magma", "plasma", "inferno", "cividis"
    name = "RH100 (m)"
  ) +
  theme_minimal() +
  labs(title = "RH100 Median") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))



# scatterplot
# Load required packages
library(sf)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Filter out NA values
clean_data <- merged_data %>%
  filter(!is.na(rh100_median) & !is.na(rh_non_ground50_median))

# Basic summary statistics
summary(clean_data$rh100_median)
summary(clean_data$rh_non_ground50_median)

# Check correlation
cor(clean_data$rh100_median, clean_data$rh_non_ground50_median, 
    use = "complete.obs", method = "pearson")


# Create scatterplot with correlation statistics
ggplot(clean_data, aes(x = rh100_median, y = rh_non_ground50_median)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "red") +
  stat_cor(method = "pearson", label.x.npc = 0.1, label.y.npc = 0.9) +
  labs(x = "RH100 (Canopy Height)", 
       y = "RH50 Non-Ground Median",
       title = "GEDI Metrics Correlation") +
  theme_minimal()


# Create bins using quantiles (4 classes)
clean_data <- clean_data %>%
  mutate(rh100_bin = cut(rh100_median, 
                         breaks = quantile(rh100_median, probs = c(0, 0.25, 0.5, 0.75, 1)),
                         labels = c("Low", "Medium-Low", "Medium-High", "High"),
                         include.lowest = TRUE))

# Calculate correlation by bin
# Corrected code - drop geometry before summarising
correlation_by_bin <- clean_data %>%
  st_drop_geometry() %>%  # Remove geometry column
  group_by(rh100_bin) %>%
  summarise(
    correlation = cor(rh100_median, rh_non_ground50_median),
    n = n(),
    mean_rh100 = mean(rh100_median),
    mean_rh50 = mean(rh_non_ground50_median)
  )

print(correlation_by_bin)



# Scatterplot faceted by bins
ggplot(clean_data, aes(x = rh100_median, y = rh_non_ground50_median)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson") +
  facet_wrap(~rh100_bin, scales = "free") +
  labs(title = "Correlation by RH100 Height Classes") +
  theme_bw()


# ADD HEIGHT!!!
# Create summary with ranges
correlation_by_bin <- clean_data %>%
  st_drop_geometry() %>%
  group_by(rh100_bin) %>%
  summarise(
    correlation = cor(rh100_median, rh_non_ground50_median),
    n = n(),
    rh100_min = min(rh100_median),
    rh100_max = max(rh100_median),
    mean_rh100 = mean(rh100_median),
    mean_rh50 = mean(rh_non_ground50_median)
  ) %>%
  mutate(range_label = paste0(rh100_bin, ": [", round(rh100_min, 1), "-", round(rh100_max, 1), "]")) %>%
  arrange(rh100_min)  # Sort by minimum height

# Join back THEN convert to factor
clean_data <- clean_data %>%
  left_join(correlation_by_bin %>% select(rh100_bin, range_label), 
            by = "rh100_bin")



# Plot
ggplot(clean_data, aes(x = rh100_median, y = rh_non_ground50_median)) +
  geom_point(alpha = 0.4, size = 0.8) +
  stat_cor(method = "pearson") +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~range_label, scales = "free") +
  labs(title = "Correlation by RH100 Height Classes",
       x = "RH100 (Canopy Height)",
       y = "RH50 Non-Ground Median") +
  theme_bw()


# TEST RAW DATA


# Filter out NA values
clean_data <- merged_data %>%
  filter(!is.na(rh100_median) & !is.na(rh50_median))

# Basic summary statistics
summary(clean_data$rh100_median)
summary(clean_data$rh50_median)

# Check correlation
cor(clean_data$rh100_median, clean_data$rh50_median, 
    use = "complete.obs", method = "pearson")


# Create scatterplot with correlation statistics
ggplot(clean_data, aes(x = rh100_median, y = rh50_median)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "red") +
  stat_cor(method = "pearson", label.x.npc = 0.1, label.y.npc = 0.9) +
  labs(x = "RH100 (Canopy Height)", 
       y = "RH50 RAW Median",
       title = "GEDI Metrics Correlation before correction") +
  theme_minimal()


# Create bins using quantiles (4 classes)
clean_data <- clean_data %>%
  mutate(rh100_bin = cut(rh100_median, 
                         breaks = quantile(rh100_median, probs = c(0, 0.25, 0.5, 0.75, 1)),
                         labels = c("Low", "Medium-Low", "Medium-High", "High"),
                         include.lowest = TRUE))

# Calculate correlation by bin
# Corrected code - drop geometry before summarising
correlation_by_bin <- clean_data %>%
  st_drop_geometry() %>%  # Remove geometry column
  group_by(rh100_bin) %>%
  summarise(
    correlation = cor(rh100_median, rh50_median),
    n = n(),
    mean_rh100 = mean(rh100_median),
    mean_rh50 = mean(rh50_median)
  )

print(correlation_by_bin)



# Scatterplot faceted by bins
ggplot(clean_data, aes(x = rh100_median, y = rh50_median)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson") +
  facet_wrap(~rh100_bin, scales = "free") +
  labs(title = "Correlation by RH100 Height Classes - RAW RH50") +
  theme_bw()

# 
# # ADD HEIGHT!!!
# # Create summary with ranges
# correlation_by_bin <- clean_data %>%
#   st_drop_geometry() %>%
#   group_by(rh100_bin) %>%
#   summarise(
#     correlation = cor(rh100_median, rh50_median),
#     n = n(),
#     rh100_min = min(rh100_median),
#     rh100_max = max(rh100_median),
#     mean_rh100 = mean(rh100_median),
#     mean_rh50 = mean(rh50_median)
#   ) %>%
#   mutate(range_label = paste0(rh100_bin, ": [", round(rh100_min, 1), "-", round(rh100_max, 1), "]")) %>%
#   arrange(rh100_min)  # Sort by minimum height
# 
# # Join back THEN convert to factor
# clean_data <- clean_data %>%
#   left_join(correlation_by_bin %>% select(rh100_bin, range_label), 
#             by = "rh100_bin")
# 
# 
# 
# # Plot
# ggplot(clean_data, aes(x = rh100_median, y = rh50_median)) +
#   geom_point(alpha = 0.4, size = 0.8) +
#   stat_cor(method = "pearson") +
#   geom_smooth(method = "lm", color = "blue") +
#   facet_wrap(~range_label, scales = "free") +
#   labs(title = "Correlation by RH100 Height Classes",
#        x = "RH100 (Canopy Height)",
#        y = "RH50 RAW Median") +
#   theme_bw()
# 
# 
