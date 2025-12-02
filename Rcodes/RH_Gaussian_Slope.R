library(sf)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(viridis)

# ============================================================================
# 1. FIXED PARSING FUNCTION - HANDLES ARRAYS PROPERLY
# ============================================================================

parse_groups_gee <- function(groups_str) {
  # Skip empty groups
  if (is.na(groups_str) || groups_str == "" || groups_str == "[]") {
    return(NULL)
  }
  
  tryCatch({
    # GEE exports with = instead of : in JSON
    # Replace = with : for valid JSON, but keep arrays as arrays
    groups_str_clean <- gsub("([a-zA-Z_0-9]+)=", '"\\1":', groups_str)
    
    # Parse JSON
    groups <- fromJSON(groups_str_clean, simplifyDataFrame = FALSE)
    
    if (is.null(groups) || length(groups) == 0) {
      return(NULL)
    }
    
    # Process each slope group
    result_list <- lapply(groups, function(group) {
      # Extract slope class
      slope_class <- group$slope
      
      # Extract arrays and separate into band 0 (rh50) and band 1 (rh_ng)
      # Each metric has 2 values: [band0_value, band1_value]
      data.frame(
        slope = slope_class,
        # Band 0 values (rh50_x10)
        rh50_mean = if (!is.null(group$mean)) group$mean[1] else NA,
        rh50_median = if (!is.null(group$median)) group$median[1] else NA,
        rh50_stdDev = if (!is.null(group$stdDev)) group$stdDev[1] else NA,
        rh50_min = if (!is.null(group$min)) group$min[1] else NA,
        rh50_max = if (!is.null(group$max)) group$max[1] else NA,
        rh50_count = if (!is.null(group$count)) group$count[1] else 0,
        rh50_p10 = if (!is.null(group$p10)) group$p10[1] else NA,
        rh50_p25 = if (!is.null(group$p25)) group$p25[1] else NA,
        rh50_p75 = if (!is.null(group$p75)) group$p75[1] else NA,
        rh50_p90 = if (!is.null(group$p90)) group$p90[1] else NA,
        # Band 1 values (rh_non_ground50_x10)
        rh_ng_mean = if (!is.null(group$mean)) group$mean[2] else NA,
        rh_ng_median = if (!is.null(group$median)) group$median[2] else NA,
        rh_ng_stdDev = if (!is.null(group$stdDev)) group$stdDev[2] else NA,
        rh_ng_min = if (!is.null(group$min)) group$min[2] else NA,
        rh_ng_max = if (!is.null(group$max)) group$max[2] else NA,
        rh_ng_count = if (!is.null(group$count)) group$count[2] else 0,
        rh_ng_p10 = if (!is.null(group$p10)) group$p10[2] else NA,
        rh_ng_p25 = if (!is.null(group$p25)) group$p25[2] else NA,
        rh_ng_p75 = if (!is.null(group$p75)) group$p75[2] else NA,
        rh_ng_p90 = if (!is.null(group$p90)) group$p90[2] else NA,
        stringsAsFactors = FALSE
      )
    })
    
    # Combine all slope classes
    result <- bind_rows(result_list)
    return(result)
    
  }, error = function(e) {
    warning(paste("Parse error:", e$message))
    return(NULL)
  })
}

# ============================================================================
# 2. READ AND PARSE CSV FILES
# ============================================================================

regions <- c( 'West3', 'West4', 
              'Central1', 'Central2', 
              'East1', 'East2', 'East3', 'East4', 
              'East5', 'East6')



# Define region names (including subdivisions)
# regions <- c('West3', 'West4', 
#              'Central1', 'Central2', 
#              'East1', 'East3')




csv_files <- paste0("Data/EU_Grid25km_RH50_gauss_SlopeBins", regions, ".csv")

combined_csv <- lapply(csv_files, function(f) {
  if (!file.exists(f)) {
    message(paste("Skipping:", basename(f)))
    return(NULL)
  }
  
  message(paste("Reading:", basename(f)))
  df <- read.csv(f, stringsAsFactors = FALSE)
  
  # Filter out empty groups
  df_filtered <- df %>%
    filter(groups != "[]" & !is.na(groups) & groups != "")
  
  message(paste("  -> Rows with data:", nrow(df_filtered)))
  
  if (nrow(df_filtered) == 0) return(NULL)
  
  # Parse each row
  parsed_data <- lapply(1:nrow(df_filtered), function(i) {
    groups_df <- parse_groups_gee(df_filtered$groups[i])
    
    if (!is.null(groups_df) && nrow(groups_df) > 0) {
      cbind(
        grid_id = df_filtered$grid_id[i],
        numeric_id = df_filtered$numeric_id[i],
        row = df_filtered$row[i],
        col = df_filtered$col[i],
        center_lon = df_filtered$center_lon[i],
        center_lat = df_filtered$center_lat[i],
        region = df_filtered$region[i],
        groups_df,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
  
  parsed_data <- Filter(Negate(is.null), parsed_data)
  
  if (length(parsed_data) > 0) {
    bind_rows(parsed_data)
  } else {
    NULL
  }
}) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()

# ============================================================================
# 3. CHECK RESULTS
# ============================================================================

print("=== PARSING RESULTS ===")
print(paste("Total rows:", nrow(combined_csv)))
print(paste("Unique grid cells:", n_distinct(combined_csv$grid_id)))
print(paste("Slope classes:", paste(sort(unique(combined_csv$slope)), collapse = ", ")))

print("\n=== Sample Data (should have separate numeric columns now) ===")
print(head(combined_csv))
print("\n=== Structure ===")
print(str(combined_csv))

# ============================================================================
# 4. CONVERT FROM x10 TO METERS AND CALCULATE DELTA
# ============================================================================

combined_csv <- combined_csv %>%
  mutate(
    # Ensure all are numeric
    across(c(numeric_id, row, col, slope, 
             starts_with("rh50_"), starts_with("rh_ng_")), as.numeric),
    across(c(center_lon, center_lat), as.numeric),
    
    # Convert from x10 to meters
    rh50_median_m = rh50_median / 10,
    rh_ng_median_m = rh_ng_median / 10,
    rh50_mean_m = rh50_mean / 10,
    rh_ng_mean_m = rh_ng_mean / 10,
    
    # Calculate delta
    delta_median = rh_ng_median_m - rh50_median_m,
    delta_mean = rh_ng_mean_m - rh50_mean_m
  ) %>%
  filter(rh50_count > 1000)

print(paste("\nAfter filtering (count > 100):", nrow(combined_csv)))
print("\n=== Sample with calculated values ===")
print(head(combined_csv %>% select(grid_id, slope, rh50_median_m, rh_ng_median_m, delta_median)))

# ============================================================================
# 5. MERGE WITH SHAPEFILES AND VISUALIZE
# ============================================================================

shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, function(f) {
  if (file.exists(f)) st_read(f, quiet = TRUE) else NULL
}) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()

# Aggregate across slopes
csv_aggregated <- combined_csv %>%
  group_by(grid_id, numeric_id, region) %>%
  summarise(
    delta_median = weighted.mean(delta_median, rh50_count, na.rm = TRUE),
    delta_mean = weighted.mean(delta_mean, rh50_count, na.rm = TRUE),
    rh50_median_m = weighted.mean(rh50_median_m, rh50_count, na.rm = TRUE),
    rh_ng_median_m = weighted.mean(rh_ng_median_m, rh_ng_count, na.rm = TRUE),
    total_count = sum(rh50_count, na.rm = TRUE),
    n_slope_classes = n(),
    .groups = 'drop'
  )

merged_data_agg <- combined_shp %>%
  left_join(csv_aggregated, by = c("numeric_id", "region"))

merged_data_by_slope <- combined_shp %>%
  inner_join(combined_csv, by = c("numeric_id", "region"))

# Visualization
ggplot(merged_data_agg) +
  geom_sf(aes(fill = delta_median), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Delta (m)", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Difference between Non-Ground and Raw RH50 Median - Gaussian") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# By slope class
ggplot(merged_data_by_slope) +
  geom_sf(aes(fill = delta_median), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Delta (m)") +
  facet_wrap(~slope, ncol = 2) +
  theme_minimal() +
  labs(title = "Delta RH50 by Slope Class")

# Create a named vector mapping numbers to descriptive labels
slope_labels <- c(
  `1` = "0-5 degrees",
  `2` = "5-10 degrees",
  `3` = "10-15 degrees",
  `4` = "15-20 degrees",
  `5` = "20-25 degrees"
)

# Update your plot with the labeller argument
ggplot(merged_data_by_slope) +
  geom_sf(aes(fill = delta_median), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Delta (m)") +
  facet_wrap(~slope, ncol = 2, labeller = as_labeller(slope_labels)) +
  theme_minimal() +
  labs(title = "Delta RH50 by Slope Class")




# Summary
summary_by_slope <- combined_csv %>%
  group_by(slope) %>%
  summarise(
    n_cells = n(),
    mean_delta = mean(delta_median, na.rm = TRUE),
    median_delta = median(delta_median, na.rm = TRUE)
  )

print("\n=== Summary by Slope ===")
print(summary_by_slope)


# ============================================================================
# 7. EXPORT PROCESSED DATA
# ============================================================================

# Save aggregated data
# st_write(merged_data_agg, "Data/EU_Grid25km_merged_aggregated.shp", 
#          delete_dsn = TRUE)
# 
# # Save by-slope data
# write.csv(combined_csv, "Data/EU_Grid25km_by_slope.csv", row.names = FALSE)
# 
# # Save summary
# write.csv(summary_by_slope, "Data/EU_Grid25km_slope_summary.csv", row.names = FALSE)
