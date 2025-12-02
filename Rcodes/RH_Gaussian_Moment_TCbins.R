library(sf)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(viridis)

# ============================================================================
# 1. PARSING FUNCTION FOR TREE COVER GROUPED DATA (4 MOMENT BANDS)
# ============================================================================

parse_groups_gee_treecover <- function(groups_str) {
  # Skip empty groups
  if (is.na(groups_str) || groups_str == "" || groups_str == "[]") {
    return(NULL)
  }
  
  tryCatch({
    # GEE exports with = instead of : in JSON
    groups_str_clean <- gsub("([a-zA-Z_0-9]+)=", '"\\1":', groups_str)
    
    # Parse JSON
    groups <- fromJSON(groups_str_clean, simplifyDataFrame = FALSE)
    
    if (is.null(groups) || length(groups) == 0) {
      return(NULL)
    }
    
    # Process each tree cover group
    result_list <- lapply(groups, function(group) {
      # Extract tree cover class
      treecover_class <- group$treecover_class
      
      # Extract arrays - each metric has 4 values (one per moment band)
      # Band order: [0] mean, [1] std, [2] skewness, [3] kurtosis
      data.frame(
        treecover_class = treecover_class,
        
        # Mean moment (band 0)
        rh_ng_mean_mean = if (!is.null(group$mean)) group$mean[1] else NA,
        rh_ng_mean_median = if (!is.null(group$median)) group$median[1] else NA,
        rh_ng_mean_stdDev = if (!is.null(group$stdDev)) group$stdDev[1] else NA,
        rh_ng_mean_min = if (!is.null(group$min)) group$min[1] else NA,
        rh_ng_mean_max = if (!is.null(group$max)) group$max[1] else NA,
        rh_ng_mean_count = if (!is.null(group$count)) group$count[1] else 0,
        rh_ng_mean_p10 = if (!is.null(group$p10)) group$p10[1] else NA,
        rh_ng_mean_p25 = if (!is.null(group$p25)) group$p25[1] else NA,
        rh_ng_mean_p75 = if (!is.null(group$p75)) group$p75[1] else NA,
        rh_ng_mean_p90 = if (!is.null(group$p90)) group$p90[1] else NA,
        
        # Std moment (band 1)
        rh_ng_std_mean = if (!is.null(group$mean)) group$mean[2] else NA,
        rh_ng_std_median = if (!is.null(group$median)) group$median[2] else NA,
        rh_ng_std_stdDev = if (!is.null(group$stdDev)) group$stdDev[2] else NA,
        rh_ng_std_min = if (!is.null(group$min)) group$min[2] else NA,
        rh_ng_std_max = if (!is.null(group$max)) group$max[2] else NA,
        rh_ng_std_count = if (!is.null(group$count)) group$count[2] else 0,
        rh_ng_std_p10 = if (!is.null(group$p10)) group$p10[2] else NA,
        rh_ng_std_p25 = if (!is.null(group$p25)) group$p25[2] else NA,
        rh_ng_std_p75 = if (!is.null(group$p75)) group$p75[2] else NA,
        rh_ng_std_p90 = if (!is.null(group$p90)) group$p90[2] else NA,
        
        # Skewness moment (band 2)
        rh_ng_skewness_mean = if (!is.null(group$mean)) group$mean[3] else NA,
        rh_ng_skewness_median = if (!is.null(group$median)) group$median[3] else NA,
        rh_ng_skewness_stdDev = if (!is.null(group$stdDev)) group$stdDev[3] else NA,
        rh_ng_skewness_min = if (!is.null(group$min)) group$min[3] else NA,
        rh_ng_skewness_max = if (!is.null(group$max)) group$max[3] else NA,
        rh_ng_skewness_count = if (!is.null(group$count)) group$count[3] else 0,
        rh_ng_skewness_p10 = if (!is.null(group$p10)) group$p10[3] else NA,
        rh_ng_skewness_p25 = if (!is.null(group$p25)) group$p25[3] else NA,
        rh_ng_skewness_p75 = if (!is.null(group$p75)) group$p75[3] else NA,
        rh_ng_skewness_p90 = if (!is.null(group$p90)) group$p90[3] else NA,
        
        # Kurtosis moment (band 3)
        rh_ng_kurtosis_mean = if (!is.null(group$mean)) group$mean[4] else NA,
        rh_ng_kurtosis_median = if (!is.null(group$median)) group$median[4] else NA,
        rh_ng_kurtosis_stdDev = if (!is.null(group$stdDev)) group$stdDev[4] else NA,
        rh_ng_kurtosis_min = if (!is.null(group$min)) group$min[4] else NA,
        rh_ng_kurtosis_max = if (!is.null(group$max)) group$max[4] else NA,
        rh_ng_kurtosis_count = if (!is.null(group$count)) group$count[4] else 0,
        rh_ng_kurtosis_p10 = if (!is.null(group$p10)) group$p10[4] else NA,
        rh_ng_kurtosis_p25 = if (!is.null(group$p25)) group$p25[4] else NA,
        rh_ng_kurtosis_p75 = if (!is.null(group$p75)) group$p75[4] else NA,
        rh_ng_kurtosis_p90 = if (!is.null(group$p90)) group$p90[4] else NA,
        
        stringsAsFactors = FALSE
      )
    })
    
    # Combine all tree cover classes
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

regions <- c('West3', 'West4', 
             'Central1', 'Central2', 
             'East1', 'East2', 'East3', 'East4', 
             'East5', 'East6')

csv_files <- paste0("Data/EU_Grid25km_Moments_gauss_TreeCoverBins", regions, ".csv")

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
    groups_df <- parse_groups_gee_treecover(df_filtered$groups[i])
    
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
print(paste("Tree cover classes:", paste(sort(unique(combined_csv$treecover_class)), collapse = ", ")))

print("\n=== Sample Data ===")
print(head(combined_csv))
print("\n=== Structure ===")
print(str(combined_csv))

# ============================================================================
# 4. CONVERT FROM x10 TO ACTUAL VALUES
# ============================================================================

combined_csv <- combined_csv %>%
  mutate(
    # Ensure all are numeric
    across(c(numeric_id, row, col, treecover_class, 
             starts_with("rh_ng_")), as.numeric),
    across(c(center_lon, center_lat), as.numeric),
    
    # Convert from x10 to actual values (mean, std, skewness, kurtosis)
    # Adjust division factor based on your scaling
    rh_ng_mean_mean_actual = rh_ng_mean_mean / 10,
    rh_ng_std_mean_actual = rh_ng_std_mean / 10,
    rh_ng_skewness_mean_actual = rh_ng_skewness_mean / 10,
    rh_ng_kurtosis_mean_actual = rh_ng_kurtosis_mean / 10
  ) %>%
  filter(rh_ng_mean_count > 1000)  # Filter by pixel count


print(paste("\nAfter filtering (count > 100):", nrow(combined_csv)))
print("\n=== Sample with calculated values ===")
# print(head(combined_csv %>% select(grid_id, slope, rh50_mean_m, rh_ng_mean_m, delta_mean)))

# ============================================================================
# 5. MERGE WITH SHAPEFILES AND VISUALIZE
# ============================================================================

shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, function(f) {
  if (file.exists(f)) st_read(f, quiet = TRUE) else NULL
}) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()


merged_data_by_TC <- combined_shp %>%
  inner_join(combined_csv, by = c("numeric_id", "region"))


# By slope class
ggplot(merged_data_by_TC) +
  geom_sf(aes(fill = rh_ng_skewness_mean), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Delta (m)") +
  facet_wrap(~treecover_class, ncol = 2) +
  theme_minimal() +
  labs(title = "Delta RH50 by Slope Class")




# Create a named vector mapping numbers to descriptive labels
treecover_labels <- c(
  `1` = "0-25% TC",
  `2` = "25-50% TC",
  `3` = "50-75% TC",
  `4` = "75-100% TC"
  # `5` = "20-25 degrees"
)

# Update your plot with the labeller argument
ggplot(merged_data_by_TC) +
  geom_sf(aes(fill = rh_ng_skewness_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "skewness Corrected"
  ) +
  facet_wrap(~treecover_class, ncol = 2, labeller = as_labeller(treecover_labels)) +
  theme_minimal() +
  labs(title = "Skewness by Tree Cover Class")



# Update your plot with the labeller argument
ggplot(merged_data_by_TC) +
  geom_sf(aes(fill = rh_ng_kurtosis_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",      # blue for negative
    mid = "white",      # light yellow for zero
    high = "#D7191C",     # red for positive
    midpoint = 0,
    name = "Kurtosis Corrected"
  ) +
  facet_wrap(~treecover_class, ncol = 2, labeller = as_labeller(treecover_labels)) +
  theme_minimal() +
  labs(title = "Kurtosis by Tree Cover Class")



# class 1



# # Update your plot with the labeller argument
# ggplot(merged_data_by_TC|> dplyr::filter(treecover_class == 4)) +
#   geom_sf(aes(fill = rh_ng_kurtosis_mean_actual), color = NA) +
#   scale_fill_gradient2(
#     low = "#2C7BB6",      # blue for negative
#     mid = "white",      # light yellow for zero
#     high = "#D7191C",     # red for positive
#     midpoint = 0,
#     name = "Kurtosis Corrected"
#   ) +
#   # facet_wrap(~treecover_class, ncol = 2, labeller = as_labeller(treecover_labels)) +
#   theme_minimal() +
#   labs(title = "Kurtosis by Tree Cover Class")




# Summary
summary_by_slope <- combined_csv %>%
  group_by(treecover_class) %>%
  summarise(
    n_cells = n(),
    mean_SK = mean(rh_ng_skewness_mean, na.rm = TRUE),
    mean_ku = mean(rh_ng_kurtosis_mean, na.rm = TRUE)
  )

print("\n=== Summary by Slope ===")
print(summary_by_slope)



# ============================================================================
# 6. CALCULATE DIFFERENCE BETWEEN TREE COVER CLASSES 1 AND 4
# ============================================================================

# Reshape data to have separate columns for each tree cover class
data_tc1_tc4 <- combined_csv %>%
  filter(treecover_class %in% c(1, 4)) %>%
  select(grid_id, numeric_id, region, treecover_class, 
         rh_ng_skewness_mean_actual, rh_ng_kurtosis_mean_actual) %>%
  pivot_wider(
    names_from = treecover_class,
    values_from = c(rh_ng_skewness_mean_actual, rh_ng_kurtosis_mean_actual),
    names_sep = "_TC"
  ) %>%
  mutate(
    # Calculate differences (TC4 - TC1)
    skewness_diff = rh_ng_skewness_mean_actual_TC4 - rh_ng_skewness_mean_actual_TC1,
    kurtosis_diff = rh_ng_kurtosis_mean_actual_TC4 - rh_ng_kurtosis_mean_actual_TC1
  ) %>%
  filter(!is.na(skewness_diff) & !is.na(kurtosis_diff))

print(paste("\nRows with both TC1 and TC4:", nrow(data_tc1_tc4)))
print("\n=== Sample differences ===")
print(head(data_tc1_tc4 %>% select(grid_id, skewness_diff, kurtosis_diff)))

# Merge with shapefiles for mapping
merged_diff <- combined_shp %>%
  inner_join(data_tc1_tc4, by = c("numeric_id", "region"))

# ============================================================================
# 7. PLOT SPATIAL DENSITY MAPS OF DIFFERENCES
# ============================================================================

# Skewness difference map (TC4 - TC1)
ggplot(merged_diff) +
  geom_sf(aes(fill = skewness_diff), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness\nDifference\n(TC4 - TC1)"
  ) +
  theme_minimal() +
  labs(title = "Skewness Difference: High Tree Cover (75-100%) vs Low Tree Cover (0-25%)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

# Kurtosis difference map (TC4 - TC1)
ggplot(merged_diff) +
  geom_sf(aes(fill = kurtosis_diff), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis\nDifference\n(TC4 - TC1)"
  ) +
  theme_minimal() +
  labs(title = "Kurtosis Difference: High Tree Cover (75-100%) vs Low Tree Cover (0-25%)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

# ============================================================================
# 8. HISTOGRAM/DENSITY PLOTS OF DIFFERENCES
# ============================================================================

# Skewness difference density plot
ggplot(data_tc1_tc4, aes(x = skewness_diff)) +
  geom_density(fill = "#2C7BB6", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Distribution of Skewness Difference (TC4 - TC1)",
    x = "Skewness Difference",
    y = "Density"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Kurtosis difference density plot
ggplot(data_tc1_tc4, aes(x = kurtosis_diff)) +
  geom_density(fill = "#D7191C", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Distribution of Kurtosis Difference (TC4 - TC1)",
    x = "Kurtosis Difference",
    y = "Density"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Combined plot with both distributions
library(tidyr)
data_tc1_tc4_long <- data_tc1_tc4 %>%
  select(grid_id, skewness_diff, kurtosis_diff) %>%
  pivot_longer(cols = c(skewness_diff, kurtosis_diff),
               names_to = "metric",
               values_to = "difference")

ggplot(data_tc1_tc4_long, aes(x = difference, fill = metric)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(
    values = c("skewness_diff" = "#2C7BB6", "kurtosis_diff" = "#D7191C"),
    labels = c("Kurtosis", "Skewness")
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of Differences Between High and Low Tree Cover",
    subtitle = "(TC4 - TC1)",
    x = "Difference",
    y = "Density",
    fill = "Metric"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )




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
