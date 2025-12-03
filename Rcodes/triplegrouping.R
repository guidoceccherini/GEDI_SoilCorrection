library(sf)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(viridis)

# ============================================================================
# 1. PARSING FUNCTION FOR TRIPLE GROUPED DATA (4 MOMENT BANDS)
# ============================================================================
# ============================================================================
# PARSING FUNCTION FOR TRIPLE GROUPED DATA (TREE COVER + FOREST + SLOPE)
# ============================================================================

parse_groups_gee_triple <- function(groups_str) {
  # Handle empty or invalid inputs
  if (is.na(groups_str) || groups_str == "" || groups_str == "[]") {
    return(NULL)
  }
  
  tryCatch({
    # GEE exports use "key=value" format instead of JSON "key":value
    # Convert to proper JSON format
    groups_str_clean <- gsub("([a-zA-Z_0-9]+)=", '"\\1":', groups_str)
    
    # Parse JSON into R list structure
    groups <- fromJSON(groups_str_clean, simplifyDataFrame = FALSE)
    
    # Validate parsed data
    if (is.null(groups) || length(groups) == 0) {
      return(NULL)
    }
    
    # Initialize list to collect all rows
    all_rows <- list()
    row_counter <- 0
    
    # ========================================================================
    # LEVEL 1: Loop through TREE COVER groups (outermost)
    # ========================================================================
    for (tc_group in groups) {
      treecover_class <- tc_group$treecover
      
      # Check if this tree cover class has forest management subgroups
      if (is.null(tc_group$groups) || length(tc_group$groups) == 0) {
        next  # Skip if no data
      }
      
      # ======================================================================
      # LEVEL 2: Loop through FOREST MANAGEMENT groups (middle)
      # ======================================================================
      for (fm_group in tc_group$groups) {
        forest_mgmt_class <- fm_group$forest_mgmt
        
        # Check if this forest mgmt class has slope subgroups
        if (is.null(fm_group$groups) || length(fm_group$groups) == 0) {
          next  # Skip if no data
        }
        
        # ====================================================================
        # LEVEL 3: Loop through SLOPE groups (innermost - has actual data)
        # ====================================================================
        for (slope_group in fm_group$groups) {
          slope_class <- slope_group$slope
          
          # Increment row counter
          row_counter <- row_counter + 1
          
          # ==================================================================
          # EXTRACT STATISTICS FOR ALL 4 MOMENT BANDS
          # ==================================================================
          # Band indices: [1] = mean, [2] = std, [3] = skewness, [4] = kurtosis
          
          # Helper function to safely extract array element
          safe_extract <- function(array, index) {
            if (!is.null(array) && length(array) >= index) {
              return(array[index])
            } else {
              return(NA)
            }
          }
          
          # Create data frame for this combination
          all_rows[[row_counter]] <- data.frame(
            # ================================================================
            # GROUPING VARIABLES
            # ================================================================
            treecover_class = treecover_class,
            forest_mgmt_class = forest_mgmt_class,
            slope_class = slope_class,
            
            # ================================================================
            # BAND 1: MEAN HEIGHT
            # ================================================================
            rh_ng_mean_mean = safe_extract(slope_group$mean, 1),
            rh_ng_mean_median = safe_extract(slope_group$median, 1),
            rh_ng_mean_stdDev = safe_extract(slope_group$stdDev, 1),
            rh_ng_mean_min = safe_extract(slope_group$min, 1),
            rh_ng_mean_max = safe_extract(slope_group$max, 1),
            rh_ng_mean_count = safe_extract(slope_group$count, 1),
            rh_ng_mean_p10 = safe_extract(slope_group$p10, 1),
            rh_ng_mean_p25 = safe_extract(slope_group$p25, 1),
            rh_ng_mean_p75 = safe_extract(slope_group$p75, 1),
            rh_ng_mean_p90 = safe_extract(slope_group$p90, 1),
            
            # ================================================================
            # BAND 2: STANDARD DEVIATION
            # ================================================================
            rh_ng_std_mean = safe_extract(slope_group$mean, 2),
            rh_ng_std_median = safe_extract(slope_group$median, 2),
            rh_ng_std_stdDev = safe_extract(slope_group$stdDev, 2),
            rh_ng_std_min = safe_extract(slope_group$min, 2),
            rh_ng_std_max = safe_extract(slope_group$max, 2),
            rh_ng_std_count = safe_extract(slope_group$count, 2),
            rh_ng_std_p10 = safe_extract(slope_group$p10, 2),
            rh_ng_std_p25 = safe_extract(slope_group$p25, 2),
            rh_ng_std_p75 = safe_extract(slope_group$p75, 2),
            rh_ng_std_p90 = safe_extract(slope_group$p90, 2),
            
            # ================================================================
            # BAND 3: SKEWNESS
            # ================================================================
            rh_ng_skewness_mean = safe_extract(slope_group$mean, 3),
            rh_ng_skewness_median = safe_extract(slope_group$median, 3),
            rh_ng_skewness_stdDev = safe_extract(slope_group$stdDev, 3),
            rh_ng_skewness_min = safe_extract(slope_group$min, 3),
            rh_ng_skewness_max = safe_extract(slope_group$max, 3),
            rh_ng_skewness_count = safe_extract(slope_group$count, 3),
            rh_ng_skewness_p10 = safe_extract(slope_group$p10, 3),
            rh_ng_skewness_p25 = safe_extract(slope_group$p25, 3),
            rh_ng_skewness_p75 = safe_extract(slope_group$p75, 3),
            rh_ng_skewness_p90 = safe_extract(slope_group$p90, 3),
            
            # ================================================================
            # BAND 4: KURTOSIS
            # ================================================================
            rh_ng_kurtosis_mean = safe_extract(slope_group$mean, 4),
            rh_ng_kurtosis_median = safe_extract(slope_group$median, 4),
            rh_ng_kurtosis_stdDev = safe_extract(slope_group$stdDev, 4),
            rh_ng_kurtosis_min = safe_extract(slope_group$min, 4),
            rh_ng_kurtosis_max = safe_extract(slope_group$max, 4),
            rh_ng_kurtosis_count = safe_extract(slope_group$count, 4),
            rh_ng_kurtosis_p10 = safe_extract(slope_group$p10, 4),
            rh_ng_kurtosis_p25 = safe_extract(slope_group$p25, 4),
            rh_ng_kurtosis_p75 = safe_extract(slope_group$p75, 4),
            rh_ng_kurtosis_p90 = safe_extract(slope_group$p90, 4),
            
            stringsAsFactors = FALSE
          )
        } # End slope loop
      } # End forest mgmt loop
    } # End tree cover loop
    
    # ========================================================================
    # COMBINE ALL ROWS INTO SINGLE DATA FRAME
    # ========================================================================
    if (length(all_rows) > 0) {
      result <- bind_rows(all_rows)
      return(result)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    warning(paste("Parse error in groups_str:", substr(groups_str, 1, 100), "..."))
    warning(paste("Error message:", e$message))
    return(NULL)
  })
}



# ============================================================================
# 2. READ AND PARSE CSV FILES
# ============================================================================

regions <- c('West3', 'West4', 
             'Central1', 'Central2', 
             'East1', 'East2', 'East3', 'East4', 
             'East5', 'East6',
             'East2a', 'East2b', 'East2c', 'East2d',
             'East4a', 'East4b', 'East4c', 'East4d',
             'East6a', 'East6b', 'East6c', 'East6d')



regions <- c('West3',  
             'Central1',
             'East1',  'East3', 
             'East5', 
             'East2a', 'East2b',
             'East4a', 'East4b', 
             'East6a', 'East6b')

csv_files <- paste0("Data/EU_25km_TreeCover_Forest_Slope_Management_TC_", regions, ".csv")

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
    if (i %% 100 == 0) message(paste("  Processing row", i, "of", nrow(df_filtered)))
    
    groups_df <- parse_groups_gee_triple(df_filtered$groups[i])
    
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
print(paste("Forest mgmt classes:", paste(sort(unique(combined_csv$forest_mgmt_class)), collapse = ", ")))
print(paste("Slope classes:", paste(sort(unique(combined_csv$slope_class)), collapse = ", ")))

print("\\n=== Sample Data ===")
print(head(combined_csv))

# ============================================================================
# 4. CONVERT FROM x10 TO ACTUAL VALUES AND FILTER
# ============================================================================

combined_csv <- combined_csv %>%
  mutate(
    # Ensure all are numeric
    across(c(numeric_id, row, col, treecover_class, forest_mgmt_class, slope_class,
             starts_with("rh_ng_")), as.numeric),
    across(c(center_lon, center_lat), as.numeric),
    
    # Convert from x10 to actual values
    rh_ng_mean_mean_actual = rh_ng_mean_mean / 10,
    rh_ng_std_mean_actual = rh_ng_std_mean / 10,
    rh_ng_skewness_mean_actual = rh_ng_skewness_mean / 10,
    rh_ng_kurtosis_mean_actual = rh_ng_kurtosis_mean / 10,
    
    # Add descriptive labels
    treecover_label = case_when(
      treecover_class == 1 ~ "0-25% TC",
      treecover_class == 2 ~ "25-50% TC",
      treecover_class == 3 ~ "50-75% TC",
      treecover_class == 4 ~ "75-100% TC"
    ),
    forest_mgmt_label = case_when(
      forest_mgmt_class == 1 ~ "Unmanaged",
      forest_mgmt_class == 2 ~ "Close-to-nature",
      forest_mgmt_class == 3 ~ "Combined obj",
      forest_mgmt_class == 4 ~ "Intensive",
      forest_mgmt_class == 5 ~ "Very intensive"
    ),
    slope_label = case_when(
      slope_class == 1 ~ "0-5°",
      slope_class == 2 ~ "5-10°",
      slope_class == 3 ~ "10-15°",
      slope_class == 4 ~ "15-20°",
      slope_class == 5 ~ ">20°"
    )
  ) %>%
  filter(rh_ng_mean_count > 1000)  # Filter by pixel count

print(paste("\\nAfter filtering (count > 1000):", nrow(combined_csv)))

# ============================================================================
# 5. MERGE WITH SHAPEFILES
# ============================================================================

shp_files <- paste0("Data/EU_Grid25km_shapefile_", regions, ".shp")
combined_shp <- lapply(shp_files, function(f) {
  if (file.exists(f)) st_read(f, quiet = TRUE) else NULL
}) %>%
  Filter(Negate(is.null), .) %>%
  bind_rows()

merged_data <- combined_shp %>%
  inner_join(combined_csv, by = c("numeric_id", "region"))

print(paste("\\nMerged spatial data rows:", nrow(merged_data)))

# ============================================================================
# 6. SUMMARY STATISTICS BY GROUPING VARIABLES
# ============================================================================

# Summary by tree cover
summary_by_treecover <- combined_csv %>%
  group_by(treecover_label) %>%
  summarise(
    n_cells = n(),
    mean_height = mean(rh_ng_mean_mean_actual, na.rm = TRUE),
    mean_std = mean(rh_ng_std_mean_actual, na.rm = TRUE),
    mean_skewness = mean(rh_ng_skewness_mean_actual, na.rm = TRUE),
    mean_kurtosis = mean(rh_ng_kurtosis_mean_actual, na.rm = TRUE)
  )

print("\\n=== Summary by Tree Cover ===")
print(summary_by_treecover)

# Summary by forest management
summary_by_forest <- combined_csv %>%
  group_by(forest_mgmt_label) %>%
  summarise(
    n_cells = n(),
    mean_height = mean(rh_ng_mean_mean_actual, na.rm = TRUE),
    mean_std = mean(rh_ng_std_mean_actual, na.rm = TRUE),
    mean_skewness = mean(rh_ng_skewness_mean_actual, na.rm = TRUE),
    mean_kurtosis = mean(rh_ng_kurtosis_mean_actual, na.rm = TRUE)
  )

print("\\n=== Summary by Forest Management ===")
print(summary_by_forest)

# Summary by slope
summary_by_slope <- combined_csv %>%
  group_by(slope_label) %>%
  summarise(
    n_cells = n(),
    mean_height = mean(rh_ng_mean_mean_actual, na.rm = TRUE),
    mean_std = mean(rh_ng_std_mean_actual, na.rm = TRUE),
    mean_skewness = mean(rh_ng_skewness_mean_actual, na.rm = TRUE),
    mean_kurtosis = mean(rh_ng_kurtosis_mean_actual, na.rm = TRUE)
  )

print("\\n=== Summary by Slope ===")
print(summary_by_slope)

# ============================================================================
# 7. SPATIAL MAPS BY TREE COVER (LIKE ORIGINAL CODE)
# ============================================================================

# Skewness by tree cover
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_skewness_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness"
  ) +
  facet_wrap(~treecover_label, ncol = 2) +
  theme_minimal() +
  labs(title = "Skewness by Tree Cover Class")

# Kurtosis by tree cover
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_kurtosis_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis"
  ) +
  facet_wrap(~treecover_label, ncol = 2) +
  theme_minimal() +
  labs(title = "Kurtosis by Tree Cover Class")

# ============================================================================
# 8. SPATIAL MAPS BY FOREST MANAGEMENT
# ============================================================================

# Skewness by forest management
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_skewness_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness"
  ) +
  facet_wrap(~forest_mgmt_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Skewness by Forest Management Type")

# Kurtosis by forest management
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_kurtosis_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis"
  ) +
  facet_wrap(~forest_mgmt_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Kurtosis by Forest Management Type")

# ============================================================================
# 9. SPATIAL MAPS BY SLOPE
# ============================================================================

# Skewness by slope
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_skewness_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness"
  ) +
  facet_wrap(~slope_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Skewness by Slope Class")

# Kurtosis by slope
ggplot(merged_data) +
  geom_sf(aes(fill = rh_ng_kurtosis_mean_actual), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis"
  ) +
  facet_wrap(~slope_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Kurtosis by Slope Class")

# ============================================================================
# 10. INTERACTION PLOTS
# ============================================================================

# Tree cover × Forest management interaction
interaction_tc_fm <- combined_csv %>%
  group_by(treecover_label, forest_mgmt_label) %>%
  summarise(
    mean_skewness = mean(rh_ng_skewness_mean_actual, na.rm = TRUE),
    mean_kurtosis = mean(rh_ng_kurtosis_mean_actual, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Skewness interaction plot
ggplot(interaction_tc_fm, aes(x = treecover_label, y = mean_skewness, 
                               color = forest_mgmt_label, group = forest_mgmt_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Interaction: Tree Cover × Forest Management on Skewness",
    x = "Tree Cover Class",
    y = "Mean Skewness",
    color = "Forest Management"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Kurtosis interaction plot
ggplot(interaction_tc_fm, aes(x = treecover_label, y = mean_kurtosis, 
                               color = forest_mgmt_label, group = forest_mgmt_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Interaction: Tree Cover × Forest Management on Kurtosis",
    x = "Tree Cover Class",
    y = "Mean Kurtosis",
    color = "Forest Management"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ============================================================================
# 11. COMPARE EXTREMES (TC1 vs TC4, Unmanaged vs Very Intensive)
# ============================================================================

# Filter for extremes
data_extremes <- combined_csv %>%
  filter(
    treecover_class %in% c(1, 4) &
    forest_mgmt_class %in% c(1, 5)
  ) %>%
  mutate(
    group_label = paste0(treecover_label, " + ", forest_mgmt_label)
  )

# Box plots comparing extremes
ggplot(data_extremes, aes(x = group_label, y = rh_ng_skewness_mean_actual, fill = group_label)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Skewness: Comparison of Extremes",
    x = "",
    y = "Skewness"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggplot(data_extremes, aes(x = group_label, y = rh_ng_kurtosis_mean_actual, fill = group_label)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Kurtosis: Comparison of Extremes",
    x = "",
    y = "Kurtosis"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# ============================================================================
# 12. CALCULATE DIFFERENCES (TC4 - TC1) FOR EACH FOREST MANAGEMENT TYPE
# ============================================================================

data_diff_by_forest <- combined_csv %>%
  filter(treecover_class %in% c(1, 4)) %>%
  select(grid_id, numeric_id, region, treecover_class, forest_mgmt_class, forest_mgmt_label,
         slope_class, rh_ng_skewness_mean_actual, rh_ng_kurtosis_mean_actual) %>%
  pivot_wider(
    names_from = treecover_class,
    values_from = c(rh_ng_skewness_mean_actual, rh_ng_kurtosis_mean_actual),
    names_sep = "_TC"
  ) %>%
  mutate(
    skewness_diff = rh_ng_skewness_mean_actual_TC4 - rh_ng_skewness_mean_actual_TC1,
    kurtosis_diff = rh_ng_kurtosis_mean_actual_TC4 - rh_ng_kurtosis_mean_actual_TC1
  ) %>%
  filter(!is.na(skewness_diff) & !is.na(kurtosis_diff))

# Merge with shapefile for mapping
merged_diff <- combined_shp %>%
  inner_join(data_diff_by_forest, by = c("numeric_id", "region"))

# Spatial map of differences by forest management
ggplot(merged_diff) +
  geom_sf(aes(fill = skewness_diff), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Skewness\\nDiff"
  ) +
  facet_wrap(~forest_mgmt_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Skewness Difference (TC4 - TC1) by Forest Management Type")

ggplot(merged_diff) +
  geom_sf(aes(fill = kurtosis_diff), color = NA) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    name = "Kurtosis\\nDiff"
  ) +
  facet_wrap(~forest_mgmt_label, ncol = 3) +
  theme_minimal() +
  labs(title = "Kurtosis Difference (TC4 - TC1) by Forest Management Type")

# ============================================================================
# 13. DENSITY PLOTS OF DIFFERENCES BY FOREST MANAGEMENT
# ============================================================================

ggplot(data_diff_by_forest, aes(x = skewness_diff, fill = forest_mgmt_label)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Distribution of Skewness Difference (TC4 - TC1) by Forest Management",
    x = "Skewness Difference",
    y = "Density",
    fill = "Forest Management"
  )

ggplot(data_diff_by_forest, aes(x = kurtosis_diff, fill = forest_mgmt_label)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Distribution of Kurtosis Difference (TC4 - TC1) by Forest Management",
    x = "Kurtosis Difference",
    y = "Density",
    fill = "Forest Management"
  )

# ============================================================================
# 14. EXPORT PROCESSED DATA
# ============================================================================

# Save full processed data
write.csv(combined_csv, "Data/EU_Grid25km_triple_grouped.csv", row.names = FALSE)

# Save differences
write.csv(data_diff_by_forest, "Data/EU_Grid25km_differences_by_forest.csv", row.names = FALSE)

# Save summaries
write.csv(summary_by_treecover, "Data/EU_Grid25km_summary_treecover.csv", row.names = FALSE)
write.csv(summary_by_forest, "Data/EU_Grid25km_summary_forest.csv", row.names = FALSE)
write.csv(summary_by_slope, "Data/EU_Grid25km_summary_slope.csv", row.names = FALSE)

print("\\n=== ANALYSIS COMPLETE ===")
print("All plots generated and data exported!")