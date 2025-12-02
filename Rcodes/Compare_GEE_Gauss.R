library(ggplot2)
library(readr)
library(tidyverse)


# Load the shot_xxx and reference files
Reference_Matteo <- read_csv("Data/shot_30071100300208676 1.csv")
GEE <- read_csv("Data/GEDI_SOIL_GAUSS_30071100300208676.csv") # replace with actual reference file name


# Use the exact column name printed above
long_colname <- names(Reference_Matteo)[1]

# Proceed as before, replacing 'col1' with your column name
Reference_Matteo_clean <- Reference_Matteo %>%
  filter(!str_detect(.data[[long_colname]], "="), !str_detect(.data[[long_colname]], "beam"), !str_detect(.data[[long_colname]], "Reference_Matteo"))

# Separate header and data
header <- Reference_Matteo_clean[[long_colname]][1]
data <- Reference_Matteo_clean[[long_colname]][-1]

# Read the data as CSV text
Reference_Matteo_df <- readr::read_csv(
  paste(c(header, data), collapse = "\n")
)

Reference_Matteo_df<- Reference_Matteo_df |> mutate(index = row_number())




# Suppose 'ref' is your tibble
# Extract the string and remove square brackets, then split and convert to numeric
rh_ground_vec <- GEE$rh_ground %>%
  .[[1]] %>%
  str_remove_all("\\[|\\]") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

rh_non_ground_vec <- GEE$rh_non_ground %>%
  .[[1]] %>%
  str_remove_all("\\[|\\]") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()
wf_ground_vec <- GEE$wf_ground %>%
  .[[1]] %>%
  str_remove_all("\\[|\\]") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

wf_non_ground_vec <- GEE$wf_non_ground %>%
  .[[1]] %>%
  str_remove_all("\\[|\\]") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

wf_vec <- GEE$wf %>%
  .[[1]] %>%
  str_remove_all("\\[|\\]") %>%
  str_split(",") %>%
  .[[1]] %>%
  as.numeric()

# Now you can check
print(rh_ground_vec)
print(rh_non_ground_vec)
print(wf_ground_vec)
print(wf_non_ground_vec)
print(wf_vec)




library(tibble)

# Assemble combined tibble
comparison_df <- tibble(
  index = Reference_Matteo_df$index,
  raw_rh = Reference_Matteo_df$rh,
  Reference_Matteo_wf_ground_gau = Reference_Matteo_df$wf_ground_gau,
  Reference_Matteo_wf_noground_gau = Reference_Matteo_df$wf_nogroud_gau,
  Reference_Matteo_rh_noground_gau = Reference_Matteo_df$rh_noground_gau,
  GEE_wf_ground = wf_ground_vec,
  GEE_wf_non_ground = wf_non_ground_vec,
  GEE_rh_ground = rh_ground_vec,
  GEE_rh_non_ground = rh_non_ground_vec,
  GEE_wf = wf_vec
  
)

# Optional: pivot to long for grouped line plot
comparison_long <- comparison_df %>%
  select(index,
         # Reference_Matteo_wf_ground_GEE,
         Reference_Matteo_wf_noground_gau,
         # GEE_wf_ground,
         GEE_wf_non_ground) %>%
  pivot_longer(-index, names_to = "series", values_to = "value")

ggplot(comparison_long, aes(x = index, y = value, color = series, linetype = series)) +
  geom_line(size = 1) +
  labs(title = "Waveform Comparison: Reference_Matteo vs GEE",
       x = "Profile Index",
       y = "Waveform Value") +
  theme_minimal()


comparison_long2 <- comparison_df %>%
  select(index,
         Reference_Matteo_wf_ground_gau,
         # Reference_Matteo_wf_noground_gau,
         GEE_wf_ground
         # GEE_wf_non_ground
         ) %>%
  pivot_longer(-index, names_to = "series", values_to = "value")

ggplot(comparison_long2, aes(x = index, y = value, color = series, linetype = series)) +
  geom_line(size = 1) +
  labs(title = "Waveform Comparison: Reference_Matteo vs GEE",
       x = "Profile Index",
       y = "Waveform Value") +
  theme_minimal()
# ggsave("Outputs/Waveform_Comparison.png", width = 10, height = 6)


rh_long <- comparison_df %>%
  select(index, raw_rh, Reference_Matteo_rh_noground_gau, GEE_rh_non_ground) %>%
  pivot_longer(-index, names_to = "series", values_to = "value")

ggplot(rh_long, aes(x = index, y = value, color = series, linetype = series)) +
  geom_line(size = 1) +
  labs(title = "RH Comparison: Reference_Matteo vs GEE",
       x = "Profile Index",
       y = "Relative Height Value") +
  theme_minimal()
# ggsave("Outputs/RH_Comparison.png", width = 10, height = 6)



rh_long <- comparison_df %>%
  select(index, GEE_wf_non_ground, GEE_wf_ground, GEE_wf) %>%
  pivot_longer(-index, names_to = "series", values_to = "value")

ggplot(rh_long, aes(x = index, y = value, color = series, linetype = series)) +
  geom_line(size = 1) +
  labs(title = "Ground Peak Detail - Mirroring Region (GEE only)",
       x = "Profile Index",
       y = "Waveform Value") +
  theme_minimal()
