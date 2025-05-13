# Load libraries
library(dplyr)
library(readr)
library(ggplot2)

# Read and clean data
TLEdata <- read_csv("data2/Lirman2014TLE.csv")
names(TLEdata) <- gsub(" ", "_", names(TLEdata))

# Filter and clean numeric columns
nursery_data <- TLEdata %>%
  filter(TYPE == "NURSERY") %>%
  mutate(
    TLE1cm = as.numeric(TLE1cm),
    TLE2cm = as.numeric(TLE2cm),
    NBranches_1 = as.numeric(NBranches_1),
    NBranches2 = as.numeric(NBranches2)
  ) %>%
  filter(!is.na(TLE1cm), !is.na(TLE2cm))

# Assign initial size class
nursery_data <- nursery_data %>%
  mutate(
    size_class = case_when(
      TLE1cm <= 10 ~ "Small (≤10 cm)",
      TLE1cm > 10 & TLE1cm <= 15 ~ "Medium (10–15 cm)",
      TLE1cm > 15 ~ "Large (>15 cm)"
    )
  )

# Calculate non-normalized TLE growth (colony-level)
nursery_data <- nursery_data %>%
  mutate(
    TLE1_total_mm = TLE1cm * 10,
    TLE2_total_mm = TLE2cm * 10,
    TLE_growth_mm_day = (TLE2_total_mm - TLE1_total_mm) / 365
  )

# Calculate branch-normalized LE rate (only for rows with branch data)
nursery_data <- nursery_data %>%
  mutate(
    TLE1_per_branch_mm = (TLE1cm * 10) / NBranches_1,
    TLE2_per_branch_mm = (TLE2cm * 10) / NBranches2,
    LE_rate_mm_day = (TLE2_per_branch_mm - TLE1_per_branch_mm) / 365
  )

# Summarize both growth metrics by size class
growth_summary <- nursery_data %>%
  group_by(size_class) %>%
  summarise(
    n = n(),
    mean_TLE_mm_day = mean(TLE_growth_mm_day, na.rm = TRUE),
    sd_TLE_mm_day = sd(TLE_growth_mm_day, na.rm = TRUE),
    mean_LE_mm_day = mean(LE_rate_mm_day, na.rm = TRUE),
    sd_LE_mm_day = sd(LE_rate_mm_day, na.rm = TRUE)
  )

# View summary table
print(growth_summary)

# Optional: Plot colony-level TLE growth rate by size class
ggplot(growth_summary, aes(x = size_class, y = mean_TLE_mm_day)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean_TLE_mm_day - sd_TLE_mm_day,
                    ymax = mean_TLE_mm_day + sd_TLE_mm_day),
                width = 0.2) +
  labs(
    title = "Colony-Level TLE Growth Rate by Initial Fragment Size",
    x = "Initial Fragment Size Class",
    y = "TLE Growth Rate (mm/day)"
  ) +
  theme_minimal(base_size = 14)
