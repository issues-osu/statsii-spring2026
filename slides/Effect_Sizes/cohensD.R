# Install and load required packages (if not installed, uncomment install lines)
# install.packages("sociome")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("sf")
# install.packages("tigris")
# install.packages("effectsize")

library(sociome)     # For ADI data
library(ggplot2)     # For visualization
library(dplyr)       # For data manipulation
library(sf)          # For mapping spatial data
library(tigris)      # For county boundaries
library(effectsize)  # For Cohen's d calculation

# --- Step 1: Get ADI Data by County ---
adi_data <- get_adi(year = 2020, geography = "county")

# Extract GEOID, county name, and median family income
adi_data <- adi_data %>%
  dplyr::select(GEOID, NAME, ADI)

# --- Step 2: Load US County Boundaries (Exclude Alaska, Hawaii, & Territories) ---
# List of FIPS codes for non-contiguous states/territories:
non_contiguous_fips <- c("02", "15", "60", "66", "69", "72", "78")  
# 02 = Alaska, 15 = Hawaii, 60 = American Samoa, 66 = Guam, 69 = Northern Mariana Islands, 72 = Puerto Rico, 78 = U.S. Virgin Islands

counties <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  filter(!STATEFP %in% non_contiguous_fips)  # Exclude non-contiguous areas

# Merge ADI data with county geometries
adi_map_data <- counties %>%
  left_join(adi_data, by = c("GEOID" = "GEOID"))

# --- Step 3: Map Median Family Income by County ---
ggplot(adi_map_data) +
  geom_sf(aes(fill = ADI), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "ADI") +
  labs(
    title = "Area Deprivation by County (Contiguous U.S., ADI Data 2020)",
    subtitle = "Excludes Alaska, Hawaii, & U.S. Territories",
    caption = "Source: Sociome ADI 2020"
  ) +
  theme_void()

# --- Step 4: Calculate Cohen's d for High vs. Low Deprivation Counties ---
# Define threshold: Top 25% (most deprived) vs. Bottom 25% (least deprived)
quantiles <- quantile(adi_data$ADI, probs = c(0.25, 0.75), na.rm = TRUE)
low_deprivation <- adi_data %>% filter(ADI <= quantiles[1])
high_deprivation <- adi_data %>% filter(ADI >= quantiles[2])

# Compute Cohen's d
cohens_d_value <- cohens_d(high_deprivation$median_family_income, low_deprivation$median_family_income)

# Print Cohen's d result
print(cohens_d_value)
