# Load necessary libraries
library(tidycensus)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(tidyr)
library(tigris)      # For county shapefiles
library(sf)          # For spatial operations
library(ggthemes)    # Optional: for better map themes
library(viridis)     # For color scales

rural_urban <- read.csv("C:/Users/barboza-salerno.1/Downloads/RuralAtlasData24.csv") %>%
  dplyr::select(FIPStxt, State, County, RuralUrbanContinuumCode2013) %>%
  mutate(GEOID = str_pad(as.character(as.numeric(FIPStxt)), width = 5, side = "left", pad = "0"))


# Fetch ACS data for SNAP receipt
Sup_Income_Receipt <- get_acs(
  geography = "county",
  variables = c("B22001_002", "B22001_001"),
  year = 2020
)

# Reshape data from long to wide format
Sup_Income_Receipt_wide <- Sup_Income_Receipt |> 
  select(GEOID, NAME, variable, estimate, moe) |> 
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) |> 
  # Calculate proportion of households receiving SNAP
  mutate(
    Proportion_SNAP = estimate_B22001_002 / estimate_B22001_001,
    # MOE formula for proportions
    MOE_Proportion_SNAP = sqrt(
      (moe_B22001_002 / estimate_B22001_002)^2 +
        (moe_B22001_001 / estimate_B22001_001)^2
    ) * Proportion_SNAP,
    # Extract town name
    town = str_remove(NAME, " town, York County, Maine| city, York County, Maine")
  ) |> 
  select(GEOID, town, Proportion_SNAP, MOE_Proportion_SNAP) %>%
  dplyr::left_join(rural_urban) %>%
  dplyr::mutate(RuralUrban = ifelse(RuralUrbanContinuumCode2013 >= 8, 1, 0))

# View the final dataset
print(Sup_Income_Receipt_wide)

# Plot with error bars
ggplot(Sup_Income_Receipt_wide, aes(x = Proportion_SNAP, y = fct_reorder(town, Proportion_SNAP))) + 
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(xmin = Proportion_SNAP - MOE_Proportion_SNAP, 
                    xmax = Proportion_SNAP + MOE_Proportion_SNAP), 
                width = 0.2, color = "black") +
  labs(
    title = "Proportion of Households Receiving SNAP in York County, ME",
    x = "Proportion Receiving SNAP (with MOE)",
    y = "Town"
  ) +
  theme_minimal()

# Download county shapefile, keeping only contiguous U.S. states
counties_sf <- counties(cb = TRUE, resolution = "5m", class = "sf") |> 
  filter(!(STATEFP %in% c("02", "15", "72"))) |>  # Exclude Alaska (02), Hawaii (15), Puerto Rico (72)
  filter(as.numeric(STATEFP) < 60) |>
  select(GEOID, NAME, STATEFP)  # Keep necessary columns

# Merge county shapefile with SNAP data
counties_map <- counties_sf |> 
  left_join(Sup_Income_Receipt_wide, by = "GEOID")

ggplot(counties_map) +
  # Non-rural counties: No border
  geom_sf(data = counties_map |> filter(RuralUrban == 0), 
          aes(fill = Proportion_SNAP), color = NA, size = 0) +  
  
  # Rural counties: White border
  geom_sf(data = counties_map |> filter(RuralUrban == 1), 
          aes(fill = Proportion_SNAP), color = "white", size = 0.3) +  
  
  # Color scale for SNAP proportion
  scale_fill_viridis_c(option = "magma", name = "SNAP %") +  
  
  # Labels and theme
  labs(
    title = "Proportion of Households Receiving SNAP by County (Contiguous U.S.)",
    subtitle = "Rural counties outlined in white",
    caption = "Data: ACS 5-Year 2020 & Rural-Urban Continuum Codes"
  ) +
  theme_void()

library(forcats)

# Filter for West Virginia only
wv_data <- Sup_Income_Receipt_wide |> 
  filter(State == "WV")

# Create a data frame with state abbreviations, FIPS codes, and 2024 voting outcome
state_voting_2024 <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", "KS", "KY", "LA", "ME", "MI", "MS", "MO", "MT", "NE", "NV", "NH", "NC", "ND", "OH", "OK", "PA", "SC", "SD", "TN", "TX", "UT", "VA", "WV", "WI", "WY",
            "CA", "CO", "CT", "DE", "DC", "HI", "IL", "MD", "MA", "MN", "NJ", "NM", "NY", "OR", "RI", "VT", "WA"),
  fips = c( "01", "02", "04", "05", "12", "13", "16", "18", "19", "20", "21", "22", "23", "26", "28", "29", "30", "31", "32", "33", "37", "38", "39", "40", "42", "45", "46", "47", "48", "49", "51", "54", "55", "56",
            "06", "08", "09", "10", "11", "15", "17", "24", "25", "27", "34", "35", "36", "41", "44", "50", "53"),
  voted_trump = c(rep(1, 34), rep(0, 17))  # 1 = Voted Trump, 0 = Did Not Vote Trump
)

# Display the data frame
print(state_voting_2024)

Sup_Income_Receipt_wide <- Sup_Income_Receipt_wide %>%
  dplyr::left_join(state_voting_2024)

# Summarize SNAP proportion by Rural/Urban classification
snap_summary <- Sup_Income_Receipt_wide |> 
  dplyr::group_by(voted_trump) |> 
  summarize(
    Mean_SNAP_Proportion = mean(Proportion_SNAP, na.rm = TRUE),
    SD_SNAP_Proportion = sd(Proportion_SNAP, na.rm = TRUE),
    Median_SNAP_Proportion = median(Proportion_SNAP, na.rm = TRUE),
    Min_SNAP_Proportion = min(Proportion_SNAP, na.rm = TRUE),
    Max_SNAP_Proportion = max(Proportion_SNAP, na.rm = TRUE),
    n = n()  # Number of counties in each category
  ) 
# Display the summarized table
snap_summary
