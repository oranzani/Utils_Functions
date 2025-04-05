# Code for direct standardization of hospitalization rates
library(tidyverse)
library(tidylog)
library(epitools)

## Example Data Creation (replace with your actual data)
# Create example hospitalization data for 30 municipalities
n_municipalities <- 10
age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
sex_categories <- c("Male", "Female")

# Create a sample dataset
municipality_data <- expand.grid(
  municipality = paste0("Municipality_", 1:n_municipalities),
  age_group = age_groups,
  sex = sex_categories
) %>%
  mutate(
    hospitalizations = rpois(n(), lambda = 10), # Random hospitalization counts
    population = rpois(n(), lambda = 1000)     # Random municipal population
  )

# Create Brazil standard population (replace with actual Brazil population data)
brazil_population <- data.frame(
  age_group = age_groups,
  Male = c(15000, 14000, 13000, 12000, 11000, 9000, 7000, 5000),
  Female = c(14500, 13500, 12500, 11500, 10500, 9500, 7500, 6000)
) %>%
  pivot_longer(cols = c(Male, Female), names_to = "sex", values_to = "std_pop")


## Standardization Function
## here we estimate the expected number of events if the municipal population
## would be brazil, ie, if the crude rates we observed in a municipality, or area,
## would be applied to the standard population (brazil)


## Standardization Function
calculate_standardized_rates <- function(data, std_pop, multiplier) {
  # Input validation
  if(multiplier <= 0) stop("Multiplier must be a positive number")
  
  # Join the data with standard population
  combined_data <- data %>%
    left_join(std_pop, by = c("age_group", "sex"))
  
  # Calculate crude rates
  crude_rates <- combined_data %>%
    group_by(municipality) %>%
    summarize(
      total_hospitalizations = sum(hospitalizations),
      total_population = sum(population),
      crude_rate = (total_hospitalizations / total_population) * multiplier
    )
  
  # Calculate age-sex specific rates
  combined_data <- combined_data %>%
    mutate(age_sex_rate = hospitalizations / population)
  
  
  # Direct standardization
  standardized_rates <- combined_data %>%
    group_by(municipality) %>%
    summarize(
      expected_cases = sum(age_sex_rate * std_pop),
      std_pop_total = sum(std_pop),
      standardized_rate = (expected_cases / std_pop_total) * multiplier
    ) %>%
    left_join(crude_rates, by = "municipality") %>% 
    select(municipality, total_hospitalizations, total_population, crude_rate,
           std_pop_total, expected_cases, standardized_rate)
  
  # Add multiplier as an attribute for reference
  attr(standardized_rates, "rate_multiplier") <- multiplier
  return(standardized_rates)
}

# Calculate standardized rates
results <- calculate_standardized_rates(municipality_data, brazil_population,multiplier = 10000)
