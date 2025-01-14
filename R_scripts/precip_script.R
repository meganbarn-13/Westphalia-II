# precipitation_calculations
library(dplyr)

precip_data <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/raw_data_files/average-precipitation-per-year.csv")
state_list <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/updated_list_of_states.csv")

data_1950_1980 <- precip_data %>%
  filter(Year >= 1950 & Year <= 1980)

data_1982_2023 <- precip_data %>%
  filter(Year >= 1982 & Year <= 2023)

write.csv(data_1950_1980, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_baseline_precip.csv", row.names = FALSE)
write.csv(data_1982_2023, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_current_precip.csv", row.names = FALSE)

# baseline average
baseline_avg <- data_1950_1980 %>%
  group_by(Entity) %>%
  summarise(
    baseline_mean = mean(Annual.precipitation, na.rm = TRUE)
  )

# absolute deviations for 1982-2023
deviation_data <- data_1982_2023 %>%
  left_join(baseline_avg, by = "Entity") %>%
  mutate(
    abs_deviation = abs(Annual.precipitation - baseline_mean)
  )

# avg deviation by state
avg_precip_deviation <- deviation_data %>%
  group_by(Entity) %>%
  summarise(
    avg_precip_deviation = mean(abs_deviation, na.rm = TRUE)
  )

# remove states outside of scope
trim_avg_dev <- avg_precip_deviation %>%
  inner_join(state_list, by = 'Entity')

write.csv(trim_avg_dev, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/final_indicator_files/precip_dev.csv", row.names = FALSE)

