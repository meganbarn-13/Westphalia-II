# precipitation_calculations
library(dplyr)

precip_data <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/raw_data_files/average-precipitation-per-year.csv")
state_list <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/updated_list_of_states.csv")

# identify states outside of scope
extra_states <- anti_join(precip_data, state_list, by = "Code")
print(extra_states)

# remove states outside of scope
trim_precip_data <- precip_data %>%
  right_join(state_list, by = 'Code')
write.csv(trim_precip_data, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_precip_data.csv")

data_1950_1980 <- trim_precip_data %>%
  filter(Year >= 1950 & Year <= 1980)

data_1982_2023 <- trim_precip_data %>%
  filter(Year >= 1982 & Year <= 2023)

write.csv(data_1950_1980, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_baseline_precip.csv", row.names = FALSE)
write.csv(data_1982_2023, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_current_precip.csv", row.names = FALSE)

# calculate abs value weighted deviation from 1950-80 baseline ====
baseline_precip <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_baseline_precip.csv")
current_precip <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_intermediate_files/trimmed_current_precip.csv")

# baseline average
baseline_avg <- baseline_precip %>%
  group_by(Code) %>%
  summarise(
    baseline_mean = mean(Annual.precipitation, na.rm = TRUE)
  )

# absolute deviations for 1982-2023
deviation_data <- current_precip %>%
  left_join(baseline_avg, by = "Code") %>%
  mutate(
    abs_deviation = abs(Annual.precipitation - baseline_mean)
  )

# weighted deviation by state
weighted_deviation <- deviation_data %>%
  group_by(Code) %>%
  summarise(
    avg_weighted_deviation = mean(abs_deviation, na.rm = TRUE)
  )

write.csv(weighted_deviation, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_weighted_dev.csv", row.names = FALSE)

