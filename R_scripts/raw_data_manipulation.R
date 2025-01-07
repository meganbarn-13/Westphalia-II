# annual precipitation data manipulation
#splitting precipitation data ====
library(dplyr)

data <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/raw_data-files/average-precipitation-per-year.csv")

data_1950_1980 <- data %>%
  filter(Year >= 1950 & Year <= 1980)

data_1982_2023 <- data %>%
  filter(Year >= 1982 & Year <= 2023)

write.csv(data_1950_1980, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precipitation_1950-1980.csv", row.names = FALSE)
write.csv(data_1982_2023, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precipitation_1982-2023.csv", row.names = FALSE)

# calculate abs value weighted deviation from 1950-80 baseline ====
baseline_precip <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/baseline_precip.csv")
current_precip <- read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/current_precip.csv")

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

# weighted deviation by country
weighted_deviation <- deviation_data %>%
  group_by(Code) %>%
  summarise(
    avg_weighted_deviation = mean(abs_deviation, na.rm = TRUE)
  )

write.csv(weighted_deviation, "/Users/home/Documents/Westphalia-Code-II/Westphalia-II/precip_weighted_dev.csv", row.names = FALSE)
