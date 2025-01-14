# libraries ====
library(ggcorrplot)
library(moments)
library(ggpubr)

# check data ====
state_data = read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/values_for_regression.csv")
head(state_data)
dim(state_data)
names(state_data)

min(state_data$precip_dev)
max(state_data$precip_dev)

plot(state_data$temp_dev, state_data$problem_solving_score)
plot(state_data$precip_dev, state_data$problem_solvng_score)

# distributions
hist(state_data$temp_dev) # positive skew
  skewness(state_data$temp_dev, na.rm = TRUE) # 1.745624
hist(state_data$precip_dev) # positive skew
  skewness(state_data$precip_dev, na.rm = TRUE) # 1.183879
hist(state_data$problem_solving_score) # negative skew
  skewness(state_data$problem_solving_score, na.rm = TRUE) # -0.2703436
  
# transformation for normal distribution
state_data$temp_dev <- sqrt(state_data$temp_dev)
state_data$precip_dev <- sqrt(state_data$precip_dev)
state_data$problem_solving_score <- sqrt(max(state_data$problem_solving_score + 1) - state_data$problem_solving_score)
  


# multicollinearity
trimmed_data <- subset(state_data, select = -problem_solving_score + Entity) # remove problem solving score and nonnumeric column
corr_matrix = round(cor(trimmed_data), 2) # compute correlation at 2 decimal places
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)


# multivariate regression model ====
env_model = lm(formula = problem_solving_score ~ temp_dev + precip_dev,
               data = state_data)
summary(env_model) # print result

# check assumptions ====
residuals = env_model$residuals
hist(residuals)
qqnorm(residuals)
qqline(residuals)


