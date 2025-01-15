# libraries ====
library(ggcorrplot)
library(ggplot2)
library(moments)
library(ggpubr)
library(car)

# check data ====
state_data = read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/values_for_regression.csv")
head(state_data)
dim(state_data)
names(state_data)

# distributions
hist(state_data$temp_dev, main = 'Original distribution of temp_dev', xlab = 'Abs Annual Deviation in Temp (K)') # positive skew
  skewness(state_data$temp_dev, na.rm = TRUE) # 1.745624
hist(state_data$precip_dev, main = 'Original distribution of precip_dev', xlab = 'Abs Annual Deviation in Precip (mm)') # positive skew
  skewness(state_data$precip_dev, na.rm = TRUE) # 1.183879
hist(state_data$problem_solving_score, main = 'Original distribution of problem_solving_score', xlab = 'Score out of 10') # negative skew
  skewness(state_data$problem_solving_score, na.rm = TRUE) # -0.2703436
  
# transformation for normal distribution
state_data$temp_dev <- sqrt(state_data$temp_dev)
state_data$precip_dev <- sqrt(state_data$precip_dev)
state_data$problem_solving_score <- sqrt(max(state_data$problem_solving_score + 1) - state_data$problem_solving_score)

# transformed distributions
hist(state_data$temp_dev, main = 'Transformed distribution of temp_dev', xlab = 'sqrt[Abs Annual Deviation in Temp (K)]') 
  skewness(state_data$temp_dev, na.rm = TRUE) # 0.6965441 improved distribution
hist(state_data$precip_dev, main = 'Transformed distribution of precip_dev', xlab = 'sqrt[Abs Annual Deviation in Precip (mm)]') 
  skewness(state_data$precip_dev, na.rm = TRUE) # 0.4164585 improved distribution
hist(state_data$problem_solving_score, main = 'Transformed distribution of problem_solving_score', xlab = '1/sqrt(Score out of 10)') 
  skewness(state_data$problem_solving_score, na.rm = TRUE) # -0.22452218 improved distribution

# multivariate regression model ====
env_model = lm(formula = problem_solving_score ~ temp_dev + precip_dev,
               data = state_data)
summary(env_model) # print result

# check assumptions ====
residuals = env_model$residuals
hist(residuals)
qqnorm(residuals)
qqline(residuals)

rstandard(env_model)
plot(env_model, which = 1, main = "Standardized Residuals Plot")

# multicollinearity
trimmed_data <- subset(state_data, select = -problem_solving_score + Entity) # remove problem solving score and nonnumeric column
corr_matrix = round(cor(trimmed_data), 2) # compute correlation at 2 decimal places
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)
vif(env_model) # temp VIF: 1.11558 // precip VIF: 1.1158

plot(trimmed_data$temp_dev, trimmed_data$problem_solving_score)
plot(trimmed_data$precip_dev, trimmed_data$problem_solvng_score)

# durbin-watson test for autocorrelation
durbinWatsonTest(env_model) # test statistic = 1.772929 // p-value = 0.142
  # p > 0.05, accept H0 and conclude residuals are not autocorrelated



