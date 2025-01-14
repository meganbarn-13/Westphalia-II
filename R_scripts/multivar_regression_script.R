# libraries ====
library(ggcorrplot)

# multivariate regression model ====
state_data = read.csv("/Users/home/Documents/Westphalia-Code-II/Westphalia-II/values_for_regression.csv")
head(state_data)
dim(state_data)

env_model = lm(formula = problem_solving_score ~ temp_dev + precip_dev, 
               data = state_data)
summary(env_model) # print result

# check assumptions ====
residuals = env_model$residuals
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# multicollinearity
trimmed_data <- subset(state_data, select = -problem_solving_score) # remove problem solving score
corr_matrix = round(cor(trimmed_data), 2) # compute correlation at 2 decimal places
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)



