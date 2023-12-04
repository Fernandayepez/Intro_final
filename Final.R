library(gtools)
library(MASS)

df1 <- read.csv("Body_masses.csv")

df1$Body.mass <- log(df1$Body.mass)

results_df <- data.frame()

num_permutations <- 2000

for (i in 1:num_permutations) {
  
  perms <- sample(df1$Body.mass, length(df1$Body.mass), replace = FALSE)
  
  results_df <- rbind(results_df, data.frame(Permutation = i, Body.mass = perms))
}


calculate_sum_squared_differences <- function(vector) {
  squared_differences <- (vector[-1] - vector[-length(vector)])^2
  sum_squared_differences <- sum(squared_differences)
  mean_value <- mean(vector)
  differences <- vector - mean_value
  sum_of_differences <- sum((differences)^{2})
  c_value <- sum_squared_differences / sum_of_differences
  return(c_value)
}

c_value_df <- data.frame(Result = numeric(0))

for (i in 1:num_permutations) {
  perm_values <- results_df$Body.mass[results_df$Permutation == i]
  result <- calculate_sum_squared_differences(perm_values)
  c_value_df <- rbind(c_value_df, data.frame(Result = result))
}


# fitting a normal distribution to my results
fit <- fitdistr(c_value_df$Result, densfun = "normal")
mean_estimate <- fit$estimate["mean"]
sd_estimate <- fit$estimate["sd"]

# I thought it was normal now 
hist(c_value_df$Result, main = "Histogram of C Values", xlab = "C Values", ylab = "Frequency", col = "lightblue", border = "black")

