# Project Title: Analyzing the Impact of Advertising on Bakery Revenue Using Correlation and Covariance

# Problem Statement:
# A bakery owner wants to understand how spending on social media advertising influences monthly revenue.
# The goal is to compute the correlation coefficient and covariance between advertising spending and revenue
# to determine the strength and direction of their relationship. This insight will help the bakery make
# informed decisions about future advertising investments.

# Data: Advertising spending (in $100) and Revenue (in $1000) for 5 months
ads <- c(3, 5, 6, 8, 10)       # Advertising spending (x)
revenue <- c(4, 6, 7, 10, 11)  # Revenue (y)

# Step 1: Calculate Means
mean_ads <- mean(ads)
mean_revenue <- mean(revenue)

# Step 2: Calculate Correlation Coefficient (r)
numerator <- sum((ads - mean_ads) * (revenue - mean_revenue))
denominator <- sqrt(sum((ads - mean_ads)^2) * sum((revenue - mean_revenue)^2))
correlation_coefficient <- numerator / denominator

# Step 3: Calculate Covariance
covariance <- numerator / length(ads)

# Step 4: Display Results
cat("Correlation Coefficient (r):", correlation_coefficient, "\n")
cat("Covariance:", covariance, "\n")

# Step 5: Interpretation
if (correlation_coefficient > 0.8) {
  cat("Strong positive correlation: Advertising spending significantly influences revenue.\n")
} else if (correlation_coefficient > 0.5) {
  cat("Moderate positive correlation: Advertising spending has a notable influence on revenue.\n")
} else {
  cat("Weak correlation: Advertising spending may not have a significant influence on revenue.\n")
}

# Visualization (optional but helpful)
plot(ads, revenue, 
     main = "Relationship Between Advertising Spending and Revenue", 
     xlab = "Advertising Spending ($100)", 
     ylab = "Revenue ($1000)", 
     pch = 19, col = "blue")
abline(lm(revenue ~ ads), col = "red") # Add a regression line
