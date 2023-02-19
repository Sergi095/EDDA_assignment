# Read the data
birthweight = read.table("birthweight.txt", header=TRUE)
data= birthweight$birthweight

# Check for normality with QQ plot
qqnorm(data, main = "Normal Q-Q Plot of Birthweight")

# Check for normality with histogram
hist(x = data,breaks=15,main = "Histogram of Birthweights",
     xlab = " Birthweight ",freq = FALSE)
lines(x= density(x= data), col=" red ")

# Check for normality with boxplot
boxplot (data , main =" Boxplot of Birthweight ",
         xlab =" Birthweight ")#horizontal = TRUE)

# calculate Shapiro-Wilk normality test
shapiro_wilk <- shapiro.test(data)
cat(sprintf("Shapiro Wilk test W: %.4f\n", shapiro_wilk[[1]]))
cat(sprintf("Shapiro Wilk test p-value: %.4f\n", shapiro_wilk[[2]]))

# Calculate the upper bound and lower bound CI 
lower_bound <- t.test(data,conf.level=0.96)[[4]][[1]]
upper_bound <- t.test(data,conf.level=0.96)[[4]][[2]]
# Print bounds
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

# Calculate sample mean and standard deviation
xbar <- mean(data)
stnd <-sd(data)

# Calculate standard error of the mean
SEM <- stnd / sqrt(length(data))
# Calculate margin of error for a 96% confidence level
z <- qnorm(0.98) # 98th percentile of standard normal distribution
ME <- z * SEM
# Print margin of error
cat("Margin of error:", round(ME, 2), "\n")

# construct a bounded 96%-CI for mu(mean) 
for (sample_size in 1:1000) {
  lower_bound = xbar - z*stnd/sqrt(sample_size)
  upper_bound = xbar + z*stnd/sqrt(sample_size)
  CI_length <- upper_bound - lower_bound
  if (CI_length <= 100) {
    break
  }
}
sample_size
CI_length



library(boot)
B <- 1000 # Choose number of bootstrap resamples
boot_data <- boot(data, statistic = function(data, i) mean(data[i]), R = B)
boot_ci <- boot.ci(boot_data, type = "perc", conf = 0.96)
boot_ci
