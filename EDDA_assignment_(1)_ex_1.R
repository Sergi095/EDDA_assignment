# Read the data
birthweight = read.table("birthweight.txt", header=TRUE)
data= birthweight$birthweight

###--------- Exercise 1 ----------####
## A
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


mu <- mean(data)
stnd <-sd(data)
CI <- 0.96
alpha <- 1-CI
# Calculate the margin of error for a 95% confidence interval
z <- qnorm(1 - alpha/2) # 98th percentile of standard normal distribution
# Calculate the margin of error
me <- z * stnd / sqrt(length(data))

# Calculate the confidence interval
lower_ci = mu - me
upper_ci = mu + me

# Print the confidence interval
cat("Confidence Interval: [", lower_ci, ", ", upper_ci, "]", sep = "")


# construct a bounded 96%-CI for mu(mean) 
for (sample_size in 1:1000) {
  lower_bound = mu - z*stnd/sqrt(sample_size)
  upper_bound = mu + z*stnd/sqrt(sample_size)
  CI_length <- upper_bound - lower_bound
  if (CI_length <= 100) {
    break
  }
}
sample_size
CI_length

# Compute a bootstrap 96%-CI for mu and compare it to the above CI.
library(boot)
B <- 1000 # Choose number of bootstrap resamples
boot_data <- boot(data, statistic = function(data, i) mean(data[i]), R = B)
boot_ci <- boot.ci(boot_data, type = "perc", conf = 0.96)
lower_bound_boot_ci <- boot_ci$percent[[4]]
upper_bound_boot_ci <- boot_ci$percent[[5]]
cat("boot_CI Confidence Interval: [", lower_bound_boot_ci, ", ", upper_bound_boot_ci, "]", sep = "")



###----------####
## B
# Verify this claim by using a one side t-test
t.test(data, mu = 2800, alternative = "greater")
# performed a one-sample t-test using the t.test() function with the 
# null hypothesis that the mean birthweight is equal to 2800 grams (mu = 2800)
# and the alternative hypothesis that the mean birthweight is greater than 2800 
# grams (alternative = "greater"). The resulting output provides the test 
# statistic, degrees of freedom, p-value, and confidence interval.
# The confidence interval in the output provides a range of plausible values
# for the true mean birthweight based on the sample data


# sign test
binom.test(sum(data > 2800), n=length(data), p = 0.5, alternative = "greater")
#counted the number of birthweights that were greater than 2800 grams 
#(sum(data > 2800)) and used the total sample size as the number of trials(n).
#We specified a null hypothesis of a 50% chance of observing a birthweight 
#greater than 2800 grams (p = 0.5) and the alternative hypothesis that the
#proportion of birthweights greater than 2800 grams is greater than 50% 
#(alternative = "greater").

###----------####
## C
power.t.test(n = length(birthweight$birthweight), delta = (2800 - mean(birthweight$birthweight)) / 
               sd(birthweight$birthweight), 
             sd = sd(birthweight$birthweight), sig.level = 0.05, type = "one.sample", alternative = "two.sided")


B = 1000; n = 50
psign = numeric(B) # will contain P-values of sign test
pttest = numeric(B) # will contain P-values of T test
for(i in 1:B) {
  x = sample(data, n)
  pttest[i] = t.test(x, mu=2800, alt='g')[[3]] # extract P-value
  psign[i] = binom.test(sum(x>2800), n, alt='g')[[3]] # extract P-value
}
power_ttest = sum(pttest<0.05)/B
power_ttest
power_sign = sum(psign<0.05)/B
power_sign
# probability of rejecting H0 under t-test is bigger 
# The t-test is more suitable for data that follows a normal distribution.


####----- D ----####

# Set the number of samples to take and the lower probability
n_samples = 100
p_left = 0.25
# Create an empty vector to store the sample probabilities
sample_probabilities = numeric(n_samples)
# Take n_samples samples of size n_samples from the birthweight vector
# Calculate the proportion of samples that have a weight less than 2600 grams
# Store the sample proportion in the sample_probabilities vector
for(i in 1:n_samples){
  sample = sample(data, n_samples)
  sample_probabilities[i] = sum(sample < 2600)/n_samples
}
# Calculate the standard deviation of the sample proportions
sample_sd = sd(sample_probabilities)
# Calculate the mean of the sample proportions
p_hat = mean(sample_probabilities)
# margin of error 
me = p_hat - p_left
# Calculate the upper confidence interval for p
p_right = p_hat + me

cat("Confidence Interval: [", p_left, ", ", p_right, "]", sep = "")

confidence_level = ((me/1.96)*2)-1
confidence_level



####----- E ----####

male_means = c()
female_means = c()

for (i in 1:1000) {
  # Select 34 males and 28 females with birthweight < 2600 g
  male_2600 = sample(data[data < 2600], 34)
  female_2600 = sample(data[data < 2600], 28)
 
   # Select 61 males and 65 females with birthweight >= 2600 g
  male_others = sample(data[data >= 2600], 61)
  female_others = sample(data[data >= 2600], 65)
 
   # Combine the selected males and females with birthweight < 2600 g
  males = c(male_2600, male_others)
  females = c(female_2600, female_others)
  
  # Calculate the mean birthweight for males and females
  male_means = c(male_means, mean(males))
  female_means = c(female_means, mean(females))
}
# Calculate the mean of the sample means for males and females
mean(male_means)
mean(female_means)
# Perform a two-sample t-test assuming unequal variances
t.test(male_means, female_means)
