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




# construct a bounded 96%-CI for mu(mean) 
lower_bound <- t.test(data,conf.level=0.96)[[4]][[1]]
upper_bound <- t.test(data,conf.level=0.96)[[4]][[2]]
# Print bounds
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

# Calculate sample mean and standard deviation
xbar <- mean(data)
stnd <-sd(data)

# Calculate standard error of the mean
SEM <- stnd / sqrt(length(x))
# Calculate margin of error for a 96% confidence level
z <- qnorm(0.98) # 98th percentile of standard normal distribution
ME <- z * SEM
# Print margin of error
cat("Margin of error:", round(ME, 2), "\n")

CI <-2*ME
cat(CI)
n= sqrt((z*stnd)/ME)
cat(n)

# Compute the minimum sample size for the error length to be 100
min_n = ((t_alpha)^2 * s^2) / 50^2
cat(sprintf("Minimum sample size for error length 100: %f\n", min_n))

# Compute bootstrap 96%-CI for the mean and compare to the bounded CI
B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(birthweight,replace=TRUE)
  Tstar[i]=mean(Xstar)}
Tstar25=quantile(Tstar,0.02)
Tstar975=quantile(Tstar,0.98)
sum(Tstar<Tstar25)
T1 = mean(birthweight)
c(2*T1-Tstar975,2*T1-Tstar25)


sample_mean = mean ( data ) # point estimate
margin_of_error = qnorm (0.98 )*( sd( data )/ sqrt ( length ( data )))
lower_bound_1 = sample_mean - margin_of_error
upper_bound_1 = sample_mean + margin_of_error
confidence_interval <- c( lower_bound_1 , upper_bound_1 )

birthweight= data
alpha = 0.04
mean = mean(birthweight)
s = sd(birthweight)
n = length(birthweight)
t_alpha = qt(1 - (alpha/2), df=n-1)
error = t_alpha * (s / sqrt(n))
lower_bound = mean - error
upper_bound = mean + error
ci = c(lower_bound, upper_bound)
cat(sprintf("Bounded 96%%-CI: [%f, %f]\n", ci[1], ci[2]))

# Compute the minimum sample size for the error length to be 100
min_n = ((t_alpha)^2 * s^2) / 50^2
cat(sprintf("Minimum sample size for error length 100: %f\n", min_n))



