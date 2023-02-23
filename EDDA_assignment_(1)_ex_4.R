# Load the MASS package and view the npk dataset
library(MASS)
data(npk)
npk
######----- A -------#####
# Create a data frame to store the randomized treatments
treatments <- data.frame(yield = numeric(24), block = rep(1:6, each = 4),
                         N = numeric(24), P = numeric(24), K = numeric(24))

# Randomly assign treatments to the plots

for(i in 1:6){
  ind <- which(treatments$block == i)
  treatments[ind, c("N", "P", "K")] <- t(replicate(2, sample(c(0, 1), 3,replace = TRUE)))
}

# View the randomized treatments
head(treatments)


######----- B -------#####

# Subset the data to only include nitrogen treatment
npk_nitrogen  <- npk[npk$N == 1,]

# Calculate the average yield per block for nitrogen treatment
nitrogen_avg_yield  <- aggregate(yield ~ block, data = npk_nitrogen, FUN = mean)

# Subset the data to only include plots without nitrogen treatment
npk_no_nitrogen  <- npk[npk$N == 0,]

# Calculate the average yield per block for plots without nitrogen treatment
no_nitrogen_avg_yield  <- aggregate(yield ~ block, data = npk_no_nitrogen, FUN = mean)



# Make a boxplot comparing the average yield per block for nitrogen and no nitrogen treatments
boxplot(no_nitrogen_avg_yield$yield, nitrogen_avg_yield$yield, 
        names = c("No Nitrogen Treatment", "Nitrogen Treatment"), ylab = "yield")

#legend("topleft", legend = c("Nitrogen Treatment", "No Nitrogen Treatment"))

ddf = structure(list(no_nitrogen_avg_yield$yield,nitrogen_avg_yield$yield),
                .Names = c("No Nitrogen Treatment", "Nitrogen Treatment"), class = "data.frame")

barplot(c(no_nitrogen_avg_yield$yield,nitrogen_avg_yield$yield),
        col = c("#1b98e0", "#353436"),beside = TRUE, xlab="blocks",ylab = "yield",
        ylim=c(0,90))
axis(side = 1, at = 1.5, labels =( "1"))
axis(side = 1, at = 4, labels =( "2"))
axis(side = 1, at = 6.5, labels =( "3"))
axis(side = 1, at = 9, labels =( "4"))
axis(side = 1, at = 11.5, labels =( "5"))
axis(side = 1, at = 13.5, labels =( "6"))
legend("topright", legend = c("Nitrogen Treatment", "No Nitrogen Treatment"),
       fill = c("#1b98e0", "#353436"))


######----- D -------#####

fit <- aov(yield ~ block + N, data = npk)
summary(fit)

# The output of the ANOVA analysis indicates that both factors (block and N) have
# a significant effect on the yield.
# The p-value for block is 0.0262, which is less than 0.05, indicating a significant
# effect of block on yield.
# 
# The p-value for N is 0.0071, which is also less than 0.05, indicating a significant
# effect of N on yield.
# 
# The F-value for block is 3.395, which is relatively small compared to the F-value
# for N (9.360), suggesting that the effect of block on yield is smaller than the effect of N.
# 
# The residual sum of squares is 343.8, indicating the variability of the data points
# around the fitted model.
# 
# In conclusion, it was sensible to include factor block in the model as it has 
# a significant effect on yield. The Friedman test cannot be applied in this case 
# since there is only one observation per combination of block and N.

