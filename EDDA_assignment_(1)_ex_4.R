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

# Make a plot comparing the average yield per block for nitrogen and no nitrogen treatments
plot(nitrogen_avg_yield$Block, nitrogen_avg_yield$Yield, ylim = range(c(nitrogen_avg_yield$Yield, no_nitrogen_avg_yield$Yield)), xlab = "Block", ylab = "Average Yield (lbs/plot)", main = "Average Yield per Block with and without Nitrogen Treatment")
points(no_nitrogen_avg_yield$Block, no_nitrogen_avg_yield$Yield, col = "red")
legend("topright", legend = c("Nitrogen Treatment", "No Nitrogen Treatment"), col = c("black", "red"), pch = 1)

