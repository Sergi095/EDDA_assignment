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



legend("topleft", legend = c("Nitrogen Treatment", "No Nitrogen Treatment"))

ddf = structure(list(no_nitrogen_avg_yield$yield,nitrogen_avg_yield$yield),
                .Names = c("No Nitrogen Treatment", "Nitrogen Treatment"), class = "data.frame")

barplot(c(no_nitrogen_avg_yield$yield,nitrogen_avg_yield$yield),                                         # Create grouped barchart
        col = c("#1b98e0", "#353436"),beside = TRUE, xlab="blocks",ylab = "yield",
        ylim=c(0,90))
axis(side = 1, at = 1.5, labels =( "1"))
axis(side = 1, at = 4, labels =( "2"))
axis(side = 1, at = 6.5, labels =( "3"))
axis(side = 1, at = 9, labels =( "4"))
axis(side = 1, at = 11.5, labels =( "5"))
axis(side = 1, at = 13.5, labels =( "6"))
legend("topright",                                    # Add legend to barplot
       legend = c("Nitrogen Treatment", "No Nitrogen Treatment"),
       fill = c("#1b98e0", "#353436"))
