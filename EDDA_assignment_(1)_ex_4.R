# Load the MASS package and view the npk dataset
library(MASS)
data(npk)
npk
######----- A -------#####
#Create a matrix of random plots
random_plots_matrix <- cbind(rep(1:24),rep(1:6, each = 4),
                             replicate(3, c(replicate(6, sample(c(1,1,0,0))))))
#Convert the matrix to a data frame
data_frame <- data.frame(random_plots_matrix)
#Set column names
header <- c("plot", "block", "N", "P", "K")
colnames(data_frame) <- header
#View the resulting data frame
head(data_frame)
data_frame

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


avg_yield <- apply(cbind(nitrogen_avg_yield[[2]], no_nitrogen_avg_yield[[2]]),1,function(x) unname(unlist(x)))
rownames(avg_yield)<- c("nitrogen_avg_yield","no_nitrogen_avg_yield")
avg_yield

barplot(avg_yield, col = c("#1b98e0", "#353436"),beside = TRUE,
        xlab="blocks",ylab = "yield", ylim=c(0,90),
        names.arg=c("1", "2", "3", "4", "5", "6"))

legend("topleft", legend = c("Nitrogen Treatment", "No Nitrogen Treatment"), 
       fill =c("#1b98e0", "#353436"))


######----- D -------#####
qqnorm(npk$yield)
qqline(npk$yield)

shapiro.test(npk$yield)

model <- aov(yield ~ block + N, data = npk)
summary(model)

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






