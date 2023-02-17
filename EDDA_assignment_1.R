my_data <- read.delim(file.choose(),header = TRUE)
data = my_data$birthweight
qqnorm(data, main = "Normal Q-Q Plot of Birthweight")

hist(x = data,breaks=15,main = "Histogram of Birthweights",
     xlab = " Birthweight ",freq = FALSE)

lines(x= density(x= data), col=" red ")

boxplot (data , main =" Boxplot of Birthweight ",
          xlab =" Birthweight ")#horizontal = TRUE)
