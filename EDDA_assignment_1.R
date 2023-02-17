my_data <- read.delim(file.choose(),header = TRUE)
data = my_data$birthweight
qqnorm(my_data, main = "Normal Q-Q Plot")
