install.packages("readxl")
install.packages("qgraph", dependencies = TRUE)
library(readxl)
library(qgraph)
setwd("/Users/juliadullin/Documents/Uni_Master/2. Semester/CustomerAnalyticsCustomerInsight/SWP")
data <- read_excel("Data_Chocolate_allinterviews_SWP3.xlsx", sheet = "Attribute Rating", na = "NA")
summary(data)
#data <- read.csv2("Data_Chocolate_Bars_Attribute_Rating.csv")

#b1 <- data[,c("b1_crunchy", "b1_creamy",	"b1_sweet",	"b1_chocolaty",	"b1_healthful",	"b1_calorie",	"b1_rich",	"b1_addiction",	"b1_accessible",	"b1_handy",	"b1_wrapping",	"b1_image",	"b1_commercial")]


person <- c(data$Person, data$Person, data$Person, data$Person, data$Person, data$Person, data$Person, data$Person, data$Person, data$Person)
choc_bar <- rep(c("Snickers", "Kinder Bueno", "Twix", "Mars", "KitKat", "Bounty", "Kinder Riegel", "Balisto Korn-Mix", "Lion", "Duplo"), each = length(data$Person))
crunchy <- c(data$b1_crunchy, data$b2_crunchy, data$b3_crunchy, data$b4_crunchy, data$b5_crunchy, data$b6_crunchy, data$b7_crunchy, data$b8_crunchy, data$b9_crunchy, data$bb_crunchy)
creamy <- c(data$b1_creamy, data$b2_creamy, data$b3_creamy, data$b4_creamy, data$b5_creamy, data$b6_creamy, data$b7_creamy, data$b8_creamy, data$b9_creamy, data$bb_creamy)
sweet <- c(data$b1_sweet, data$b2_sweet, data$b3_sweet, data$b4_sweet, data$b5_sweet, data$b6_sweet, data$b7_sweet, data$b8_sweet, data$b9_sweet, data$bb_sweet)
chocolaty <- c(data$b1_chocolaty, data$b2_chocolaty, data$b3_chocolaty, data$b4_chocolaty, data$b5_chocolaty, data$b6_chocolaty, data$b7_chocolaty, data$b8_chocolaty, data$b9_chocolaty, data$bb_chocolaty)
healthful <- c(data$b1_healthful, data$b2_healthful, data$b3_healthful, data$b4_healthful, data$b5_healthful, data$b6_healthful, data$b7_healthful, data$b8_healthful, data$b9_healthful, data$bb_healthful)
calorie <- c(data$b1_calorie, data$b2_calorie, data$b3_calorie, data$b4_calorie, data$b5_calorie, data$b6_calorie, data$b7_calorie, data$b8_calorie, data$b9_calorie, data$bb_calorie)
rich <- c(data$b1_rich, data$b2_rich, data$b3_rich, data$b4_rich, data$b5_rich, data$b6_rich, data$b7_rich, data$b8_rich, data$b9_rich, data$bb_rich)
addiction <- c(data$b1_addiction, data$b2_addiction, data$b3_addiction, data$b4_addiction, data$b5_addiction, data$b6_addiction, data$b7_addiction, data$b8_addiction, data$b9_addiction, data$bb_addiction)
accessible <- c(data$b1_accessible, data$b2_accessible, data$b3_accessible, data$b4_accessible, data$b5_accessible, data$b6_accessible, data$b7_accessible, data$b8_accessible, data$b9_accessible, data$bb_accessible)
handy <- c(data$b1_handy, data$b2_handy, data$b3_handy, data$b4_handy, data$b5_handy, data$b6_handy, data$b7_handy, data$b8_handy, data$b9_handy, data$bb_handy)
wrapping <- c(data$b1_wrapping, data$b2_wrapping, data$b3_wrapping, data$b4_wrapping, data$b5_wrapping, data$b6_wrapping, data$b7_wrapping, data$b8_wrapping, data$b9_wrapping, data$bb_wrapping)
image <- c(data$b1_image, data$b2_image, data$b3_image, data$b4_image, data$b5_image, data$b6_image, data$b7_image, data$b8_image, data$b9_image, data$bb_image)
commercial <- c(data$b1_commercial, data$b2_commercial, data$b3_commercial, data$b4_commercial, data$b5_commercial, data$b6_commercial, data$b7_commercial, data$b8_commercial, data$b9_commercial, data$bb_commercial)

data_cleaned <- data.frame(person, choc_bar, crunchy, creamy, sweet, chocolaty, healthful, calorie, rich, addiction, accessible, handy, wrapping, image, commercial)
str(data_cleaned)

data_cleaned[order(data_cleaned$person),]
data_aggregated <- aggregate(data_cleaned[,-c(1,2)], by=list(data_cleaned$choc_bar),mean, na.rm=TRUE)

data_aggregated

distance_df <- dist(data_aggregated[,-c(1)])
distance_df

fit <- cmdscale(distance_df)
fit

x <- fit[,1]
y <- fit[,2]

data_aggregated$x <- x
data_aggregated$y <- y

profit <- lm(cbind(crunchy, creamy, sweet, chocolaty, healthful, calorie,
                   rich, addiction, accessible, handy, wrapping, image, commercial) ~ -1 + x + y, data = data_aggregated)

coef(profit)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS for Chocolate Bars", pch = 19, ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5))
text(x, y, labels = data_aggregated$Group.1, cex = 0.75, pos = 4) 
abline(h = 0, v = 0, col = "grey")
arrows(x0 = c(0, 0, 0), y0 = c(0, 0, 0),
       x1 = coef(profit)[1, ]*3, y1 = coef(profit)[2, ]*3, col = 2, lwd = 1)
text(t(coef(profit)*3.2), colnames(coef(profit)*3.2), cex=0.7, col = 1, pos = 4)


