#data <- read.csv(file="Salaries.csv")
library(car)
library(corrplot)
data <- Salaries
data.df <- data.frame(data)
data.df
#nrow(data) #observations
#ncol(data) #variables
dim(data)
#professors <- nrow(subset(data, yrs.service>40 )) #rank == Professor (summary)
#professors <- nrow(data$rank[data$yrs.service > 40 & data$rank=="Professor"])
table(data.df$rank == 'Prof', data.df$yrs.service > 40) #Ergebnis: 19

salary <- nrow(subset(data, salary > 150000))
salary

mean((subset(data$salary, data.df$rank == 'Prof', data.df$yrs.service > 20))) #Ergebnis: 126772,1
#mean((subset(data$salary, data.df$yrs.service > 20)))
summary(data) #find out more about the data set
""" professors_male <- subset(data, sex=='Male')
aggregate(sex ~ rank, professors_male, NROW) #count of men by rank
professors_female <- subset(data, sex=='Female')
aggregate(sex ~ rank, professors_female, NROW) """ #count of women by rank #table (slides)

table(data$sex, data$rank)

prop.table(table(data$sex, data$rank))

hist(data$yrs.service, freq=FALSE, xlab="years of service", ylab="proportional density", main="Histogram of years of service")
lines(density(data$yrs.service), type="l", col="darkred") #red density line
boxplot(data$salary , xlab="Professors", ylab="Salary", main="Distribution of Salary") #boxplot for salary
boxplot(data$salary ~data$rank,horizontal=TRUE,
        xlab="Salary", las=1, main="Salary of professors by rank") #boxplot for salary by rank
#axis(side=2, at=c(1,2), labels=c(aggregate(salary ~ yrs.since.phd, data, NROW)))
plot(data$yrs.since.phd, data$salary, col="blue",
     main="Salary compared to years since PhD", xlab="Years since PhD", ylab="Salary") #plot salary vs. years since PhD

num_data <- data[, sapply(data, is.numeric)]
cor(num_data) #correlation cor.test()
cor.test(data$yrs.since.phd, data$salary)
cor.test(data$yrs.service, data$salary)
corrplot(corr=cor(num_data), method ="ellipse")
