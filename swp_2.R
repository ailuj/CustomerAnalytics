#data <- read.csv2("Salaries.csv")
library(car)
library(lattice)
library(corrplot)
data <- Salaries
data
data.df <- data.frame(data)
mean.salaries <- aggregate(salary~sex+rank, data, mean ) #mean salary by sex and rank
mean.salaries
#boxplot(salary~sex+rank, data, xlab = "Salary",
#        ylab = "Rank", main = "mean salary by sex and rank", horizontal=TRUE) #mean salary by sex + rank

bwplot(rank~salary | sex, data=data.df, horizontal=TRUE, xlab='salary', ylab='rank') #salary by rank and sex

#boxplot(salary~sex, data, xlab = "Salary",
#        ylab = "Rank", main = "mean salary by sex", horizontal=TRUE) #mean salary by sex

prop.table(table(data$sex, data$discipline)) #proportion of women by discipline

#ANOVA model 
fit <- aov(salary ~ sex, data)
anova(fit)
#plot(fit)
summary(fit) #p-value < 0.05 (significance level), therefore significant differences in salary between genders

confint(lm(salary~sex, data=data)) #linear regression with confidence interval

plot(data$salary)

linear.model <- lm(salary ~ sex + rank + discipline + yrs.since.phd, data)
linear.model
#corrplot(corr=cor(linear.model, use="complete.obs"), method="ellipse")

#correlation plot
