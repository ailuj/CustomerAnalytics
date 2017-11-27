#
# SWP2
# CACI 
# Abgabe 29.11.2017
#

wdir = getwd()
wdir = "/Users/Natalie/Documents/R"
setwd(wdir)

install.packages("data.table")
library("data.table")
install.packages("gplots")
library("gplots")
install.packages("corrplot")
library("corrplot")
install.packages("car")
library(car)
install.packages("lattice")
library(lattice)

salaries = read.csv("salaries.csv")
summary(salaries)
str(salaries)

# analysing the salary
salaries.dt <- data.table(salaries)
salaries.dt[, mean(salary), by = rank]
salaries.dt[, mean(salary), by = sex]

#Julia
mean.salaries <- aggregate(salary~sex+rank, salaries, mean) #mean salary by sex and rank

boxplot(formula = salary ~ sex,
        data = salaries,
        outline = TRUE,
        horizontal = TRUE,
        xlab = "Salaries in Euros",
        ylab = "Sex of the university employees",
        main = "Describing salary distribution by sex and rank",
        col = rainbow(2))

salaries.fem.dt <- data.table(subset(salaries, salaries$sex == "Female"))
salaries.fem.dt[, mean(salary), by = discipline]

#Julia
prop.table(table(salaries$sex, salaries$discipline))

#ANOVA (analysis of variance)
seg.aov.salary <- aov(salary ~ sex,
                      data = salaries) 

aov.salary <- anova(seg.aov.salary)

tuk <- TukeyHSD(seg.aov.salary)
plot(tuk)

plot(salaries$salary)
hist(salaries$salary)


#linear model and correlation plot




