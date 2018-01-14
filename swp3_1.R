# CACI
# SWP3
# January 13, 2018


## set environment
wdir <- "C:\\Users\\Natalie\\Dropbox\\WiSe201718\\Customer Analytics and Customer Insights\\Data"
setwd(wdir)

## install libraries
install.packages("data.table"); library(data.table)  
install.packages("corrplot"); library(corrplot)
install.packages("psych"); library(psych)

## functions
std_rank <- function(x){
  return(sum(x)/(50*7))
}

## Reading data, once table "Attribute Rating", twice "Direct Preference Rating"
bars <- read.csv("attribute_evaluation_choc_bars.csv", sep = ";", header = TRUE)
rating <- read.csv("bars_rating.csv", sep = ";", header = TRUE)

rank <- list()
rating$Person <- NULL
rank <- sort(round(sapply(rating, std_rank), digits = 2), decreasing = TRUE)


## Cleaning sample bars
bars_mean <- list()
bars_mean <- sapply(na.omit(bars), mean)

for(col in 1:length(bars)) {
  #bars[[paste(col, "missing", sep = "_")]] <- ifelse(is.na(bars[[col]]), 1, 0)
  bars[[col]] <- ifelse(is.na(bars[[col]]), bars_mean[col], bars[[col]])
}

## fa for each of the 10 chocolat products 
# here for the first chocolat product
b1_attr <- bars[,2:14]
cor(b1_attr)
#corrplot(b1_attr, method = "pie", type = "upper")
plot(eigen(cor(b1_attr))$values, type = "b", 
     main = "Plotting variances of Eigenvalues",
     xlab = "Attributes of Snickers",
     ylab = "Variance")   # use 6 factors

b1.fa <- fa(b1_attr, nfactors = 3, rotate = "variance", cut = 0.3)
fa.diagram(b1.fa)
data.frame(b1.fa$scores)  #to-do: name columns


#swp3_1.R