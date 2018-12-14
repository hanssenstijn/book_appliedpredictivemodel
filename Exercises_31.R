rm(list = ls())

# load library
install.packages("mlbench")
library(mlbench)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(corrplot)
library(e1071)

# load data
data("Glass")

# check data structure
str(Glass)
summary(Glass)

# Use visualizations to understand distributions and look at relationships
s1 <- ggplot(data=Glass, aes(x=RI)) +
    geom_histogram()
s2 <- ggplot(data=Glass, aes(x=Na)) +
    geom_histogram()
s3 <- ggplot(data=Glass, aes(x=Mg)) +
    geom_histogram()
s4 <- ggplot(data=Glass, aes(x=Al)) +
    geom_histogram()
s5 <- ggplot(data=Glass, aes(x=Si)) +
    geom_histogram()
s6 <- ggplot(data=Glass, aes(x=K)) +
    geom_histogram()
s7 <- ggplot(data=Glass, aes(x=Ca)) +
    geom_histogram()
s8 <- ggplot(data=Glass, aes(x=Ba)) +
    geom_histogram()
s9 <- ggplot(data=Glass, aes(x=Fe)) +
    geom_histogram()
s10 <- ggplot(data=Glass, aes(x=as.factor(Type))) +
    geom_bar()

grid.arrange(s1, s2, s3, s4 ,s5,s6,s7,s8,s9,s10 ,nrow = 5)

# correlations predictors
all_numVar <- Glass[, -10]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
corrplot(cor_numVar,method = "number",type="upper")

# Skewness
skewness(all_numVar$RI)
skewValues <- apply(all_numVar, 2, skewness)
skewValues
