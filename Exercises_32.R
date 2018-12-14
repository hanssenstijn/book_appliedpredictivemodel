rm(list = ls())

# load libraries
library(mlbench)
library(mice)
library(VIM)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
#load data
data("Soybean")

# check data
dim(Soybean)
str(Soybean)

# distributions
s1 <- ggplot(data=Soybean, aes(x=date)) +
    geom_bar()
s2 <- ggplot(data=Soybean, aes(x=plant.stand)) +
    geom_bar()
s3 <- ggplot(data=Soybean, aes(x=precip)) +
    geom_bar()
s4 <- ggplot(data=Soybean, aes(x=temp)) +
    geom_bar()
s5 <- ggplot(data=Soybean, aes(x=hail)) +
    geom_bar()
s6 <- ggplot(data=Soybean, aes(x=crop.hist)) +
    geom_bar()
s7 <- ggplot(data=Soybean, aes(x=area.dam)) +
    geom_bar()
s8 <- ggplot(data=Soybean, aes(x=sever)) +
    geom_bar()
s9 <- ggplot(data=Soybean, aes(x=seed.tmt)) +
    geom_bar()
s10 <- ggplot(data=Soybean, aes(x=germ)) +
    geom_bar()

grid.arrange(s1, s2, s3, s4 ,s5,s6,s7,s8,s9,s10 ,nrow = 5)

# Check missing value patterns
na.cols <- which(colSums(is.na(Soybean)) > 0)
sort(colSums(sapply(Soybean[na.cols], is.na)), decreasing = TRUE)

table(Soybean$Class,is.na(Soybean$hail))
table(Soybean$Class,is.na(Soybean$sever))
table(Soybean$Class,is.na(Soybean$seed.tmt))
table(Soybean$Class,is.na(Soybean$lodging))

injury = Soybean$Class == "2-4-d-injury"
injury
test = Soybean[injury,]
head(test)

# get percentage missing per class
hasMissing <- unlist(lapply(Soybean, function(x) any(is.na(x))))
hasMissing <- names(hasMissing)[hasMissing]
byPredByClass <- apply(Soybean[, hasMissing], 2, 
                       function(x, y) {
                           tab <- table(is.na(x), y)
                           tab[2,]/apply(tab, 2, sum)
                       },
                       y = Soybean$Class)
byPredByClass <- byPredByClass[apply(byPredByClass, 1, sum) > 0,]
byPredByClass <- byPredByClass[, apply(byPredByClass, 2, sum) > 0]
t(byPredByClass)
