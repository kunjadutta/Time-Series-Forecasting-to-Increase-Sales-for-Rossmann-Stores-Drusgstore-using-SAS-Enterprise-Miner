###############################
# OPIM5671 Final Project
# Rossmann Exploratory Analysis
# Team 8 

# This is an exploratory analysis of the Rossmann Store Sales data.

install.packages("data.table")
install.packages("ggplot2")
library(data.table)
library(ggplot2)

# use fread function in data.table pakage to read three files 
test = fread("P:/OPIM5671/Project/test.csv")
train = fread("P:/OPIM5671/Project/train.csv")
store = fread("P:/OPIM5671/Project/store.csv")

# take a look at datasets information 
str(train)
str(test)
str(store)
summary(train)
summary(test)
summary(store)

# Then take a look at the columns that are unique to the train set
# histogram of Sales 
hist(train$Sales, 100)
# get histogram of mean sales per store when store was not closed 
x1 = aggregate(train[Sales != 0,]$Sales, list(train[Sales != 0,]$Store), mean)
hist(x1$x, 100, main = "Mean sales per store when store was not closed")

# histogram of Customers  
hist(train$Customers, 100)
# get histogram of mean sales per store when store was not closed
x2 = aggregate(train[Sales != 0]$Customers, list(train[Sales != 0]$Store), mean)
hist(x2$x, 100, main = "Mean customers per store when store was not closed")

# take a look at relationship btw Customers and Sales 
ggplot(train[train$Sales != 0 & train$Customers != 0,],
       aes(x = log(train$Customers), y = log(train$Sales))) + 
  geom_point(alpha = 0.2) + geom_smooth()

# explore effect of promos on sales and customers 
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Sales)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Customers)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

# Explore the effect of factor DayofWeek on Sales 
ggplot(train[Sales != 0],
       aes(x = factor(DayOfWeek), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

# Analysis and visualization on store and assortment type
table1 = table(store[,store$StoreType])
barplot(table1,col = "steelblue",main = "Count for Store Type", xlab = "Store Type", ylab = "Count")
table2 = table(store[,store$Assortment])
barplot(table2, col = "steelblue", main = "Count for Assortment Type", xlab = "Assortment Type",ylab = "Count")

# connection btw store type and assortment type
table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))

# boxplot on sales and types
train_store = merge(train, store, by = "Store")
boxplot(Sales~StoreType,data = train_store,
        main="Total Sales vs.Store Type", xlab="Store Type", ylab="Total Sales")
boxplot(Sales~Assortment,data = train_store,
        main="Total Sales vs.Assortment Type", xlab="Assortment Type", ylab="Total Sales")

ggplot(data = train_store, 
       aes(x = StoreType, y = Sales)) + 
         geom_jitter(alpha = 0.1) +
         geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) 
ggplot(data = train_store, 
       aes(x = Assortment, y = Sales)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

# Explore trends of sales based on different store and assortment types 
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2)
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
  geom_smooth(size = 2)

# Explore relationship btw competition distance and sales 
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
  geom_point() + geom_smooth()



