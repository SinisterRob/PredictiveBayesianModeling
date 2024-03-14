#loading in libraries
library(data.table)
library(tidyverse)
library(knitr)
library(plotly)
library(mgcv)
library(showtext)
library(flextable)
library(GGally)
library(mice)
showtext_auto()

#data
ship <- data.table::fread("../data/Spaceship_Titanic/train.csv")

#look at overall stats
ship[,.N]
ship[,.N,by = Transported]
ship[,.N,by = Destination]

#seems like Cabin has a certain format - let's take a look
ship[,length(PassengerId),Cabin]
mem_cabin <- ship[,length(PassengerId),keyby = .(Cabin)]
mem_cabin[order(-V1)]

#Can we assume each cabin contains a single family?
#cabin = deck/num/side, each one could have unique predictive implications
ship <- separate(ship, Cabin, into = c("deck", "num", "side"), sep = "/")

#draw some plots

# Histogram of Age
ggplot(ship, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Histogram of Age")

# Scatterplot matrix
ggpairs(ship[,.(Age,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck,Transported)])

# Seems like none of the predictors are particularly correlated with one another
#Let's build a linear regression model with all the predictors



