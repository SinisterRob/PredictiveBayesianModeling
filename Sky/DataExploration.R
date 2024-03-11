#loading in libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(plotly)
library(mgcv)
library(showtext)
library(flextable)
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

#draw some plots
