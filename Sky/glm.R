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
library(cars)
library(caret)
showtext_auto()

#data
ship <- data.table::fread("../data/Spaceship_Titanic/train.csv")

# Scatterplot matrix
ggpairs(ship[,.(Age,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck,Transported)])


#splitting Cabin and imputing data
ship[, c("deck", "num", "side") := tstrsplit(Cabin, "/", fixed = TRUE)]
ship <- ship[,-c("Cabin")]
ship$Transported <- as.numeric(ship$Transported)

ship_transported <- ship$Transported
ship_no_transported <- ship[, -c("PassengerId","Transported","num","Name")]

imputed_ship <- mice(ship_no_transported, m = 5, printFlag = TRUE)
imputed_ship_data <- complete(imputed_ship, 1)
imputed_ship_data$Transported <- ship_transported
imputed_ship_data <-
  imputed_ship_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)), deck = factor(deck), 
         num = factor(num), side = factor(side))

#glm with all predictors

model <- glm(Transported ~ HomePlanet + CryoSleep + side + deck + Destination + VIP + Age + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck,
             data = imputed_ship_data, family = binomial(link = "logit"))

predictions <- predict(model,newdata = imputed_ship_data)


#evaluating training error
threshold <- 0.5
probabilities <- plogis(predictions)
binary_predictions <- ifelse(probabilities >= threshold, 1,0)

confusionMatrix(binary_predictions, imputed_ship_data$Transported)

imputed_ship_data$predictions <- binary_predictions
(nrow(imputed_ship_data) - nrow(imputed_ship_data[Transported != predictions]))/nrow(imputed_ship_data)

#----------------------------------------------------generating test predictions
ship_test <- data.table::fread("../data/Spaceship_Titanic/test.csv")

ship_test[, c("deck", "num", "side") := tstrsplit(Cabin, "/", fixed = TRUE)]
ship_test <- ship_test[,-c("Cabin","num")]
imputed_ship_test <- mice(ship_test, m = 1, printFlag = FALSE)
imputed_ship_test_data <- complete(imputed_ship_test, 1)
imputed_ship_test_data <-
  imputed_ship_test_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)), deck = factor(deck), 
         num = factor(num), side = factor(side))

predictions <- predict(model,newdata = imputed_ship_test_data)

threshold <- 0.5
probabilities <- plogis(predictions)
binary_predictions <- ifelse(probabilities >= threshold, 'True', 'False')
submission <- data.frame(PassengerId = imputed_ship_test_data$PassengerId, Transported = binary_predictions)
write.csv(submission, "submission.csv", row.names = FALSE)


#-------------------------------------------------using fewer predictors and not splitting
ship <- data.table::fread("data/Spaceship_Titanic/train.csv")

ship_transported <- as.numeric(ship$Transported)
ship_clean <- ship[, -c("PassengerId","Name")]
imp_ship <- mice(ship_clean, m = 5, seed = 1)
imp_ship_data <- complete(imp_ship)
imp_ship_data <-
  imp_ship_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)))

model <-glm(Transported ~ HomePlanet + CryoSleep + Age + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck,
                          data = imp_ship_data,family = binomial(link = "logit"))

# Destination and VIP weren't significant
summary(model)

#----------------------------------------------------generating test predictions
ship_test <- data.table::fread("data/Spaceship_Titanic/test.csv")

imputed_ship_test <- mice(ship_test, m = 1, printFlag = FALSE)
imputed_ship_test_data <- complete(imputed_ship_test, 1)

predictions <- predict(model,newdata = imputed_ship_test_data)

threshold <- 0.5
probabilities <- plogis(predictions)
binary_predictions <- ifelse(probabilities >= threshold, 'True', 'False')
submission <- data.frame(PassengerId = imputed_ship_test_data$PassengerId, Transported = binary_predictions)
write.csv(submission, "Sky/glm_noDest_noVIP.csv", row.names = FALSE)
