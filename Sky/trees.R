#libraries
library(randomForest)
library(gbm)
library(data.table)
library(tidyverse)
library(mice)

#-------------------------------------------------Bagging
ship <- data.table::fread("data/Spaceship_Titanic/train.csv")

ship_clean <- ship[, -c("PassengerId","Name")]
imp_ship <- mice(ship_clean, m = 5, seed = 1)
imp_ship_data <- complete(imp_ship)
imp_ship_data <-
  imp_ship_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)))

rf_model <- randomForest(factor(Transported) ~ Destination + HomePlanet + CryoSleep + Age + RoomService
                             + FoodCourt + ShoppingMall + Spa + VRDeck,
                             data = imp_ship_data, ntree = 250)

rf_model
importance(rf_model)

#----------------------------------------------------generating test predictions
ship_test <- data.table::fread("data/Spaceship_Titanic/test.csv")

ship_test_clean <- ship_test[, -c("PassengerId","Name")]
imp_ship_test <- mice(ship_test_clean, m = 5, seed = 1)
imp_ship_test_data <- complete(imp_ship_test)
imp_ship_test_data <-
  imp_ship_test_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)))

predictions <- predict(rf_model,newdata = imp_ship_test_data)
binary_predictions <- ifelse(predictions == "TRUE", 'True', 'False')

submission <- data.frame(PassengerId = ship_test$PassengerId, Transported = binary_predictions)
write.csv(submission, "Sky/rf.csv", row.names = FALSE)

#----------------------------------------Next Steps: Look into the individual vars
#See if any variables can be transformed to be better predictors
#Rerun the random forest

