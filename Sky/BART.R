#libraries
library(randomForest)
library(gbm)
library(data.table)
library(tidyverse)
library(mice)
library(pROC)
library(bartMachine)

#----------------------------------------BART
ship <- data.table::fread("data/Spaceship_Titanic/train.csv")

#splitting Cabin and imputing data
ship[, c("deck", "num", "side") := tstrsplit(Cabin, "/", fixed = TRUE)]
ship <- ship[,-c("Cabin","num")]
ship_clean <- ship[, -c("PassengerId","Name")]
imp_ship <- mice(ship_clean, m = 2, seed = 1)
imp_ship_data <- complete(imp_ship)
imp_ship_data <-
  data.table(imp_ship_data) %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)),
         Transported = factor(Transported),
         deck = factor(deck, levels = unique(deck)),
         side = factor(side, levels = unique(side)))

bart_model <- bartMachine(X = imp_ship_data[,.(HomePlanet,CryoSleep,Destination,
                                               deck,side,
                                               Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck)], 
                          y = imp_ship_data$Transported, 
                          use_missing_data = TRUE,
                          num_trees = 50, 
                          num_burn_in = 100, 
                          num_iterations_after_burn_in = 100)

predictions_train <- predict(bart_model,imp_ship_data[,.(HomePlanet,CryoSleep,Destination,
                                                         deck,side,
                                                         Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck)],
                             type = "class")
imp_ship_data$predictions <- as.numeric(predictions_train)

bart_model
roc_curve <- roc(Transported ~ predictions,imp_ship_data)
auc_value <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve for BART Model")

#----------------------------------------------------generating test predictions
ship_test <- data.table::fread("data/Spaceship_Titanic/test.csv")

#splitting Cabin and imputing data
ship_test[, c("deck", "num", "side") := tstrsplit(Cabin, "/", fixed = TRUE)]
ship_test <- ship_test[,-c("Cabin","num")]
ship_test_clean <- ship_test[, -c("PassengerId","Name")]
imp_ship_test <- mice(ship_test_clean, m = 2, seed = 1)
imp_ship_test_data <- complete(imp_ship_test)
imp_ship_test_data <-
  data.table(imp_ship_test_data) %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)))

predictions <- predict(bart_model,imp_ship_test_data[,.(HomePlanet,CryoSleep,Destination,
                                                        deck,side,
                                                        Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck)])
binary_predictions <- ifelse(predictions < 0.5, 'True', 'False')

submission <- data.frame(PassengerId = ship_test$PassengerId, Transported = binary_predictions)
write.csv(submission, "Sky/bart.csv", row.names = FALSE)