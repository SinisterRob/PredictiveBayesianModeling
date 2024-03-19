#libraries
library(data.table)
library(tidyverse)

ship <- data.table::fread("data/Spaceship_Titanic/train.csv")

ggplot(ship, aes(x = Age, y = as.numeric(Transported))) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.5, se = FALSE, color = "blue") +
  labs(title = "Relationship between Age and Transported Status",
       x = "Age",
       y = "Probability of Being Transported")
#plateaus at 40
#can we set age to stop increasing after 40?

model <- glm(Transported ~ VRDeck, data = ship, family = binomial)
VRDeck_seq <- seq(0, 10000, length.out = 100)
predictions <- predict(model, newdata = data.frame(VRDeck = VRDeck_seq), type = "response")

