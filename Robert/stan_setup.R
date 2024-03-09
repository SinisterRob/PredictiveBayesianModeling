library(rstan)
library(data.table)
library(dplyr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sst_data <- fread("../data/Spaceship Titanic/sample_submission.csv")
View(SST_data)