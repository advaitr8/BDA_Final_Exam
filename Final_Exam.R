library(arm)
library(rstan)
library(beepr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/Julian Bautista/Documents/School Stuff/Semesters/4 Fall 2016/Applied Statistics III/Final Exam/BDA_Final_Exam")


#number 2 HMC
source("2_HMC.R")

#number 3 simulation


#number 4
source("4_data.R") #data manipulation
stanc("logistic.stan")$status
fit1 <- stan("logistic.stan",
             data = c("N","marital","state_id","party"),
             iter = 1000, chains = 3)

