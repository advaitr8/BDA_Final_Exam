library(arm)
library(rstan)
library(beepr)
library(ggthemes)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/Julian Bautista/Documents/School Stuff/Semesters/4 Fall 2016/Applied Statistics III/Final Exam/BDA_Final_Exam")


#number 2 HMC
source("2_HMC.R")

#number 3 simulation
theta <- runif(1,0,1)
sig <- 10
y <- rnorm(1,theta,sig)

#number 4
source("4_data.R") #data manipulation
stanc("logistic.stan")$status
fit1 <- stan("logistic.stan",
             data = c("N","marital","state_id","party", 
                      "state_id_pred", "mar_pred"),
             iter = 1000, chains = 4)

#create objects for plotting
fitted <- extract(fit1)
obama_per <- elect$vote_Obama_pct/100
mar_gap <- marriage %>% 
           group_by(state) %>% 
           summarise(gap = mean(marital[party == 1]) - mean(marital[party == 0]))

gap <- mar_gap$gap

#pulling from stan
gap_pred <- colMeans(fitted$gap_pred)
vote_pred <- colMeans(fitted$vote_pred)
vote_pred_sd <- apply(fitted$vote_pred, 2, sd)
vote_pred_sd <- rep(sd(vote_pred),48)

invlogit(apply(fitted$alpha,2,sd) + apply(fitted$beta,2,sd))

# gap_pred <- invlogit(colMeans(fitted$alpha) + 
#                      colMeans(fitted$beta)) - 
#             invlogit(colMeans(fitted$alpha))
# vote_pred <- colMeans(fitted$vote_pred)

#balance plot for vote prediction
plot(c(1:48), obama_per - vote_pred, ylim = c(-1,1))
abline(0,0)

diff_vote <- obama_per - vote_pred

ggplot(NULL, aes(c(1:48), diff_vote)) + 
  geom_point() + geom_hline(yintercept = 0) + 
  theme_few() + ylim(-.5,.5) +
  geom_pointrange(aes(ymax = diff_vote + vote_pred_sd, ymin = diff_vote - vote_pred_sd)) 



#plot marriage gap
plot(gap, obama_per, pch = 16)
points(gap_pred, obama_per, pch = 16, col = "grey")

