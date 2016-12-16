library(arm)
library(rstan)
library(beepr)
library(ggthemes)
library(gridExtra)
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
             iter = 3000, chains = 4)

#create objects for plotting
fitted <- extract(fit1)
obama_per <- elect$vote_Obama_pct/100
mar_gap <- marriage %>% 
           group_by(state) %>% 
           summarise(gap = mean(marital[party == 1]) - mean(marital[party == 0]), vote = mean(party))
obama_vote <- mar_gap$vote

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

#vote prediction plots
plotter<- rbind(
  data.frame(vote = obama_vote, Legend = rep("Data", 48),obama_per),
  data.frame(vote = vote_pred, Legend = rep("Model", 48),obama_per))

pp<-ggplot(plotter, aes(vote, obama_per, colour = Legend)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) +
  labs(x = "Obama Vote Shares", y = "Predicted Vote Shares", title = "Vote Prediction", colour = "")  + 
  theme_gdocs() + scale_colour_colorblind() +
  theme(plot.title = element_text(hjust = 0.5))

#balance plot for vote prediction
diff_vote <- obama_per - vote_pred

b_p<-ggplot(NULL, aes(c(1:48), diff_vote)) + 
  geom_point() + geom_hline(yintercept = 0) + 
  labs(x = "State Indexes", y = "Difference between Predicted and Actual Votes", title = "Vote Prediction Balance") +
  theme_gdocs() + scale_colour_colorblind() + ylim(-1,1) +
  theme(plot.title = element_text(hjust = 0.5))#+
  #geom_pointrange(aes(ymax = diff_vote + vote_pred_sd, ymin = diff_vote - vote_pred_sd)) 

grid.arrange(pp,b_p, ncol = 2)

#plot marriage gap
plotter2<- rbind(
  data.frame(gap = gap, Legend = rep("Actual Marriage Gap", 48),obama_per),
  data.frame(gap = gap_pred, Legend = rep("Predicted Marriage Gap", 48),obama_per))

ggplot(plotter2, aes(gap, obama_per, colour = Legend)) + geom_point() + 
  theme_gdocs() + scale_colour_colorblind() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "", x = "Marriage Gap", y = "Obama Vote % 2008", title = "Marriage Gap")

