library(plyr)
library(readstata13)
library(rstan)
library(arm)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#####################################################
#                                                   #
#                   Question Two                    #
#                                                   #
#####################################################

#Consider a model of n independent observations, yi ∼ Gamma(a, bxi), for i = 1, . . . , n, with a normal prior on log(a) with mean log(5) and standard deviation log(2), and a normal prior on log(b) with mean log(0.1) and standard deviation log(10). (As always, when we say log, we mean log, not log10.)

#The transformed distribution on a is lognormal(log(5), log(2))
set.seed(56)
N <- 10^3
log.a <- rnorm(N, log(5), log(2))

par(mfrow = c(1, 2),
    mar = c(3, 1, 1, 1),
    mgp = c(2, 0.7, 0),
    cex = 0.8)
hist(log.a,
     breaks =25,
     freq = FALSE,
     yaxt = 'n',
     main = "log(a)",
     xlab = " ")
lines(density(log.a),
      col = "red",
      lwd = 2)
text(3, .5,
     col = "red",
     labels = "Normal Density")

hist(exp(log.a),
     breaks = 25,
     freq = FALSE,
     yaxt = 'n',
     main = "exp(log(a))",
     xlab = " ")
lines(density(rlnorm(N, log(5), log(2))),
      col = "red",
      lwd = 2)
text(30, 0.01,
     col = "red",
     labels = "Log Normal Density")

#(a) Write the log posterior distribution (not forgetting to account for transformations) and write the analytic gradients.

#(b) Write a program in R (not using Stan) to fit the model using HMC and check that the chains have approximately converged.

log_p_th <- function (param, y, x){
    log_prior <- sum(dlnorm(param[1], 
                            meanlog = log(5),
                            sdlog = log(2),
                            log = TRUE),
                     dlnorm(param[2],
                            meanlog = log(0.1),
                            sdlog = log(10),
                            log = TRUE))
    log_likelihood <- sum(dgamma(y, 
                                 shape = param[1], 
                                 rate = param[2]*x, 
                                 log = TRUE))
    return (log_prior + log_likelihood)
}

gradient_th <- function (param, y, x){
    d_alpha <- sum(log(param[2]*x)
                - digamma(param[1]) 
                + log(y)) 
               - 1/param[1] - 2.08*param[1] + 3.35
    d_beta <- sum(param[1]/param[2] -x*y) - (1/param[2])
              -0.189*param[2] + 0.435
    return (c (d_alpha, d_beta))
}

gradient_th_numerical <- function (param, y, x){
  d <- length(param)
  e <- .0001
  diff <- rep(NA, d)
  for (k in 1:d){
    param_hi <- param
    param_lo <- param
    param_hi[k] <- param[k] + e
    param_lo[k] <- param[k] - e
    diff[k]<-(log_p_th(param_hi, y, x) - log_p_th(param_lo, y ,x))/(2*e)
  }
  return (diff)
}

hmc_iteration <- function(param, y, x, epsilon, L, M) {
  M_inv <- 1/M
  d <- length (param)
  phi <- rnorm(d, 0, sqrt(M))
  param_old <- param
  log_p_old <- log_p_th(param, y, x) - 0.5*sum(M_inv*phi^2)
  phi <- phi + 0.5*epsilon*gradient_th_numerical(param, y, x)
  for (l in 1:L){
    param <- param + epsilon*M_inv*phi
    phi <- phi + (if (l==L) 0.5 else 1)*epsilon*gradient_th_numerical(param, y, x)
  }
  phi <- -phi
  log_p_star <- log_p_th(param, y, x) - 0.5*sum(M_inv*phi^2)
  r <- exp(log_p_star - log_p_old)
  if (is.nan(r)) r <- 0
  p_jump <- min(r,1)
  param_new <- if (runif(1) < p_jump) param else param_old
  return (list (param=param_new, p_jump=p_jump))
}

hmc_run <- function (starting_values, iter, epsilon_0, L_0, M) {
  chains <- nrow (starting_values)
  d <- ncol (starting_values)
  sims <- array (NA, c(iter, chains, d),
                 dimnames=list(NULL, NULL, colnames(starting_values)))
  warmup <- 0.5*iter
  p_jump <- array (NA, c(iter, chains))
  for (j in 1:chains){
    param <- starting_values[j,]
    for (t in 1:iter){
      epsilon <- runif (1, 0, 2*epsilon_0)
      L <- ceiling (2*L_0*runif(1))
      temp <- hmc_iteration(param, y, x, epsilon, L, M)
      p_jump[t,j] <- temp$p_jump
      sims[t,j,] <- temp$param
      param <- temp$param
    } }
  monitor (sims, warmup)
  cat ("Avg acceptance probs:",
       fround(colMeans(p_jump[(warmup+1):iter,]),2),"\n")
  return (list (sims=sims, p_jump=p_jump))
}

parameter_names <- c("alpha", "beta")
d <- length (parameter_names)
chains <- 4

mass_vector <- rep (10, d)

starts <- array (NA, c(chains,d),
                 dimnames=list(NULL,parameter_names))
for (j in 1:chains){
  starts[j,] <- runif (d,0,1)
  starts[j,2] <- runif (1,0,1)
}

M1 <- hmc_run(starting_values=starts, 
               iter = 1000,
               epsilon_0 = .08, 
               L_0 = 14, 
               M = mass_vector)


#(c) Simulate fake data with n = 100, the log xi’s drawn independently from N(0,1), a = 2, and b = 0.3. Apply your program to fit the model to your simulated data.
N <- 100
x <- exp(rnorm(N, 0, 1))
y <- rgamma(N, 2, 0.3*x)

#####################################################
#                                                   #
#                   Question Three                  #
#                                                   #
#####################################################

#Suppose a measurement y is recorded with a N(θ,σ2) sampling distribution, with σ known exactly and θ known to lie in the interval [0,1]. Consider two point estimates of θ: (1) the maximum likelihood estimate, restricted to the range [0, 1], and (2) the posterior mean based on the assumption of a uniform prior distribution on θ.

#(a) Write the likelihood function and the posterior distribution.

#(b) Demonstrate using simulation that if σ is large enough, estimate (1) has a higher mean squared error than (2) for any value of θ in [0, 1]. If we define θ0 as the true value of the parameter and θˆmle and θˆBayes as the two estimates, then the relevant mean squared errors are E((θˆmle − θ0)2|θ0) and E((θˆBayes − θ0)2|θ0), where the expectations average over p(y|θ0)

#Generate fake data:
N <- 10
theta <- 0.5
sigma <- 4

set.seed(56)
y <- rnorm(N, theta, sigma)

#Simulate MSE of MLE
sm <- lm(y ~ 1)

#Simulate in Stan
fit3 <- stan("/Users/alexpavlakis/Desktop/R/bda_final_3.stan",
             data = list("N", "y", "sigma"),
             chains = 3, iter = 1000)

fit3

#(c) Mathematically prove the result in (b).

#####################################################
#                                                   #
#                   Question Four                   #
#                                                   #
#####################################################

#The file at http://www.stat.columbia.edu/~gelman/bda.course/pew_research_center_ june_elect_wknd_data.dta has data from Pew Research Center polls taken during the 2008 election campaign. (If you don’t remember how to read a .dta file in R, Google it.) Use hierarchical modeling to estimate how the marriage gap (the difference between Obama’s vote share among married people, compared to all other people) varied by state (excluding Alaska and Hawaii). To get vote intention, use the variable heat2 where it is available and heat4 where it is available. Consider only respondents who expressed an intention or leaning to vote for the Democratic or Republican candidate. Use a hierarchical logistic regression predicting vote intention given state and an indicator for being married. Fit the model in Stan and check that the chains have mixed.

#pulling the data from web
pew <- read.dta13('http://www.stat.columbia.edu/~gelman/bda.course/pew_research_center_june_elect_wknd_data.dta')
elect <- read.csv("http://www.stat.columbia.edu/~gelman/bda.course/2008ElectionResult.csv")

#combining party affiliation from heat2 and heat 4 with corresponding marital

marriage <- data.frame(
  state = pew$state,
  marital = pew$marital,
  party = ifelse(is.na(pew$heat2) == FALSE, 
                 as.character(pew$heat2), 
                 as.character(pew$heat4))
)


#drop third parties
marriage <- marriage[as.character(marriage$party) == "dem/lean dem"
                     | as.character(marriage$party) == "rep/lean rep",]

#drop hawaii and  alaska
marriage <- marriage[marriage$state != "hawaii" 
                    & marriage$state != "alaska"
                    & marriage$state != "washington dc",]

elect <- elect[elect$state != "Hawaii" 
               & elect$state != "Alaska"
               & elect$state != "District of Columbia",]

#Get rid of NAs in marriage and dem
marriage <- marriage[is.na(marriage$marital) == FALSE,]
marriage <- marriage[is.na(marriage$party) == FALSE,]

#0 if republican, 1 if democrat; 0 if not married, 1 if married
marriage$dem <- ifelse(marriage$party == "rep/lean rep", 0, 1)
marriage$mar <- ifelse(marriage$marital == "married", 1, 0)

#remove dropped state factors then generate state id
marriage$state <- factor(marriage$state)
marriage$id <- as.numeric(marriage$state)

#creating variables
state_id <- marriage$id
dem <- marriage$dem
marital <- marriage$mar
state <- levels(marriage$state)
N <- length(marital)

#Get proportion married by state
mar.state <- ddply(marriage, 
                   .(state), summarise,
                   mar = mean(mar),
                   obama.vote = mean(dem))

mar.state$id <- c(1:48)

mar_pred <- mar.state$mar
state_id_pred <- mar.state$id

#Stan model for party id by marital status
fit4 <- stan("/Users/alexpavlakis/Desktop/R/bda_final_4.stan",
             data = list("N", "state_id", "party", "marital",
                         "mar_pred", "state_id_pred"),
             chain = 4, iter = 2000)

fit4

#(a) Write the posterior distribution unambiguously using clear notation.

#(b) Plot your inference for the predicted vote share by state, along with the raw data, plotting vs. Obama’s actual vote share in 2008 (data available at http://www.stat. columbia.edu/~gelman/bda.course/2008ElectionResult.csv).

ext.fit4 <- extract(fit4)

par(mfrow = c(1, 2),
    mar = c(5, 4, 3, 2))
plot(colMeans(ext.fit4$vote_pred), 
     elect$vote_Obama_pct/100,
     pch = 16,
     cex = 0.8,
     xlim = c(0.3, 0.75),
     ylim = c(0.3, 0.75))
abline(0, 1)
points(mar.state$obama.vote,
       elect$vote_Obama_pct/100,
       pch = 16, 
       col = "skyblue",
       cex = 0.8)

#(c) Plot your inference for the marriage gap, along with the raw marriage gaps from the data, plotting vs. Obama’s vote share in 2008.
mar.gap <- ddply(marriage, .(state), summarise,
                 gap = mean(mar[dem == 1]) - mean(mar[dem == 0]))

mar.gap.pred <- invlogit(colMeans(ext.fit4$alpha) 
                         + colMeans(ext.fit4$beta)) 
                - invlogit(colMeans(ext.fit4$alpha)) 

plot(mar.gap.pred,
     elect$vote_Obama_pct/100,
     pch = 16,
     cex = 0.8,
     xlim = c(-0.4, 0.4))
points(mar.gap$gap,
       elect$vote_Obama_pct/100,
       pch = 16,
       cex = 0.8,
       col = "skyblue")

#(d) Use posterior predictive checking to assess the fit of your model.

#(e) Fit a simpler model in which the marriage gap is the same (on the logit scale) among all states. Repeat steps (a)–(c) above.

#Stan model for party id by marital status
fit4.2 <- stan("/Users/alexpavlakis/Desktop/R/bda_final_4_2.stan",
             data = list("N", "state_id", "party", "marital",
                         "mar_pred", "state_id_pred"),
             chain = 4, iter = 1000)

fit4.2

ext.fit4.2 <- extract(fit4.2)

par(mfrow = c(1, 1),
    mar = c(5, 4, 3, 2))
plot(colMeans(ext.fit4.2$vote_pred), 
     elect$vote_Obama_pct/100,
     pch = 16,
     cex = 0.8,
     xlim = c(0.3, 0.75),
     ylim = c(0.3, 0.75))
abline(0, 1)
points(mar.state$obama.vote,
       elect$vote_Obama_pct/100,
       pch = 16, 
       col = "skyblue",
       cex = 0.8)

mar.gap.pred2 <- invlogit(mean(ext.fit4.2$alpha) 
                          + mean(ext.fit4.2$beta)) 
                 - invlogit(mean(ext.fit4.2$alpha)) 

plot(rep(mar.gap.pred2, 49),
     elect$vote_Obama_pct/100,
     pch = 16,
     cex = 0.8,
     xlim = c(-0.4, 0.4))
points(mar.gap$gap,
       elect$vote_Obama_pct/100,
       pch = 16,
       cex = 0.8,
       col = "skyblue")


#(f) Discuss your results in (e) using the concept of partial pooling.
