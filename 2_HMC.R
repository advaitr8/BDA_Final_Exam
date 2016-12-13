#number 3
theta <- seq(0.01,0.99, 0.01)
sigma <- 

y <- rnorm(theta, sigma)

#number 2

#function that gets the log posterior
log_post <- function(theta, x, y){
  J <- length(theta)
  alpha <- theta[1]
  beta <- theta[2]
  
  param_prior_a <- dnorm(alpha, log(5), log(2),log = TRUE)
  param_prior_b <- dnorm(beta, log(0.1), log(10), log = TRUE)
  
  log_prior_a <- sum(param_prior_a)
  log_prior_b <- sum(param_prior_b)
  
  log_likelihood <- sum(dgamma(y, shape = alpha, rate = beta*x, log = TRUE))
  
  return(log_prior + log_likelihood)
}

#function for gradient
gradient <- function(beta, x, y, n){
  d <- length(beta)
  e <- 0.0001
  diff <- rep(NA,d)
  for(k in 1:d){
    beta_hi <- beta
    beta_lo <- beta
    beta_hi[k] <- beta[k] +e
    beta_lo[k] <- beta[k] -e
    diff[k] <- (log_post(beta_hi,x,y,n) - log_post(beta_lo,x,y,n))/(2*e)
  }
  return(diff)
}

#function for a single iteration of HMC
hmc_iteration <- function(beta,x,y,n,epsilon,L,M){
  M_inv <- 1/M
  d <- length(beta)
  phi <- rnorm(d,0,sqrt(M))
  beta_old <- beta
  log_p_old <- log_post(beta,x,y,n) - 0.5*sum(M_inv*phi^2)
  phi <- phi + 0.5*epsilon*gradient(beta,x,y,n)
  for(l in 1:L){
    beta <- beta + epsilon*M_inv*phi
    phi <- phi + (if(l == L)0.5 else 1)*epsilon*gradient(beta,x,y,n)
  }
  phi <- -phi
  log_post_star <- log_post(beta,x,y,n) - 0.5*sum(M_inv*phi^2)
  r <- exp(log_post_star - log_p_old)
  if(is.nan(r)) r <- 0
  p_jump <- min(r,1)
  beta_new <- if(runif(1) < p_jump) beta else beta_old
  return(list(beta = beta_new, p_jump = p_jump))
}

#running HMC the appropriate number of iterations
hmc_run <- function(starting_values, iter, epsilon_0, L_0, M){
  chains <- nrow(starting_values)
  d <- ncol(starting_values)
  sims <- array(NA, c(iter,chains,d),
                dimnames = list(NULL, NULL, colnames(starting_values)))
  warmup <- 0.5*iter
  p_jump <- array(NA, c(iter,chains))
  for(j in 1:chains){
    beta <- starting_values[j,]
    for(t in 1:iter){
      epsilon <- runif(1,0,2*epsilon_0)
      L <- ceiling(2*L_0*runif(1))
      temp <- hmc_iteration(beta,x,y,n,epsilon,L,M)
      p_jump[t,j] <- temp$p_jump
      sims[t,j,] <- temp$beta
      beta <- temp$beta
    }
  }
  monitor(sims,warmup)
  cat("Avg acceptance probs:",
      fround(colMeans(p_jump[(warmup+1):iter,]),2),"\n")
  return(list(sims = sims, p_jump = p_jump))
}

#running HMC for 4 chains to check acceptance rates
parameter_names <- c (paste ("beta[",1:2,"]",sep=""))
d <- length(parameter_names)
chains <- 4
