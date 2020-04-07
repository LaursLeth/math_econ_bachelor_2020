

rm(list=ls())

library(ggplot2)
library(timeDate)

### Exercise 5.2

# Number of simulated paths (for computed the MC estimator)
N <- 1000

# Empty vectors for the maximum likelihood estimtaes (for each simulated path)
mu_vec <- numeric(N)
sigma_vec <- numeric(N)

# Specification of true parameters

S0 <- 100 # Starting value
mu <- 0.2 # drift
sigma <- 0.2 # Volatility

# Construct time-grid for simulating the GBM
T <- 1        # End time
m <- 1000     # Number of evaluations when simulating one path
dt <- T/(m)   # Equidistant time step


# Implement the three steps: simulate n paths, compute mle for mu and sigma for each i, take mean of estimates
for (i in 1:N){
  Z <- rnorm(m, mean=0, sd=1) 
  X <- cumsum((mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
  S <- c(S0,S0*exp(X))
  
  theta1 <- 1/m*(log(S[m+1]/S[1]))
  theta2 <- mean((diff(log(S)) - theta1)^2)
  
  mu_vec[i] <- (theta1 + theta2/2)*m
  sigma_vec[i] <- sqrt(theta2*m)
}

# Monte Carlo estimates
mean(mu_vec); mean(sigma_vec)


# Mean of errors (discus?)
mean(mu_vec - mu); mean(sigma_vec - sigma)


### Exercise 5.5

# Specification of model parameters
S0 <- 100 # Starting value
sigma <- 0.2 # Volatility

# Specify market parameters
K <- 110 # pay-off if S_T > K
r <- 0.05 # Short rate of interest
T <- 2 # let's try a new maturity

## Risk-neutral valuation (at time 0)

# constant 'inside' Q-probability
d <- (log(K/S0) - (r-1/2 * sigma^2)*T)/(sigma*sqrt(T))

# 'in-the-money' Q-probability
p <- 1- pnorm(d); p

# Risk-neutral price at time-0:
price_rv <- exp(-r*T)*K*p


# Let's do a sanity check with Monte Carlo simulation
N <- 10000     # Numer of simulations (simulated payoffs)
m <- 1000     # Number of evaluations for each simulated path
dt <- T/(m)   # Equidistant time step for each simulated path

payoff_vec <- numeric(N) # Vector to save simulated payoffs

for (i in 1:N){
  # Simulate under Q-measure (Q-dynamics)
  Z <- rnorm(m, mean=0, sd=1) 
  X <- cumsum((r-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
  S <- c(S0,S0*exp(X))
  
  # Compute simulated payoff at time T
  payoff_vec[i] <- K*(S[m+1]>K) # payoff - NOT discounted value. (S >K) is Boolian (True/False ~ 1/0)
  
}

# Discount average and get Monte Carlo price
price_mc <- exp(-r*T)*mean(payoff_vec)

price_rv; price_mc