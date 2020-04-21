

rm(list=ls())

library(ggplot2)
library(timeDate)
library(reshape2)


### Illustrate payoffs for call and put
# Strike(s)

K <- 100

S <- seq(0,200,length=1000)

payoff_call <- pmax(S-K, 0)
payoff_put <- pmax(K-S, 0)

df <- data.frame(S=S, Call=payoff_call, Put=payoff_put)
df_melt = melt(df, id = "S")

ggplot(data=df_melt, aes(x=S, y=value, color=variable)) + 
  geom_line(size=2) +
  xlab("S(T)") +
  ylab("Payoff at time T") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Contract Function for European call and put")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))


### Illustrate 

# Function
d1 <- function(S,K,r,q,sigma,tau){
  (log(S/K)+(r-q)*tau + 1/2*sigma^2*tau)/(sigma*sqrt(tau))
}


d2 <- function(S,K,r,q,sigma,tau){
  d1(S,K,r,q,sigma,tau) - sigma*sqrt(tau)
}


C <- function(S,K,r,q,sigma,tau,option="Call"){
  d1 <- d1(S,K,r,q,sigma,tau)
  d2 <- d2(S,K,r,q,sigma,tau)
  
  if (option=="Call"){
    result <- exp(-q*tau)*S*pnorm(d1) - K*exp(-r*tau)*pnorm(d2)
  }
  
  if (option=="Put"){
    result <-  exp(-q*tau)*S*pnorm(d1) - K*exp(-r*tau)*pnorm(d2) -exp(-q*tau)*S + K*exp(-r*tau)
  }
  result
}
C <- Vectorize(C)

# Set parameters
r <- 0.02
sigma <- 0.2
T <- 1
t <- 1/4
tau <- T -t
q <- 0 # dividender

df$Price <- C(S=S, K=K, r=r, q=0, sigma=sigma, tau=tau)

df_melt_2 <- melt(df, id = "S", measure.vars = c("Price", "Call"))

ggplot(data=df_melt_2, aes(x=S, y=value, color=variable)) + 
  geom_line(size=1) +
  xlab("S") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Contract Function for European call vs. Price")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))

C(S=110, K=K, r=r, q=0, sigma=sigma, tau=tau)

# Monte Carlo

N <- 10000     # Numer of simulations (simulated payoffs)
m <- 1000     # Number of evaluations for each simulated path
tau <- T -t 
dt <- tau/(m)   # Equidistant time step for each simulated path

 # Vector to save simulated payoffs
S0 <- c(90,100,110)
call_prices <- numeric(length(S0))
for (j in 1:length(S0)){
  
  payoff_vec <- numeric(N)
  for (i in 1:N){
    # Simulate under Q-measure (Q-dynamics)
    Z <- rnorm(m, mean=0, sd=1) 
    X <- cumsum((r-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
    S <- c(S0[j],S0[j]*exp(X))
    
    # Compute simulated payoff at time T
    payoff_vec[i] <- max(S[m+1]-K, 0)
  }
  call_prices[j] <- mean(payoff_vec)
}
exp(-r*tau)* call_prices

# Discount average and get Monte Carlo price
price_mc <- exp(-r*tau)*mean(payoff_vec)
price_mc
exp(-r*tau)
