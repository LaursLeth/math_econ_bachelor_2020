
rm(list=ls())

library(ggplot2)
library(timeDate)
library(reshape2)

## Functions used
d1 <- function(S,K,r,q,sigma,tau){
  (log(S/K)+(r-q)*tau + 1/2*sigma^2*tau)/(sigma*sqrt(tau))
}


d2 <- function(S,K,r,q,sigma,tau){
  d1(S,K,r,q,sigma,tau) - sigma*sqrt(tau)
}


# Black-Scholes Price function
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

# Black-Scholes Delta
BS.Delta <- function(S,K,r,q,sigma,tau,option="Call"){
  
  if (option == "Call"){
    result <-  exp(-q*tau)*pnorm(d1(S,K,r,q,sigma,tau))
  }
  
  if (option == "Put"){
    result <-  exp(-q*tau)*(pnorm(d1(S,K,r,q,sigma,tau))-1)
  }
  
  result
}

BS.Delta <- Vectorize(BS.Delta)

## Simulate S and illustrate corresponding arbitrage-free price

# Model parameters
mu <- 0.07
sigma <- 0.2

# Market parameters
q <- 0
r <- 0.02
S0 <- 100
K <- 100 # Initially at-the-money
T <- 1

# Time grid
N <- 10000
t <- seq(0,T,length=N+1) # N intervals
tau <- T -t 
dt <- 1/N   # Equidistant time step for each simulated path

# Simulate S under P
Z <- rnorm(N, mean=0, sd=1) 
X <- cumsum((mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
S <- c(S0,S0*exp(X))

call_prices <- C(S=S, K=K, r=r,q=q, sigma=sigma,tau=tau)

df <- data.frame(time=t, spot=S, call=call_prices)

df_melt <- melt(df, id = "time", measure.vars = c("spot", "call"))


ggplot(data=df_melt, aes(x=time, y=value, color=variable)) + 
  geom_line(size=1) +
  xlab("time") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Contract Function for European call vs. Price")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))


## delta hedging strategy

delta_hedge_error <- function(S,K,r,q,sigma, tau, call_price, strategy=-1,option="Call"){
  
  delta   <- BS.Delta(S=S,K=K,r=r,q=q, sigma=sigma,tau=tau, option=option)
  
  n            <- length(S[-length(S)])
  V            <- numeric(n+1); V[1] <- call_price[1] # Value process 
  Pi           <- numeric(n+1);Pi[1]  <- 0  ### Path of PL/Hedging Error
  dPi          <- numeric(n+1);dPi[1] <- 0  ### Hedging error over time interval
  
  if (length(r)==1){r<-rep(r,n)}
  if (length(q)==1){q<-rep(q,n)}
  
  for (i in 1:n){
    B       <- Pi[i] + strategy*delta[i]*S[i] - strategy*call_price[i] # Rebalance with money account
    V[i+1] <- exp(r[i]*dt)*B - strategy*delta[i]*exp(q[i]*dt)*S[i+1] # Mark-to-market for portfolio
    Pi[i+1] <- V[i+1] + strategy*call_price[i+1] # Mark-to-market for adjusted portfolio/cumulated hedging error
    dPi[i+1] <- Pi[i+1] - Pi[i] # 
  }
  output=list(pf_value= V, error_cum = Pi, error_dt = dPi) # Save output
}

res <- delta_hedge_error(S=S,K=K,r=r,q=0,sigma=sigma, tau=tau, call_price=call_prices, strategy=-1,option="Call")


## Compare hedging strategy with price process
df$portfolio <- res$pf_value

df_melt <- melt(df, id = "time", measure.vars = c("call", "portfolio"))

ggplot(data=df_melt, aes(x=time, y=value, color=variable)) + 
  geom_line(size=1) +
  xlab("time") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("European Call Price vs. Value Process for Strategy")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))

# Plot error
df_error <- data.frame(time=t, error=res$error_cum, error_dt=res$error_dt)
df_melt <- melt(df_error, id = "time", measure.vars = c("error", "error_dt"))

ggplot(data=df_melt, aes(x=time, y=value, color=variable)) + 
  geom_line(size=1) +
  xlab("time") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("European Call Price vs. Value Process for Strategy")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))

