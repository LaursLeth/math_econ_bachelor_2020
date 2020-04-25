

##### Functions in Black and Scholes #####


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

# Implied volatility in Black-Scholes model

V.BS <- function(S,K,r,q,sigma,tau,option="Call"){
  valuehigh <- 100*S
  valuelow <- 0
  diff <- 10
  while (diff > 0.000001){
    valuemid <- 0.5*(valuehigh + valuelow)
    S.hat <- C(valuemid,K,r,q,sigma,tau,option="Call")
    diff <- abs(S-S.hat)
    if (S.hat > S){
      valuehigh <- valuemid
    }else{
      valuelow <- valuemid	
    }
  }
  valuemid
}


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

