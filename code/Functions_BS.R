

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




