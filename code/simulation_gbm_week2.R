


# Generating GBM
rm(list=ls())

library(ggplot2)
library(timeDate)

# Method 1

# Parameters
S <-  100        # Starting value of process
mu <- 0.05    # drift
sigma <- 0.2  # volatility

# Time grid 
T <- 1        # End time
n <- 1000         # Number of evaluations
dt <- T/(n)   # Equidistant time step

S_vec <- numeric(n+1)
S_vec[1] <- S

# Choose a seed number for comparison
seed_number <- 35
set.seed(seed_number)
for (i in 1:n){
  
  Z <- rnorm(1,mean=0, sd=1)        # Generate 1 value from normal distribution with mean=0 and variance=1
  dS <- mu*S*dt + sigma*S*sqrt(dt)*Z      # Compute S_i - S_{i-1} 
  
  S <- S + dS # Update S
  
  S_vec[i+1] <- S # Save S
}
S_vec


# Method 2
S <- 100
S_vec2 <- numeric(n+1)
S_vec2[1] <- S
set.seed(seed_number)
for (i in 1:n){
  Z <- rnorm(1, mean=0, sd=1) # Generate value from normal dist. with mean=0 and var=1
  X <- (mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z  # log(S_i/S_{i-1})
  S <- S*exp(X)
  S_vec2[i+1] <- S
}

# Combine and plot simulated data
df_sde <- data.frame(index = 1:length(S_vec), sde1=S_vec, sde2=S_vec2)

library(ggplot2)
library(reshape2)
df_sde_melt <- melt(df_sde, id=c('index'))

ggplot(data=df_sde_melt, aes(x=index/n, y=value, colour=variable)) + geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Simulating SDE: Euler's Method vs new method")+
  xlab("Time") +
  ylab("Value") +
  scale_color_manual("Method", labels = c("Euler's method", "new method"), values = c("red", "blue")) +
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0,vjust=1))


# Method 3: no for-loop....
S <- 100
set.seed(seed_number)
Z <- rnorm(n, mean=0, sd=1) # Generate n values from multi dim. normal dist.
X <- cumsum((mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z) # Argument should be a n-dim vector
S_vec3 <- c(S,S*exp(X))
S_vec3

# Method 4: Build-in function
library(sde)
set.seed(seed_number)
S_vec4 <- GBM(x=100, r=mu, sigma=sigma, N=n)
