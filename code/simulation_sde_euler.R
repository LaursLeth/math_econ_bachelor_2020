
#### Simulating SDE: Euler's method vs 'sde'-package

### Euler's method

# Parameters
S <-  100        # Starting value of process
mu <- 0.05    # drift
sigma <- 0.2  # volatility

# Time grid 
T <- 1        # End time
n <- 1000    # Number of 
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



### Simulate a Geometric Brownian Motion using the package 'sde'
library(sde)
S0 <- 100 # Startin value
set.seed(seed_number)
x <- GBM(x=S0, r=mu, sigma=sigma, N=n)

# Combine and plot simulated data
df_sde <- data.frame(index = 1:length(S_vec), sde=S_vec, gbm=x)

library(ggplot2)
library(reshape2)
df_sde_melt <- melt(df_sde, id=c('index'))

ggplot(data=df_sde_melt, aes(x=index, y=value, colour=variable)) + geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Simulating SDE: Euler's Method vs 'sde'-package")+
  xlab("Time") +
  ylab("Value") +
  scale_color_manual("Method", labels = c("Euler's method", "package"), values = c("red", "blue")) +
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))


