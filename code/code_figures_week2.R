rm(list=ls())

library(ggplot2)
library(timeDate)



mydir <- "C:/Users/LLT/OneDrive - ATP/bachelorvejledning_2020"
setwd(mydir)
df_stock <- read.csv(file="data/sp500.csv",header=TRUE)


head(df_stock)
plot(df_stock$Close)

# Plot daily values
df_stock <- df_stock[, c('Date', 'Close')]
head(df_stock)

dummy_start <- which(df_stock$Date == '2003-12-31') + 1
df_stock <- df_stock[dummy_start:nrow(df_stock), ]


years <- as.character(2004:2019)
breakVec<- c(1)
N <- 0
for (i in 1:(length(years)-1)){
  year <- years[i]
  n <- sum(format(as.Date(as.character(df_stock$Date)),"%Y")==year)
  breakVec <- c(breakVec,breakVec[i] + n)
}


labelVec <- as.character(df_stock[breakVec,]$Date)

## Include break and label for last observation
breakVec <- c(breakVec,nrow(df_stock))
labelVec <- c(labelVec,as.character(tail(df_stock,1)[1,1]))


df_stock$dummy <- 1:nrow(df_stock)


# Compute raw returns with correct index
a <- which(0.05<abs(diff(df_stock$Close)/df_stock$Close[-nrow(df_stock)]))
a <- a+1

length(a)
b_col <- rep(1,nrow(df_stock))
b_col[a] <- 2

b_size <- rep(0.5,nrow(df_stock))
b_size[a] <- 2

df_stock$panel <- "Daily Closing Values"


p1 <- ggplot(data=df_stock,aes(x=1:nrow(df_stock),y=Close)) + geom_point(size=b_size,colour=b_col) +
  scale_x_continuous(breaks=breakVec,labels=labelVec) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Daily Values for SP500 Index")+
  xlab("Date") +
  ylab("Value") +
  facet_grid(panel~.) +
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))

p1


# Daily log-returns
df_stock_returns <- data.frame(Date = df_stock$Date[-1], Close = diff(log(df_stock$Close)))
df_stock_returns$x <- df_stock_returns$Close/sd(df_stock_returns$Close) # Standardized log-returns

breakVec_returns <- breakVec -1
breakVec_returns[1] <- 1
labelVec_returns <- as.character(df_stock_returns[breakVec_returns,]$Date)

df_stock_returns$panel <- "Daily Log-Returns"
p2 <- ggplot(data=df_stock_returns,aes(x=1:nrow(df_stock_returns),y=Close)) + geom_point(size=1) +
  scale_x_continuous(breaks=breakVec_returns,labels=labelVec_returns) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Daily Log-Returns for SP500 Index")+
  xlab("Date") +
  ylab("Value") +
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))

p2

# Histogram of log-returns


p_hist <- ggplot(data=df_stock_returns,aes(x=x)) + 
  geom_histogram(aes(y = ..density..), binwidth=0.1, position="identity",alpha=.5,color="black") +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1),
                aes(color="normal"),size=0.5)+
  scale_colour_manual("Density", values = c("red") )+
  theme(legend.position="bottom") +
  xlab("standardized price changes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Histogram of Standardized Daily Price Changes") 
p_hist

p_qq <- ggplot(df_stock_returns, aes(sample = x)) + 
  stat_qq() + 
  stat_qq_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Histogram of Standardized Daily Price Changes") 

p_qq


