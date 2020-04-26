

rm(list=ls())

# Libraries

library(zoo)
library(timeDate)
library(ggplot2)
library(reshape2)

### Working directory

myDir <- "C:/Users/llt/OneDrive - ATP/git-private/math_econ_bachelor_2020/data/"
setwd(myDir)


nCol <- 2
df <- read.csv2(file="OxfordManRealizedVolatilityIndices.csv",sep=",",header=TRUE, skipNul = TRUE,
                colClasses = c(rep("character",nCol),rep("character",20-nCol)))

df <- subset(df, Symbol=='.SPX')

colnames(df)
df[, !(c('Symbol', 'open_to_close','open_time', 'close_time','nobs', 'open_price'))]
head(df)
df2 <- subset(df, select=-c(Symbol, open_to_close, open_time, close_time, nobs, open_price))

# Compute and annualize volatility
df3 <- df2[, c(1,7,2:6,8:dim(df2)[2])]
names(df3)[1:2] <- c('date', 'price')


vol_an <- function(x){
  sqrt(x)*sqrt(252)
}

colsNum <- 2:dim(df2)[2]
df3[, colsNum] <- apply(df3[,colsNum], 2, vol_an)



# Compute daily (naive) volatility
df3$naive <- NA
n_roll <- 63 # 3 months 
df3[(n_roll+1):dim(df3)[1], c('naive')] <- rollapply(data = diff(log(df3$price)),width=n_roll,FUN=sd)*sqrt(252)

df3 <- df3[complete.cases(df3), ]


# Plot volatilities
vol_names <- names(df3)[3:dim(df3)[2]]

df_vol <- melt(df3, measure.vars = vol_names, id.vars = c('date'))
df_vol$date <- as.Date(df_vol$date)
str(df_vol)


# Vol-estimators to plot
vol_measures <- c('rv5', 'rsv', 'medrv', 'bv_ss', 'rk_parzen','naive')
df_vol2 <- df_vol[df_vol$variable %in% vol_measures, ]

size_dummy <- c(rep(0.5,nrow(df_vol2)/length(vol_measures) * (length(vol_measures)-1)), 
                rep(2, nrow(df_vol2)/length(vol_measures)))

ggplot(data=df_vol2, aes(x=date, y=value, color=variable)) + 
  geom_line(size=size_dummy) +
  xlab("Date") +
  ylab("Value") +
  ggtitle("Estimators for Volatility") +
  theme(plot.title = element_text(lineheight=0.8, face="bold",vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1))

