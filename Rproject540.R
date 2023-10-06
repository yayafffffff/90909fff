SP500Data = read.csv("S&PReturnsNew.csv")
close = SP500Data$Adj.Close..
returns <- c(NA, diff(close) / lag(close))
SP500Data$Returns <- returns[2:253]
SP500Data$Returns
SPPrice <- cumsum(SP500Data$Returns)
plot(SPPrice, type ='l')
close

NASDAQData = read.csv("^IXIC.csv")
close2 = NASDAQData$Adj.Close
returns2 <- c(NA, diff(close2) / lag(close2))
NASDAQData$Returns <- returns2[2:253]
NP <- cumsum(NASDAQData$Returns)
plot(NP,type='l')
NASDAQData$Returns

meanSP500 = mean(SP500Data$Returns)
medianSP500 = median(SP500Data$Returns)
quantile25SP500 = quantile(SP500Data$Returns,.25)
quantile75SP500 = quantile(SP500Data$Returns, .75)
stDevSP500 = sd(SP500Data$Returns)

# Probability distribution of returns SP500
hist(SP500Data$Returns, breaks = 30, col = "white", border = "black", xlab = "Returns", ylab = "Density",ylim = c(0, 45))
lines(density(SP500Data$Returns), col = "#FF6666", lwd = 2)

# Cumulative distribution function of returns SP500
plot(ecdf(SP500Data$Returns), col = "#FF6666", lwd = 2, main = "Cumulative Distribution Function of Returns (SP500)", xlab = "Returns", ylab = "Cumulative Probability")

# Probability distribution of returns NASDAQ
hist(NASDAQData$Returns, breaks = 30, col = "white", border = "black", xlab = "Returns", ylab = "Density")
lines(density(NASDAQData$Returns), col = "#FF6666", lwd = 2)

# Cumulative distribution function of returns NASDAQ
plot(ecdf(NASDAQData$Returns), col = "#FF6666", lwd = 2, main = "Cumulative Distribution Function of Returns (NASDAQ)", xlab = "Returns", ylab = "Cumulative Probability")

corCoefficient = cor(SP500Data$Returns, NASDAQData$Returns)

#Compute the conditional probability that the daily return on the Nas-
#daq is positive given that the return on the S&P is negative.

# Subset the data to include only negative returns for SP500
SP500_neg <- SP500Data$Returns[SP500Data$Returns < 0]


# Subset the data to include only positive returns for NASDAQ
NASDAQ_pos <- NASDAQData$Returns[NASDAQData$Returns > 0]

qj <- 1
##index <- 0
for ( i in 1:length(NASDAQData$Returns))
{
  if ((NASDAQData$Returns[i] > 0)&(SP500Data$Returns[i] < 0)) 
  {
    qj[i]=1
    index[i]=i
    
  } 
  else 
  {
    qj[i]=0
  }
}
##index[!is.na(index)]
##NASDAQData$Returns[65]
##SP500Data$Returns[65]
conditionalProbability <- sum(qj)/length(SP500_neg)
conditionalProbability


