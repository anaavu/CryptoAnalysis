setwd("~/Desktop/Anagha/UCSB/STKO Lab/Crypto/Currency")
tradeHistory = list.files(pattern=".csv")
myfiles = lapply(tradeHistory, read.csv)

library(xts)
library(regclass)

##All-time Bitcoinity trading volume up to 2016
agg_2015=c()
##All-time Bitcoinity trading volume up to 2017
agg_2016=c()
##All-time Bitcoinity trading volume up to 2015
agg_2014=c()

## Calculates agg_2015 and agg_2016
for (i in 1:43) {
  i = myfiles[[i]]
  sum_result= 0
  ##sum2=0
  time <- as.POSIXct(i[,1])
  i[,1] <- xts(x = i$Time, order.by=time)
  ##cbind(i,xt)
  for (j in 1:nrow(i)) {
    if (i[j,1] >= 2016) break
    temp= sum(as.numeric(as.character(i[j,2:ncol(i)])), na.rm=TRUE)
    sum_result=sum_result+temp
  }
  
    ## i[1].splitstr('-')[1] = "2016"
  
  agg_2015 <- c(agg_2015, sum_result)
  
  sum_result= 0
  for (j in 1:nrow(i)) {
    if (i[j,1] >= 2017) break
    temp= sum(as.numeric(as.character(i[j,2:ncol(i)])), na.rm=TRUE)
    sum_result=sum_result+temp
  }
  agg_2016 <- c(agg_2016, sum_result)
  
  sum_result=0
  for (j in 1:nrow(i)) {
    if (i[j,1] >= 2015) break
    temp= sum(as.numeric(as.character(i[j,2:ncol(i)])), na.rm=TRUE)
    sum_result=sum_result+temp
  }
  agg_2014 <- c(agg_2014, sum_result)
  
}
format(agg_2015, scientific=FALSE)
format(agg_2016, scientific=FALSE)
format(agg_2014, scientific=FALSE)
print(c(agg_2015[41], agg_2016[41]))

##Bring in other data
crypto_data = read.csv('../Crypto.csv')
country_data = read.csv('../data_theglobaleconomy.csv')

colnames(crypto_data)[4] <- "tradeVol2015"
colnames(crypto_data)[5] <- "tradeVol2016"

new_data = merge(crypto_data,country_data, by="Country")
new_data_2015 = subset(new_data, Year=='2015')
new_data_2016 = subset(new_data, Year=='2016')
##########################################
# new_data_2015[2,2] <- NA
# new_data_2015[103,2] <- NA
# new_data_2015[66,2] <- NA
# for (i in 1:106) {
#   if (is.na(new_data_2015[i,2]))
#     {new_data_2015[i,33] <- NA
#   } else if (as.character(new_data_2015[i,2]) == "Illegal")
#     {new_data_2015[i,33] <- "Illegal"
#  } else {
#    new_data_2015[i,33] <- "Legal"
#  }
# }
# colnames(new_data_2015)[33] <- "Legality2"
# new_data_2015$Legality2 = factor(new_data_2015$Legality2)
# ###########################################

##Regression
linearMod <- lm(X2016.Trade.Vol ~ Political.stability+Legality2+Inflation.y+Trade.balance.percent.of.GDP+Bond.Yield.Volatility+Monetary.freedom+Investment.freedom+Financial.freedom+Real.interest.rate+Happiness.index, data=new_data_2015)
summary(linearMod)


## remove incomplete cases
dat <- na.omit(new_data_2015)
## extract factor columns and drop redundant levels
fctr <- lapply(dat[sapply(dat, is.factor)], droplevels)
## count levels
sapply(fctr, nlevels)

