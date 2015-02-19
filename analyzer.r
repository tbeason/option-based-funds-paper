fund.classes <- read.csv("~/GitHub/option-based-funds-paper/fund classes.csv", stringsAsFactors=FALSE)
names(fund.classes) <- c("lowVol","lowVolBench","miscBench","threeFundPort","enhancedRet","assetAlloc")
allFunds <- read.csv("~/GitHub/option-based-funds-paper/allfunds.csv", stringsAsFactors=FALSE,header=FALSE)

require(quantmod)
require(plyr)

getSymbols(allFunds[,1], auto.assign=TRUE)

           
for(i in 1:length(allFunds[,1]))
{
  vname <- allFunds[i,1]
  if(substr(vname,1,1)=='^')
  {
    vname <- substr(vname,2,nchar(vname))
  }
  adjName <- paste(vname, "$",vname,".Adjusted",sep="")
  price <- eval(parse(text=adjName))
  if(i == 1)
  {
    allPrices <- price
  }
  if(i > 1)
  {
    allPrices <- na.omit(merge(allPrices,price))
  }
}

tfp <- (1000/as.numeric(allPrices$VBMFX.Adjusted[1]))*allPrices$VBMFX.Adjusted+
  (1000/as.numeric(allPrices$VTSMX.Adjusted[1]))*allPrices$VTSMX.Adjusted+
  (1000/as.numeric(allPrices$VGTSX.Adjusted[1]))*allPrices$VGTSX.Adjusted
names(tfp) <- "tfp"
allPrices <- na.omit(merge(allPrices,tfp))

calcRet <- function(pr)
{
  dailyReturn(pr,leading=FALSE)
}
allReturns <- "[<-"(allPrices, , vapply(allPrices, calcRet, FUN.VALUE = numeric(nrow(allPrices))))
maxDrawDown <- adply(allReturns,2,min,na.rm=TRUE)
highestReturn <- adply(allReturns,2,max,na.rm=TRUE)
meanReturn <- adply(allReturns,2,mean,na.rm=TRUE)
stdReturn <- adply(allReturns,2,sd,na.rm=TRUE)
summaryTable <- cbind(meanReturn,stdReturn[,2],maxDrawDown[,2],highestReturn[,2])
names(summaryTable) <- c("name","mean","sd","low","high")

View(summaryTable)


# Determine number of clusters
wss <- (nrow(t(na.omit(allReturns)))-1)*sum(apply(t(na.omit(allReturns)),2,var))
for (i in 2:15) wss[i] <- sum(kmeans(t(na.omit(allReturns)), 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
fit <- kmeans(t(na.omit(allReturns)), 3) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)