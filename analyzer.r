fund.classes <- read.csv("~/GitHub/option-based-funds-paper/fund classes.csv", stringsAsFactors=FALSE)
names(fund.classes) <- c("lowVol","lowVolBench","miscBench","threeFundPort","enhancedRet","assetAlloc")
allFunds <- read.csv("~/GitHub/option-based-funds-paper/allfunds.csv", stringsAsFactors=FALSE,header=FALSE)
toDate <- function(x) as.Date(x, origin = "1899-12-30")
extraTickers<- read.zoo("~/GitHub/option-based-funds-paper/paper data.csv",header=TRUE,sep=",",FUN = toDate)
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
    allPrices <- merge(allPrices,price)
  }
}

tfp <- (1000/as.numeric(allPrices$VBMFX.Adjusted[1]))*allPrices$VBMFX.Adjusted+
  (1000/as.numeric(allPrices$VTSMX.Adjusted[1]))*allPrices$VTSMX.Adjusted+
  (1000/as.numeric(allPrices$VGTSX.Adjusted[1]))*allPrices$VGTSX.Adjusted
names(tfp) <- "tfp"
allPrices <- na.omit(merge(allPrices,tfp))
xtra <- as.xts(extraTickers)
allPrices <- na.omit(merge(allPrices,xtra))
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
summaryTable$name <- c(allFunds[,1],"TFP",names(xtra))
View(summaryTable)

# plot all funds risk return
ggplot(summaryTable) +
  geom_point(aes(x=sd,y=mean)) +
  geom_text(aes(x=sd,y=mean,label=name),size=4,hjust=0,vjust=0) +
  labs(title = "Return vs Risk - All Funds") +
  ylab("Mean Daily Return") +
  xlab("Daily Standard Deviation") 
  
  
write.csv(summaryTable,file="~/GitHub/option-based-funds-paper/table.csv")
table <- read.csv("~/GitHub/option-based-funds-paper/table.csv", stringsAsFactors=FALSE)
table2 <- table[,-1]

#calc information ratio
infr <- t(InformationRatio(allReturns,cbind(allReturns[,16:20],allReturns[,32:36]),scale=252))
sum2 <- cbind(summaryTable,infr)

lvGroup <- sum2[which(table2[,6]!=2),]

# plot all funds risk return
ggplot(lvGroup) +
  geom_point(aes(x=sd,y=mean)) +
  geom_text(aes(x=sd,y=mean,label=name),size=4,hjust=0,vjust=0) +
  labs(title = "Return vs Risk - Low Volatility Funds") +
  ylab("Mean Daily Return") +
  xlab("Daily Standard Deviation")

erGroup <- sum2[which(table2[,6]!=1),]

# plot all funds risk return
ggplot(erGroup) +
  geom_point(aes(x=sd,y=mean)) +
  geom_text(aes(x=sd,y=mean,label=name),size=4,hjust=0,vjust=0) +
  labs(title = "Return vs Risk - Enhanced Return Funds") +
  ylab("Mean Daily Return") +
  xlab("Daily Standard Deviation")

write.csv(lvGroup,file="~/GitHub/option-based-funds-paper/lvGroup.csv")
write.csv(erGroup,file="~/GitHub/option-based-funds-paper/erGroup.csv")

