library(dygraphs)
library(plyr)
library(VPdtw)
 library(ggplot2)
 library(plotly)

# Plotting Reference and Query
data(reference)
data(query)

plot(reference,type="l",main="Gas Chromatogram",ylab="intensity",lwd=2,col=1)
lines(query,col=2)

dfref <- data.frame(1:length(reference),reference)
dygraph(dfref, main = "Plot of Reference") %>%
  dyRangeSelector()

dfquery <- data.frame(1:length(query),query)
dygraph(dfquery, main = "Plot of Query" )%>%
  dyRangeSelector()

dfrq <- data.frame(1:length(reference),reference,query)
dygraph(dfrq, main = "Plot of Reference and Query") %>%
  dyRangeSelector()


plot(reference,log="y",type="l",main="Gas Chromatogram",ylab="log(intensity)",lwd=2,col=1)
lines(query,col=2)

dfref <- data.frame(1:length(reference),reference)
dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

dfquery <- data.frame(1:length(query),query)
dygraph(dfquery, main = "Plot of Query" )%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

dfrq <- data.frame(1:length(reference),reference,query)
dygraph(dfrq, main = "Plot of Reference and Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

# gg_chrom<-ggplot(chrom, aes(chrom$retention_time_ms)) + 
#  geom_line(aes(y = chrom$total_ion_chromatogram)) 

write.csv(reference, 'reference.csv')

csvref<-read.csv('reference.csv')
dfrqindex <- 1:length(reference)
 #ggdfrq <- ggplot(csvref,aes(csvref$X)) +
 #+ xlab("Index") + ylab("Intensity") + 
#   geom_line(y=aes(csvref$x))
   #+
   #geom_line(data=dfrq, aes(dfrqindex, query), colour="darkblue", size=0.2) + 
   #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
   #             labels = trans_format("log10", math_format(10^.x))) +
   #annotation_logticks() 
 
 ggdfrq <- ggplot(csvref, aes(dfrqindex)) +
   geom_line(y = aes(csvref$X))
 ggdfrq
 
 ggplotly(ggdfrq)



## Example 1 - dilation of a signal
data(reference)
dref <- dilation(reference,150)
plot(reference,log="y",type="l")
lines(dref,col=2)



## Example 2 - dilation of an image
BIN <- (volcano>177)
dBIN <- t(apply(BIN,1,dilation,span=5))
dBIN <- apply(dBIN,2,dilation,span=5)
par(mfrow=c(2,2))
image(volcano)
image(BIN)
image(dBIN)





## Citation
citation("VPdtw")
## Basic Examples of zero-penalty DTW
## Example of exact fit in the middle
query <- c(1,5,4,3,9,8,5,2,6,5,4)
reference <- c(rnorm(5),query,rnorm(5))
lambda <- rep(0,length(reference))
maxshift <- 11
res <- VPdtw(reference,query,lambda,maxshift)
plot(res)
res

resdf <- data.frame(res[1],res[2],res[5])
xVals <- 1:length(res[[3]])
dfquery <- data.frame(xVals,res[3])
dfref <- data.frame(res[1],res[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(res[1],res[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

## Example of exact fit on one side
reference <- c(1,5,4,3,9,8,5,2,6,5,4)
query <- c(rnorm(5),reference)
reference <- c(reference,rnorm(5))
lambda <- rep(0,length(reference))
maxshift <- 6
res <- VPdtw(reference,query,lambda,maxshift)
plot(res)
res

resdf <- data.frame(res[1],res[2],res[5])
xVals <- 1:length(res[[3]])
dfquery <- data.frame(xVals,res[3])
dfref <- data.frame(res[1],res[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(res[1],res[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()


## Example of exact fit on the other side
reference <- c(1,5,4,3,9,8,5,2,6,5,4)
query <- c(reference,rnorm(5))
reference <- c(rnorm(5),reference)
lambda <- rep(0,length(reference))
maxshift <- 6
res <- VPdtw(reference,query,lambda,maxshift)
plot(res)
res

resdf <- data.frame(res[1],res[2],res[5])
xVals <- 1:length(res[[3]])
dfquery <- data.frame(xVals,res[3])
dfref <- data.frame(res[1],res[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(res[1],res[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()


## Example of exact fit except where one query gets dropped and its all on one side
reference <- c(1,5,4,3,9,8,5,2,6,5,4)
query <- c(reference[1:5],20,reference[6:11])
reference <- c(rnorm(5),reference)
query <- c(query,rnorm(5))
lambda <- rep(0,length(reference))
maxshift <- 6
res <- VPdtw(reference,query,lambda,maxshift)
plot(res)
res

resdf <- data.frame(res[1],res[2],res[5])
xVals <- 1:length(res[[3]])
dfquery <- data.frame(xVals,res[3])
dfref <- data.frame(res[1],res[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(res[1],res[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()


## Examples that use penalty term. Examples with long signals
data(reference)
data(query)
## Do alignment on log scale
reference <- log(reference)
query <- log(query)
## VPdtw
result <- VPdtw(reference=reference[1:2500],query=query[1:2500],
                penalty=dilation(reference[1:2500],150)/4,maxshift=150)
plot(result)
result

resultdf <- data.frame(result[1],result[2],result[5])
xVals <- 1:length(result[[3]])
dfquery <- data.frame(xVals,result[3])
dfref <- data.frame(result[1],result[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(result[1],result[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resultdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

## Zero penalty DTW
result2 <- VPdtw(reference=reference[1:2500],query=query[1:2500],
                 penalty=rep(0,length(reference)),maxshift=150)
plot(result2)

result2df <- data.frame(result2[1],result2[2],result2[5])
xVals <- 1:length(result2[[3]])
dfquery <- data.frame(xVals,result2[3])
dfref <- data.frame(result2[1],result2[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(result2[1],result2[6])

dygraph(dfref, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(result2df, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()


## Try both penalties at the same time
penalty <- dilation(reference,350)/5
penalty <- cbind(penalty,rep(0,length(penalty)))
result <- VPdtw(reference,query,penalty=penalty,maxshift=350)
plot(result,"After")
plot(result,"Shift")
result
## All three plots at once
plot(result)

resultdf <- data.frame(result[1],result[2],result[5])
xVals <- 1:length(result[[3]])
dfquery <- data.frame(xVals,result[3])
dfref <- data.frame(result[1],result[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(result[1],result[6])

dygraph(dfquery, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(result2df, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()






