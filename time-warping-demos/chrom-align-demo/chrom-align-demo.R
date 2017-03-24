library(xcms)
library(faahKO)
library(VPdtw)
library(ggplot2)
library(plotly)

cdfpath <- system.file("cdf", package = "faahKO")
cdffiles <- list.files(cdfpath, recursive = TRUE, full.names = TRUE)
xr1<-xcmsRaw(cdffiles[1])
xr1

plotTIC(xr1)
xr1@tic
plot(xr1@scantime,xr1@tic)
plot(xr1@scantime,xr1@tic, type = 'l')

retention_time_ms<-xr1@scantime
total_ion_chromatogram<-xr1@tic
chrom1 <- data.frame(retention_time_ms,total_ion_chromatogram)
write.csv(chrom1, 'chrom1.csv', row.names = FALSE)

gg_chrom1<-ggplot(chrom1, aes(chrom1$retention_time_ms)) + 
  geom_line(aes(y = chrom1$total_ion_chromatogram)) 

gg_chrom1
ggplotly(gg_chrom1)

spec1<-read.csv('chrom1.csv',header = T)

gg_chrom1<-ggplot(spec1, aes(spec1$retention_time_ms)) + 
  geom_line(aes(y = spec1$total_ion_chromatogram)) 

library(dygraphs)

dygraph(spec1) %>%
  dyRangeSelector()




xr2<-xcmsRaw(cdffiles[2])
retention_time_ms<-xr2@scantime
total_ion_chromatogram<-xr2@tic
chrom2 <- data.frame(retention_time_ms,total_ion_chromatogram)
write.csv(chrom2, 'chrom2.csv', row.names = FALSE)
spec2<-read.csv('chrom2.csv',header = T)

gg_chrom2<-ggplot(spec2, aes(spec2$retention_time_ms)) + 
  geom_line(aes(y = spec2$total_ion_chromatogram)) 
gg_chrom2
ggplotly(gg_chrom2)
dygraph(spec2) %>%
  dyRangeSelector()

spec1<-read.csv('chrom1.csv',header = T)
spec2<-read.csv('chrom2.csv',header = T)
## Do alignment on log scale
#spec1$total_ion_chromatogram <- log(spec1$total_ion_chromatogram)
#spec2$total_ion_chromatogram <- log(spec2$total_ion_chromatogram)

dygraph(spec1, main = "Plot of Reference") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(spec2, main = "Plot of Query") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

## VPdtw
result <- VPdtw(reference=spec1$total_ion_chromatogram,query=spec2$total_ion_chromatogram,
                penalty=dilation(spec1$total_ion_chromatogram,150)/4,maxshift=150)
plot(result)
result

resultdf <- data.frame(result[1],result[2],result[5])
xVals <- 1:length(result[[3]])
dfquery <- data.frame(xVals,result[3])
dfref <- data.frame(result[1],result[2])
dfrq <- join(dfref,dfquery,type="full")
dfshift <- data.frame(result[1],result[6])

dygraph(dfrq, main = "Query and Reference Before Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(resultdf, main = "Query and Reference After Alignment") %>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()
dygraph(dfshift, main = "Shifts Required for Alignment")%>%
  dyAxis("y",logscale="y") %>%
  dyRangeSelector()

