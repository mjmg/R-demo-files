library(xcms)
library(faahKO)

library(ggplot2)
library(plotly)

cdfpath <- system.file("cdf", package = "faahKO")
cdffiles <- list.files(cdfpath, recursive = TRUE, full.names = TRUE)
xr<-xcmsRaw(cdffiles[1])
xr

plotTIC(xr)
xr@tic
plot(xr@scantime,xr@tic)
plot(xr@scantime,xr@tic, type = 'l')

retention_time_ms<-xr@scantime
total_ion_chromatogram<-xr@tic
chrom <- data.frame(retention_time_ms,total_ion_chromatogram)
write.csv(chrom, 'chrom.csv', row.names = FALSE)

gg_chrom<-ggplot(chrom, aes(chrom$retention_time_ms)) + 
  geom_line(aes(y = chrom$total_ion_chromatogram)) 

gg_chrom
ggplotly(gg_chrom)

spec<-read.csv('chrom.csv',header = T)

gg_chrom<-ggplot(spec, aes(spec$retention_time_ms)) + 
  geom_line(aes(y = spec$total_ion_chromatogram)) 

library(dygraphs)

dygraph(spec) %>%
  dyRangeSelector()

