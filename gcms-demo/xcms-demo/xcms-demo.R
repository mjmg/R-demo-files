library(xcms)
library(utils)

cdfpath <- system.file("cdf", package = "faahKO")
cdffiles <- list.files(cdfpath, recursive = TRUE, full=T) # input files (step 1)
xset <- xcmsSet(cdffiles)  # peak picking (step 2)
xsg <- group(xset)    # peak alignment (step 3.1)
xsg <- retcor(xsg)     # retention time correction (step 3.2)
xsg <- group(xsg)     # re-align (step 3.3)
xsg <- fillPeaks(xsg)  # filling in missing peak data (step 4)
dat <- groupval(xsg, "medret", "into")  # get peak intensity matrix (step 5)
dat <- rbind(group = as.character(phenoData(xsg)$class), dat)  # add group label
write.csv(dat, file='MyPeakTable.csv')  # save the data to CSV file
