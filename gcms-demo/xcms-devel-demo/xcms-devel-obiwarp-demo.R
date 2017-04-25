library(faahKO)
library(MSnbase)
fls <- dir(system.file("cdf/KO", package = "faahKO"), recursive = TRUE,
           full.names = TRUE)

## Reading 2 of the KO samples
raw_data <- readMSData2(fls[1:2])

## Perform retention time correction on the OnDiskMSnExp:
res <- adjustRtime(raw_data, param = ObiwarpParam())

## As a result we get a numeric vector with the adjusted retention times for
## all spectra.
head(res)

## We can split this by file to get the adjusted retention times for each
## file
resL <- split(res, fromFile(raw_data))

##############################
## Perform retention time correction on an XCMSnExp:
##
## Perform first the chromatographic peak detection using the matchedFilter
## method.
mfp <- MatchedFilterParam(snthresh = 20, binSize = 1)
res <- findChromPeaks(raw_data, param = mfp)

## Performing the retention time adjustment using obiwarp.
res_2 <- adjustRtime(res, param = ObiwarpParam())

head(rtime(res_2))
head(rtime(raw_data))

## Also the retention times of the detected peaks were adjusted.
tail(chromPeaks(res))
tail(chromPeaks(res_2))

