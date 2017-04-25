
## Load some of the files from the faahKO package.
library(faahKO)
fs <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
        system.file('cdf/KO/ko16.CDF', package = "faahKO"),
        system.file('cdf/KO/ko18.CDF', package = "faahKO"))
## Read the files
od <- readMSData2(fs)

## Perform peak detection on them using default matched filter settings.
mfp <- MatchedFilterParam()
xod <- findChromPeaks(od, param = mfp)

## Subset the dataset to the first and third file.
xod_sub <- filterFile(xod, file = c(1, 3))

## The number of chromatographic peaks per file for the full object
table(chromPeaks(xod)[, "sample"])

## The number of chromatographic peaks per file for the subset
table(chromPeaks(xod_sub)[, "sample"])

basename(fileNames(xod))
basename(fileNames(xod_sub))

## Filter on mz values; chromatographic peaks and features within the
## mz range are retained (as well as adjusted retention times).
xod_sub <- filterMz(xod, mz = c(300, 400))
head(chromPeaks(xod_sub))
nrow(chromPeaks(xod_sub))
nrow(chromPeaks(xod))

## Filter on rt values. All chromatographic peaks and features within the
## retention time range are retained. Filtering is performed by default on
## adjusted retention times, if present.
xod_sub <- filterRt(xod, rt = c(2700, 2900))

range(rtime(xod_sub))
head(chromPeaks(xod_sub))
range(chromPeaks(xod_sub)[, "rt"])

nrow(chromPeaks(xod))
nrow(chromPeaks(xod_sub))

