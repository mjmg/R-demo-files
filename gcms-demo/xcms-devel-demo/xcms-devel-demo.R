## ----message = FALSE, warning = FALSE--------------------------------------
library(xcms)
library(RColorBrewer)
register(SerialParam())

## ----message = FALSE, warning = FALSE--------------------------------------
## Reading the raw data using the MSnbase package
library(xcms)
## Load 6 of the CDF files from the faahKO
cdf_files <- dir(system.file("cdf", package = "faahKO"), recursive = TRUE,
                 full.names = TRUE)

## Define the sample grouping.
s_groups <- rep("KO", length(cdf_files))
s_groups[grep(cdf_files, pattern = "WT")] <- "WT"
## Define a data.frame that will be used as phenodata
pheno <- data.frame(sample_name = sub(basename(cdf_files), pattern = ".CDF",
                                      replacement = "", fixed = TRUE),
                    sample_group = s_groups, stringsAsFactors = FALSE)

## Read the data.
raw_data <- readMSData2(cdf_files, pdata = new("NAnnotatedDataFrame", pheno))

## ----faahKO-tic, message = FALSE, fig.align = 'center', fig.width = 8, fig.height = 4----
library(RColorBrewer)
sample_colors <- brewer.pal(3, "Set1")[1:2]
names(sample_colors) <- c("KO", "WT")
## Subset the full raw data by file and plot the data.
tmp <- filterFile(raw_data, file = 1)
plot(x = rtime(tmp), y = tic(tmp), xlab = "retention time", ylab = "TIC",
     col = paste0(sample_colors[pData(tmp)$sample_group], 80), type = "l")
for (i in 2:length(fileNames(raw_data))) {
  tmp <- filterFile(raw_data, file = i)
  points(rtime(tmp), tic(tmp), type = "l",
         col = paste0(sample_colors[pData(tmp)$sample_group], 80))
}
legend("topleft", col = sample_colors, legend = names(sample_colors), lty = 1)

## ----faahKO-bpi, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
## Get the base peak chromatograms. This reads data from the files.
bpis <- extractChromatograms(raw_data, aggregationFun = "max")
plot(3, 3, pch = NA, xlim = range(unlist(lapply(bpis, rtime))),
     ylim = range(unlist(lapply(bpis, intensity))), main = "BPC",
     xlab = "rtime", ylab = "intensity")
for (i in 1:length(bpis)) {
  points(rtime(bpis[[i]]), intensity(bpis[[i]]), type = "l",
         col = paste0(sample_colors[pData(raw_data)$sample_group[i]], 80))
}

## ----faahKO-tic-boxplot, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
## Get the total ion current by file
tc <- split(tic(raw_data), f = fromFile(raw_data))
boxplot(tc, col = paste0(sample_colors[pData(raw_data)$sample_group], 80),
        ylab = "intensity", main = "Total ion current")

## ----faahKO-centWave-------------------------------------------------------
## Defining the settings for the centWave peak detection.
cwp <- CentWaveParam(snthresh = 20, noise = 1000)
xod <- findChromPeaks(raw_data, param = cwp)

## ----faahKO-peak-intensity-boxplot, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
ints <- split(chromPeaks(xod)[, "into"], f = chromPeaks(xod)[, "sample"])
ints <- lapply(ints, log2)
boxplot(ints, varwidth = TRUE, col = sample_colors[pData(xod)$sample_group],
        ylab = expression(log[2]~intensity), main = "Peak intensities")

## ----faahKO-chromPeaks-extractChroms, warning = FALSE----------------------
rtr <- chromPeaks(xod)[68, c("rtmin", "rtmax")]
## Increase the range:
rtr[1] <- rtr[1] - 60
rtr[2] <- rtr[2] + 60
mzr <- chromPeaks(xod)[68, c("mzmin", "mzmax")]

chrs <- extractChromatograms(xod, rt = rtr, mz = mzr)

## In addition we get all peaks detected in the same region
pks <- chromPeaks(xod, rt = rtr, mz = mzr)

## ----faahKO-extracted-chrom-with-peaks, message = FALSE, fig.cap = "Extracted ion chromatogram for one of the identified peaks. Each line represents the signal measured in one sample. The rectangles indicate the margins of the identified chromatographic peak in the respective sample.", fig.align = "center", fig.width = 8, fig.height = 8----
## Define the limits on x- and y-dimension
xl <- range(lapply(chrs, rtime), na.rm = TRUE)
yl <- range(lapply(chrs, intensity), na.rm = TRUE)
plot(3, 3, pch = NA, main = paste(format(mzr, digits = 6), collapse = "-"),
     xlab = "rt", ylab = "intensity", xlim = xl, ylim = yl)
## Plot the chromatogram per sample
for (i in 1:length(chrs)) {
  points(rtime(chrs[[i]]), intensity(chrs[[i]]), type = "l",
         col = sample_colors[pData(xod)$sample_group[i]])
}
## Highlight the identified chromatographic peaks.
for (i in 1:nrow(pks)) {
  rect(xleft = pks[i, "rtmin"], xright = pks[i, "rtmax"], ybottom = 0,
       ytop = pks[i, "maxo"],
       border = paste0(sample_colors[pData(xod)$sample_group][pks[i, "sample"]], 60))
}

## ----faahKO-obiwarp, message = FALSE---------------------------------------
## Doing the obiwarp alignment using the default settings.
xod <- adjustRtime(xod, param = ObiwarpParam())

## ----faahKO-bpi-obiwarp, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 8----
## Get the base peak chromatograms. This reads data from the files.
bpis <- extractChromatograms(xod, aggregationFun = "max")

par(mfrow = c(2, 1), mar = c(4.5, 4.2, 1, 0.5))
plot(3, 3, pch = NA, xlim = range(unlist(lapply(bpis, rtime))),
     ylim = range(unlist(lapply(bpis, intensity))), main = "BPC",
     xlab = "rtime", ylab = "intensity")
for (i in 1:length(bpis)) {
  points(rtime(bpis[[i]]), intensity(bpis[[i]]), type = "l",
         col = paste0(sample_colors[pData(xod)$sample_group[i]], 80))
}
## Plot also the difference of adjusted to raw retention time.
plotAdjustedRtime(xod, col = paste0(sample_colors[pData(xod)$sample_group], 80))

## ----faahKO-adjusted-rtime-boxplot, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
## Calculate the difference between the adjusted and the raw retention times.
diffRt <- rtime(xod) - rtime(xod, adjusted = FALSE)

## By default, rtime and most other accessor methods return a numeric vector. To
## get the values grouped by sample we have to split this vector by file/sample
diffRt <- split(diffRt, fromFile(xod))

boxplot(diffRt, col = sample_colors[pData(xod)$sample_group],
        main = "Obiwarp alignment results", ylab = "adjusted - raw rt")

## ----faahKO-extracted-chrom-with-peaks-aligned, echo = FALSE, message = FALSE, fig.cap = "Extracted ion chromatogram for one of the identified peaks after alignment.", fig.align = "center", fig.width = 8, fig.height = 8----
rtr <- chromPeaks(xod)[68, c("rtmin", "rtmax")]
## Increase the range:
rtr[1] <- rtr[1] - 60
rtr[2] <- rtr[2] + 60
mzr <- chromPeaks(xod)[68, c("mzmin", "mzmax")]

chrs <- extractChromatograms(xod, rt = rtr, mz = mzr)

## In addition we get all peaks detected in the same region
pks <- chromPeaks(xod, rt = rtr, mz = mzr)

## Define the limits on x- and y-dimension
xl <- range(lapply(chrs, rtime), na.rm = TRUE)
yl <- range(lapply(chrs, intensity), na.rm = TRUE)
plot(3, 3, pch = NA, main = paste(format(mzr, digits = 6), collapse = "-"),
     xlab = "rt", ylab = "intensity", xlim = xl, ylim = yl)
## Plot the chromatogram per sample
for (i in 1:length(chrs)) {
  points(rtime(chrs[[i]]), intensity(chrs[[i]]), type = "l",
         col = sample_colors[pData(xod)$sample_group[i]])
}
## Highlight the identified chromatographic peaks.
for (i in 1:nrow(pks)) {
  rect(xleft = pks[i, "rtmin"], xright = pks[i, "rtmax"], ybottom = 0,
       ytop = pks[i, "maxo"],
       border = paste0(sample_colors[pData(xod)$sample_group][pks[i, "sample"]], 60))
}

## ----faahKO-groupPeakDensity, message = FALSE------------------------------
## Define the PeakDensityParam
pdp <- PeakDensityParam(sampleGroups = pData(xod)$sample_group,
                        maxFeatures = 300, minFraction = 0.66)
xod <- groupChromPeaks(xod, param = pdp)

## ----faahKO-featureDefinitions, message = FALSE----------------------------
head(featureDefinitions(xod))

## ----faahKO-featureValues, message = FALSE---------------------------------
## Extract the "into" peak integrated signal.
head(featureValues(xod, value = "into"))

## ----faahKO-fillPeaks, message = FALSE-------------------------------------
## Fill in peaks with default settings. Settings can be adjusted by passing
## a FillChromPeaksParam object to the method.
xod <- fillChromPeaks(xod)

head(featureValues(xod, value = "into"))

## ----faahKO-processHistory, message = FALSE--------------------------------
## List the full process history
processHistory(xod)

## ----faahKO-processHistory-select, message = FALSE-------------------------
ph <- processHistory(xod, type = "Retention time correction")

## Access the parameter
processParam(ph[[1]])

## ----faahKO-drop-alignment, message = FALSE--------------------------------
## Remove the alignment results
xod <- dropAdjustedRtime(xod)

processHistory(xod)

## ----faahKO-initial-correspondence, message = FALSE------------------------
## Define the parameter for the correspondence
pdparam <- PeakDensityParam(sampleGroups = pData(xod)$sample_group,
                            minFraction = 0.7, maxFeatures = 100)
xod <- groupChromPeaks(xod, param = pdparam)

## ----faahKO-peak-groups-matrix, message = FALSE----------------------------
## Create the parameter class for the alignment
pgparam <- PeakGroupsParam(minFraction = 0.9, span = 0.4)

## Extract the matrix with (raw) retention times for the peak groups that would
## be used for alignment.
adjustRtimePeakGroups(xod, param = pgparam)

## ----faahKO-peak-groups-alignment, message = FALSE-------------------------
## Perform the alignment using the peak groups method.
xod <- adjustRtime(xod, param = pgparam)

## ----faahKO-peak-groups-alignment-plot, message = FALSE, fig.align = "center", fig.width = 8, fig.height = 4----
plotAdjustedRtime(xod, col = sample_colors[pData(xod)$sample_group])

## ----message = FALSE-------------------------------------------------------
## Defining the variables:
set.seed(123)
X <- sort(abs(rnorm(30, mean = 20, sd = 25))) ## 10
Y <- abs(rnorm(30, mean = 50, sd = 30))

## Bin the values in Y into 20 bins defined on X
res <- binYonX(X, Y, nBins = 22)

res

## ----binning-imputation-example, message = FALSE, fig.width = 10, fig.height = 7, fig.cap = 'Binning and missing value imputation results. Black points represent the input values, red the results from the binning and blue and green the results from the imputation (with method lin and linbase, respectively).'----
## Plot the actual data values.
plot(X, Y, pch = 16, ylim = c(0, max(Y)))
## Visualizing the bins
abline(v = breaks_on_nBins(min(X), max(X), nBins = 22), col = "grey")

## Define colors:
point_colors <- paste0(brewer.pal(4, "Set1"), 80)
## Plot the binned values.
points(x = res$x, y = res$y, col = point_colors[1], pch = 15)

## Perform the linear imputation.
res_lin <- imputeLinInterpol(res$y)

points(x = res$x, y = res_lin, col = point_colors[2], type = "b")

## Perform the linear imputation "linbase"
res_linbase <- imputeLinInterpol(res$y, method = "linbase")
points(x = res$x, y = res_linbase, col = point_colors[3], type = "b", lty = 2)

## --------------------------------------------------------------------------
## Define a vector with empty values at the end.
X <- 1:11
set.seed(123)
Y <- sort(rnorm(11, mean = 20, sd = 10))
Y[9:11] <- NA
nas <- is.na(Y)
## Do interpolation with profBinLin:
resX <- xcms:::profBinLin(X[!nas], Y[!nas], 5, xstart = min(X),
                          xend = max(X))
resX
res <- binYonX(X, Y, nBins = 5L, shiftByHalfBinSize = TRUE)
resM <- imputeLinInterpol(res$y, method = "lin",
                          noInterpolAtEnds = TRUE)
resM

## ----profBinLin-problems, message = FALSE, fig.align = 'center', fig.width=10, fig.height = 7, fig.cap = "Illustration of the two bugs in profBinLin. The input values are represented by black points, grey vertical lines indicate the bins. The results from binning and interpolation with profBinLin are shown in blue and those from binYonX in combination with imputeLinInterpol in green."----
plot(x = X, y = Y, pch = 16, ylim = c(0, max(Y, na.rm = TRUE)),
     xlim = c(0, 12))
## Plot the breaks
abline(v = breaks_on_nBins(min(X), max(X), 5L, TRUE), col = "grey")
## Result from profBinLin:
points(x = res$x, y = resX, col = "blue", type = "b")
## Results from imputeLinInterpol
points(x = res$x, y = resM, col = "green", type = "b",
       pch = 4, lty = 2)

## ----eval = FALSE----------------------------------------------------------
#  mzarea <- seq(which.min(abs(mzs - peakArea[i, "mzmin"])),
#  	      which.min(abs(mzs - peakArea[i, "mzmax"])))

