### R code from vignette source 'runGC.Rnw'

###################################################
### code chunk number 1: runGC.Rnw:110-114
###################################################
library(metaMS)
data(FEMsettings)
TSQXLS.GC
metaSetting(TSQXLS.GC, "PeakPicking")


###################################################
### code chunk number 2: runGC.Rnw:137-138
###################################################
data(threeStdsDB)


###################################################
### code chunk number 3: runGC.Rnw:140-148 (eval = FALSE)
###################################################
## library(metaMSdata)
## data(threeStdsDB)        ## provides DB
## 
## cdfdir <- system.file("extdata", package = "metaMSdata")
## cdffiles <- list.files(cdfdir, pattern = "_GC_",
##                        full.names = TRUE, ignore.case = TRUE)
## result <- runGC(files = cdffiles, settings = TSQXLS.GC, DB = DB,
##                 nSlaves = 2)


###################################################
### code chunk number 4: runGC.Rnw:152-153 (eval = FALSE)
###################################################
## result <- runGC(xset = GCset, settings = TSQXLS.GC, DB = DB)


###################################################
### code chunk number 5: runGC.Rnw:168-169
###################################################
data("GCresults")


###################################################
### code chunk number 6: runGC.Rnw:171-172
###################################################
allSamples.msp <- GCresults$samples.msp


###################################################
### code chunk number 7: runGC.Rnw:183-186 (eval = FALSE)
###################################################
## GCset <- peakDetection(cdffiles, 
##                        settings = metaSetting(TSQXLS.GC, "PeakPicking"), 
##                        convert2list = TRUE, nSlaves = 2)


###################################################
### code chunk number 8: runGC.Rnw:220-222 (eval = FALSE)
###################################################
## allSamples <- lapply(GCset, runCAMERA, chrom = "GC", 
##                      settings = metaSetting(TSQXLS.GC, "CAMERA"))


###################################################
### code chunk number 9: runGC.Rnw:233-235 (eval = FALSE)
###################################################
## allSamples.msp <- lapply(allSamples, to.msp, file = NULL, 
##                          settings = metaSetting(TSQXLS.GC, "DBconstruction"))


###################################################
### code chunk number 10: runGC.Rnw:237-239
###################################################
sapply(allSamples.msp, length)
allSamples.msp[[1]][[26]]


###################################################
### code chunk number 11: runGC.Rnw:252-253
###################################################
plotPseudoSpectrum(allSamples.msp[[1]][[26]])


###################################################
### code chunk number 12: runGC.Rnw:270-276
###################################################
DB.treated <- treat.DB(DB)
allSam.matches <- 
  matchSamples2DB(allSamples.msp, DB = DB.treated, 
                  settings = metaSetting(TSQXLS.GC, "match2DB"), 
                  quick = FALSE)
allSam.matches


###################################################
### code chunk number 13: runGC.Rnw:292-294
###################################################
matchExpSpec(allSamples.msp[[1]][[4]], DB.treated, 
             DB.treated = TRUE, plotIt = TRUE)


###################################################
### code chunk number 14: runGC.Rnw:333-341
###################################################
allSamples.msp.scaled <- lapply(allSamples.msp, treat.DB, 
                                isMSP = FALSE)
allSam.matches <- 
  matchSamples2Samples(allSamples.msp.scaled, 
                       allSamples.msp, 
                       annotations = allSam.matches$annotations, 
                       settings = metaSetting(TSQXLS.GC, "betweenSamples"))
names(allSam.matches)


###################################################
### code chunk number 15: runGC.Rnw:351-352
###################################################
allSam.matches$annotations[[1]]


###################################################
### code chunk number 16: runGC.Rnw:365-368
###################################################
features.df <- getFeatureInfo(stdDB = DB, allMatches = allSam.matches, 
                              sampleList = allSamples.msp)
features.df[, c(1:3, ncol(features.df) - 2:0)]


###################################################
### code chunk number 17: runGC.Rnw:390-395
###################################################
PseudoSpectra <- constructExpPseudoSpectra(allMatches = allSam.matches, 
                                           standardsDB = DB)
ann.df <- getAnnotationMat(exp.msp = allSamples.msp, pspectra = PseudoSpectra, 
                           allMatches = allSam.matches)
ann.df


###################################################
### code chunk number 18: runGC.Rnw:401-405
###################################################
ann.df2 <- sweep(ann.df, 1, sapply(PseudoSpectra, 
                                   function(x) max(x$pspectrum[, 2])), 
                 FUN = "*")
ann.df2


###################################################
### code chunk number 19: runGC.Rnw:429-435
###################################################
library(metaMSdata)
stddir <- system.file("extdata", package = "metaMSdata")
input.file <- list.files(stddir, pattern = "csv", full.names = TRUE)
threeStdsInfo <- readStdInfo(input.file, stddir, sep = ";", dec = ",")
threeStdsInfo[,"stdFile"] <- file.path(stddir, "STDmix_GC_03.CDF")
threeStdsInfo[,c(1:4, 8)]


###################################################
### code chunk number 20: runGC.Rnw:450-453 (eval = FALSE)
###################################################
## data(threeStdsNIST)                       ## provides smallDB
## DB <- createSTDdbGC(threeStdsInfo, TSQXLS.GC, extDB = smallDB,
##                     nSlaves = 2)


###################################################
### code chunk number 21: runGC.Rnw:462-463
###################################################
names(DB[[1]])

