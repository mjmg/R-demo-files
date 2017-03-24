

getRGBGrProfile <- function (imgfilename, display=0, kmean=0, kmed=0) {
  library("EBImage")
  img = readImage(imgfilename)
  
  if (display != 0){
    display(img, "Image")
  }
  
  redProfile = rowMeans(channel(img,"red"))
  greenProfile = rowMeans(channel(img,"green"))
  blueProfile = rowMeans(channel(img,"blue"))

  # Luminance-preserving Color to Grayscale conversion using 
  # CIE 1931 luminance weights: 0.2126 * R + 0.7152 * G + 0.0722 * B
  graychannel = channel(img,"luminance")
  grayProfile = rowMeans(graychannel)

  numofpts <- (1:(dim(img)[1]))

  if (kmed > 1) {
    redProfileRunMed <- runmed(redProfile, kmed)
    greenProfileRunMed <- runmed(greenProfile, kmed)
    blueProfileRunMed <- runmed(blueProfile, kmed)
    grayProfileRunMed <- runmed(grayProfile, kmed)
    
    RGBGrProfile <- data.frame(redProfileRunMed, greenProfileRunMed, blueProfileRunMed, grayProfileRunMed)
  }

  else if (kmean > 1) {
    redProfileRMeans <- rowMeans(embed(redProfile), kmean)
    greenProfileRMeans <- rowMeans(embed(greenProfile), kmean)
    blueProfileRMeans <- rowMeans(embed(blueProfile), kmean)
    grayProfileRMeans <- rowMeans(embed(grayProfile), kmean)
    
    RGBGrProfile <- data.frame(redProfileRMeans, greenProfileRMeans, blueProfileRMeans, grayProfileRMeans)
  }
  
  else {
    RGBGrProfile <- data.frame(redProfile, greenProfile, blueProfile, grayProfile)
  }
  
  write.csv(RGBGrProfile, file = "RGBGrProfiles.csv", row.names=TRUE)

}

library("EBImage")
library("dygraphs")
#getRGBGrProfile ("http://i.imgur.com/nH7zS9b.jpg",1,5)
#getRGBGrProfile ("nH7zS9b.jpg",1,5)

RGBGrProfilecsv <- read.csv("RGBGrProfiles.csv")

#dygraph(cbind(numofpts,RGBGrProfilecsv))

dygraph(RGBGrProfilecsv) %>%
dyRangeSelector()

