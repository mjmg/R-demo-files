  
  library("dtw")
  
    ## dtw
    idx <- seq(0,6.28,len=100);
    ref <- sin(idx)+runif(100)/10;
    samp <- cos(idx+50)
    
    warpfun <- dtw(samp, ref, keep.internals = TRUE) # necessary for the plot
    
    wx2 <- warp(warpfun)
    warpedsamp <- samp[wx2]
    
    plot(idx, ref, type = "l", xlab = "Points", ylab = "I",
         main = "Dynamic time warping")
    lines(idx, samp, lty = 2, col = "blue")
    
    lines(idx, warpedsamp, col = "green")
    legend("bottomleft", col = c("black", "blue", "green"), lty = c(1,2,1),
           legend = c("Reference", "Sample", "Warped sample"))
    

    