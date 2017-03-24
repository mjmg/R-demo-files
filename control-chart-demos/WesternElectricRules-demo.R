library(plyr)

x <- c(rep(10, 90), rep(10.5, 10)) + rnorm(100, mean=0, sd=0.5)

find_zones <- function(x) {
  x.mean <- mean(x)
  x.sd <- sd(x)
  boundaries <- seq(-4, 4)
  # creates a set of zones for each point in x
  zones <- sapply(boundaries, function(i) {
    i * rep(x.sd, length(x))
  })
  zones + x.mean
}

head(find_zones(x))
# [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9]
# [1,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27
# [2,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27
# [3,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27
# [4,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27
# [5,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27
# [6,] 7.954 8.493 9.032 9.572 10.11 10.65 11.19 11.73 12.27

evaluate_zones <- function(x) {
  zones <- find_zones(x)
  colnames(zones) <- paste("zone", -4:4, sep="_")
  x.zones <- rowSums(x > zones) - 4
  x.zones
}

evaluate_zones(x)
# [1]  0  2  0  1  2  0  0  1 -1  0 -1  1  1  1 -2  1 ...

find_violations <- function(x.zones, i) {
  values <- x.zones[max(i-8, 1):i]
  rule4 <- ifelse(all(values > 0), 1,
                  ifelse(all(values < 0), -1,
                         0))

  values <- x.zones[max(i-5, 1):i]
  rule3 <- ifelse(sum(values >= 2) >= 4, 1,
                  ifelse(sum(values <= -2) >= 4, -1,
                         0))
  
  values <- x.zones[max(i-3, 1):i]
  rule2 <- ifelse(sum(values >= 3) >= 2, 1,
                  ifelse(sum(values <= -3) >= 2, -1,
                         0))
  
  values <- x.zones[i]
  rule1 <- ifelse(any(values > 3), 1,
                  ifelse(any(values < -3), -1,
                         0))
  
  c("rule1"=rule1, "rule2"=rule2, "rule3"=rule3, "rule4"=rule4)
}

find_violations(evaluate_zones(x), 70)
# rule1 rule2 rule3 rule4 
# 0     0     0     0 

compute_violations <- function(x, start=1) {
  x.zones <- evaluate_zones(x)
  results <- ldply(start:length(x), function(i) {
    find_violations(x.zones, i)
  })
  results$color <- ifelse(results$rule1!=0, "pink",
                          ifelse(results$rule2!=0, "red",
                                 ifelse(results$rule3!=0, "orange",
                                        ifelse(results$rule4!=0, "yellow",
                                               "black"))))
  results
}

tail(compute_violations(x))
# rule1 rule2 rule3 rule4 color
# 95      0     1     1     0   red
# 96      1     1     1     0  pink
# 97      0     1     1     0   red
# 98      0     1     1     0   red
# 99      0     1     1     1   red
# 100     1     1     1     1  pink

plot.wer <- function(x, holdout) {
  wer <- compute_violations(x, length(x) - holdout)
  bands <- find_zones(x)
  plot.data <- x[(length(x) - holdout):length(x)]
  plot(plot.data, col=wer$color, type='b', pch=19,
       ylim=c(min(bands), max(bands)),
       main="Western Eletric Rule Ouput",
       xlab="", ylab="")
  
  for (i in 1:7) {
    lines(bands[,i], col=cols[i], lwd=0.75, lty=2)
  }
}

x <- c(rep(10, 90), rep(10.5, 10)) + rnorm(100, mean=0, sd=0.5)
plot.wer(x, 30)
