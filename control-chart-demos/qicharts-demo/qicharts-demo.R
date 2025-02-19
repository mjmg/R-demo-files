require(qicharts)
require(widgetframe)
library(ggplot2)
library(plotly)

set.seed(1)
# Run chart of 24 samples of a random continuous variable
# with an approximate mean = 12 and standard deviation = 3.
y <- rnorm(24, 12, 3)
qplot <- qic(y)
qplot
ggplotly(qplot)


# Add subgroup vector (dates) and a target
x <- seq.Date(as.Date('2013-08-04'), by = 'week', length = 24)
qic(y, x = x, target = 16)

# Individuals control chart
qic(y, x = x, chart = 'i')

# Xbar control chart, sample size = 5
y <- rnorm(5 * 24)
x <- rep(x, 5)
qic(y, x = x, chart = 'xbar')

# Create data frame with counts and sample sizes by week
d <- data.frame(week = seq.Date(as.Date('2013-08-04'),
                                by = 'week',
                                length = 36),
                y = c(rbinom(24, 20, 0.5), rbinom(12, 20, 0.8)),
                n = round(rnorm(36, 20, 2)))

# Proportions control chart
qic(y, n, x = week, data = d[1:24,], chart = 'p')

# Introduce change in process performance
qic(y, n, x = week, data = d, chart = 'p')

# Freeze baseline to first 24 samples
qic(y, n, x = week, data = d, chart = 'p', freeze = 24)

# Break control chart before and after change
qic(y, n, x = week, data = d, chart = 'p', breaks = 24)

# Introduce extreme sample value and notes
d$a <- ''
d$a[30] <- 'Extreme value'
d$y[30] <- 1
qic(y, n, x = week, data = d, chart = 'p',
    breaks = 24,
    notes = a)

# Exclude value from calculations
d$a[30] <- 'Value excluded from calculations'
qic(y, n, x = week, data = d, chart = 'p',
    breaks = 24,
    notes = a,
    exclude = 30)




# Load qicharts2
library(qicharts2)

# Lock random number generator to make examples reproducible.
set.seed(19)

# Generate 24 random numbers from a normal distribution.
y <- rnorm(24)

qc<-qic(y)

ggplotly(qc)

qc2<-qic(y, chart = 'i')

qcw<-ggplotly(qc2)


htmlwidgets::saveWidget(frameableWidget(qcw),'qcw.html')




head(gtt)
qgtt<-qic(month, harms, days,
    data     = gtt,
    multiply = 1000,
    title    = 'Patient harm',
    ylab     = 'Adverse events per 1000 patient days',
    xlab     = 'Month')

htmlwidgets::saveWidget(frameableWidget(ggplotly(qgtt)),'qgtt.html')
