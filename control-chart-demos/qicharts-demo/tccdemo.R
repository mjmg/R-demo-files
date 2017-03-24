library(qicharts)
library(ggplot2)
library(plotly)
library(ggfortify)

# Run chart of 24 random normal variables
tccplot <- tcc(rnorm(24))
tccplot
ggplotly(tccplot) %>% hide_legend()

# Build data frame for example
d <- data.frame(x = rep(1:24, 4),
                mo = (rep(seq(as.Date('2014-1-1'),
                              length.out = 24,
                              by = 'month'),
                          4)),
                n = rbinom(4 * 24, 100, 0.5),
                d = round(runif(4 * 24, 90, 110)),
                g1 = rep(c('a', 'b'), each = 48),
                g2 = rep(c('A', 'B'), each = 24))
# P chart
p <- tcc(n, d, mo, g1 = g1, g2 = g2, breaks = 12, data = d, chart = 'p')
plot(p)
ggplotly(p)
summary(p)


# Run chart with two grouping variables
tccplot <- tcc(n, d, mo, g1 = g1, g2 = g2, data = d)
tccplot
ggplotly(tccplot)

# P chart
tccplot <- tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p')
tccplot
ggplotly(tccplot)


# P chart with baseline fixed to the first 12 data points
tccplot <- tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p', freeze = 12)
tccplot
ggplotly(tccplot)

# P chart with two breaks and summary output
tccplot <- tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p',
    breaks = c(12, 18), print.summary = TRUE)

tccplot
ggplotly(tccplot)



set.seed(1)
# Run chart of 24 samples of a random continuous variable
# with an approximate mean = 12 and standard deviation = 3.
y <- rnorm(24, 12, 3)
tccplot <- tcc(y)
tccplot
ggplotly(tccplot)

# Add subgroup vector (dates) and a target
x <- seq.Date(as.Date('2013-08-04'), by = 'week', length = 24)
tccplot <- tcc(y, x = x, target = 16)
tccplot
ggplotly(tccplot)

# Individuals control chart
tccplot <- tcc(y, x = x, chart = 'i')
tccplot
ggplotly(tccplot)


# Xbar control chart, sample size = 5
y <- rnorm(5 * 24)
x <- rep(x, 5)
tccplot <- tcc(y, x = x, chart = 'xbar')
tccplot
ggplotly(tccplot)

# Create data frame with counts and sample sizes by week
d <- data.frame(week = seq.Date(as.Date('2013-08-04'),
                                by = 'week',
                                length = 36),
                y = c(rbinom(24, 20, 0.5), rbinom(12, 20, 0.8)),
                n = round(rnorm(36, 20, 2)))

# Proportions control chart
tccplot <- tcc(y, n, x = week, data = d[1:24,], chart = 'p')
tccplot
ggplotly(tccplot)

# Introduce change in process performance
tccplot <- tcc(y, n, x = week, data = d, chart = 'p')
tccplot
ggplotly(tccplot)

# Freeze baseline to first 24 samples
tccplot <- tcc(y, n, x = week, data = d, chart = 'p', freeze = 24)
tccplot
ggplotly(tccplot)

# Break control chart before and after change
tccplot <- tcc(y, n, x = week, data = d, chart = 'p', breaks = 24)
tccplot
ggplotly(tccplot)


# Introduce extreme sample value and notes
d$a <- ''
d$a[30] <- 'Extreme value'
d$y[30] <- 1
tccplot <- tcc(y, n, x = week, data = d, chart = 'p',
    breaks = 24,
    notes = a)
tccplot
ggplotly(tccplot)

# Exclude value from calculations
d$a[30] <- 'Value excluded from calculations'
tccplot <- tcc(y, n, x = week, data = d, chart = 'p',
    breaks = 24,
    notes = a,
    exclude = 30)
tccplot
ggplotly(tccplot)
