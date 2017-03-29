# Load the qicharts package
library(qicharts)

# Lock random number generator to reproduce the charts from this tutorial
set.seed(7)

# Create vector of random values to plot
y <- rnorm(24)

# Plot I chart
qic(y, chart = 'i')

tcc(y, chart = 'i')


# Introduce an outlier at data point number 18
y[18] <- 5

# Plot I chart
qic(y, chart = 'i')
tcc(y, chart = 'i')


# Vector of birth weights from 24 babies
y <- round(rnorm(24, mean = 3400, sd = 400))
y

# Plot I chart of individual birth weights
qic(y,
    chart = 'i',
    main  = 'Birth weight (I chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')
tcc(y,
    chart = 'i',
    main  = 'Birth weight (I chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')

# Plot moving ranges
qic(y,
    chart = 'mr',
    main  = 'Pairwise differences in birth weights (MR chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')
tcc(y,
    chart = 'mr',
    main  = 'Pairwise differences in birth weights (MR chart)',
    ylab  = 'Grams',
    xlab  = 'Baby no.')



# Setup parameters
m.beds       <- 300
m.stay       <- 4
m.days       <- m.beds * 7
m.discharges <- m.days / m.stay
p.pu         <- 0.08

# Simulate data
discharges  <- rpois(24, lambda = m.discharges)
patientdays <- round(rnorm(24, mean = m.days, sd = 100))
n.pu        <- rpois(24, lambda = m.discharges * p.pu * 1.5)
n.pat.pu    <- rbinom(24, size = discharges, prob = p.pu)
week        <- seq(as.Date('2014-1-1'),
                   length.out = 24, 
                   by         = 'week') 

# Combine data into a data frame
d <- data.frame(week, discharges, patientdays,n.pu, n.pat.pu)
d


# Rebuild data frame 
d <- data.frame(n.pat.pu, discharges, week)
d

# Plot standardised P chart
qic(n.pat.pu, 
    n            = discharges,
    x            = week, 
    data         = d,
    chart        = 'p',
    standardised = TRUE,
    main         = 'Patients with hospital acquired pressure ulcers (Standardised P chart)',
    ylab         = 'Standard deviations',
    xlab         = 'Week')

tcc(n.pat.pu, 
    n            = discharges,
    x            = week, 
    data         = d,
    chart        = 'p',
    standardised = TRUE,
    main         = 'Patients with hospital acquired pressure ulcers (Standardised P chart)',
    ylab         = 'Standard deviations',
    xlab         = 'Week')
