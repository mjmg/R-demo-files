library(qcc)
library(qicharts)

Aw <- round(rnorm(240, mean = 6, sd = 1.2))
date = (seq(as.Date('2014-1-1'),
           length.out = 240,
           by = 'day'))

#Build data frame for examples
d <- data.frame(date,
                Aw)

# I chart
qic(Aw, x=date, chart = 'i')
ggplotly(tcc(Aw, x=date, chart = 'i'))


# MR chart
qic(Aw, x=date, chart = 'mr')
ggplotly(tcc(Aw, x=day, chart = 'mr'))

