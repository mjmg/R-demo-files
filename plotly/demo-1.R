data(people)

#par(mfrow = c(2, 2))
mdaplot(people, type = 'p')
mdaplot(people[, c(6, 7)], type = 'p')
mdaplot(people[, 1], type = 'p', ylab = 'Height')
mdaplot(people[1, ], type = 'p', ylab = '')

p1 <-ggplot(people[, c(6, 7)])


df <- data.frame(people[, c(6, 7)])
df

ggp <- ggplot(df) +  geom_point(aes(x=df$Income, y = df$Beer))
ggplotly(ggp)


p <- ggplot(df, aes(x=df$Income, y=df$Beer)) +
  geom_point(shape=1)
ggplotly(p)
       