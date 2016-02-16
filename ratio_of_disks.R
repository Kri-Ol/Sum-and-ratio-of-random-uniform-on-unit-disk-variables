library(ggplot2)
library(data.table)

PDF.r <- function(u) {
    q <- 0.0

    if (u >= 0.0) {
        if (u <= 1.0) {
            q <- u
        } else {
            q <- 1.0/u^3L
        }
    }
    q
}

# sampling
n <- 10^6L

set.seed(12345)
dt.r <- data.table(x = sqrt( runif(n) ), y = sqrt( runif(n) ))
dt.r[ , sum := x / y]

# plot density histogram overlaped with PDF
p <- ggplot(dt.r, aes(x = sum)) +
    geom_histogram(aes(y=..density..),
                   binwidth = 0.05,
                   colour = "black", fill = "blue") +
    stat_function(fun = Vectorize(PDF.r), colour = "cyan") +
    labs(title = "PDF vs MC",
         x = "radius", y = "PDF(r)") +
    xlim(0.0, 4.0) + ylim(0.0, 1.1)
print(p)
