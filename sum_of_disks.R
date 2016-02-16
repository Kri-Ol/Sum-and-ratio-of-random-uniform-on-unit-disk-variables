library(ggplot2)
library(data.table)

PDF.r <- function(R) {
    q <- 0.0

    if (R >= 0.0) {
        if (R <= 2.0) {
            if (R <= 1.0) {
                q <- (2.0/3.0)*R^3L
            } else {
                q <- (R - 1.0)
                q <- 2.0 * R * ( 1.0 - q^2L ) - (4.0/3.0) * ( 1.0 - q^3L )
            }
        }
    }
    q
}

# sampling
n <- 10^6L

dt.r <- data.table(x = sqrt( runif(n) ), y = sqrt( runif(n) ))
dt.r[ , sum := x + y]

# plot density histogram overlaped with PDF
p <- ggplot(dt.r, aes(x = sum)) +
     geom_histogram(aes(y=..density..),
                   binwidth = 0.05,
                   colour = "black", fill = "blue") +
     stat_function(fun = Vectorize(PDF.r), colour="cyan") +
     labs(title = "PDF vs MC",
          x = "radius", y = "PDF(r)") +
     xlim(0.0, 2.0) + ylim(0.0, 1.2)
print(p)
