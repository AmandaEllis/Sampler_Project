x <- seq(0,1, length=100)
h1 <- dbeta(x,2,6)
h2 <- dbeta(x,6,2)

plot(x, h1, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions", col="blue")

lines(x, h2, lty=2, col="red")


