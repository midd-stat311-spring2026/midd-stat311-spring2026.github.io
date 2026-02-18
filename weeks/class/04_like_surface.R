library(plotly)
library(tidyverse)
set.seed(123)
# simulate data points with true mu and sigma2
n <- 10000
x <- rnorm(n, mean = 2, sd = sqrt(2^2))

# create function evaluate log-like at specific values of mu, s2
logLik <- function(mu, s2, x) {
  n <- length(x)
  - (n/2)*log(2*pi) - (n/2)*log(s2) -sum((x - mu)^2) / (2*s2)
}

# create sequences of mu and s2 to evaluate log-like at
mu_seq  <- seq(0, 4, length.out = 100)
s2_seq <- seq(0.01, 6, length.out = 100)

# obtain MLE, along with log-like evaluated at MLE
mle_mu  <- mean(x)
mle_s2 <- sum((x - mle_mu)^2)/n
mle_logLik <- logLik(mle_mu, mle_s2, x)

# evaluate log-like at grid of mu and s2 values
z_matrix <- outer(mu_seq, s2_seq,
                  Vectorize(function(mu, s2)
                    logLik(mu, s2, x)))
# visualize!
plot_ly(
  x = mu_seq,
  y = s2_seq,
  z = z_matrix,
  type = "surface"
) |>
  layout(
    title = "Log-Likelihood Surface",
    scene = list(
      xaxis = list(title = "mu"),
      yaxis = list(title = "s2"),
      zaxis = list(title = "logLik")
    )
  )  |> add_markers(
    x = mle_mu,
    y = mle_s2,
    z = mle_logLik,
    marker = list(color = "red", size = 5),
    name = "MLE"
  )

