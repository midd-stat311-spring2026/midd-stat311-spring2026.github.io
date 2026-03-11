library(tidyverse)
theta <- 3
p_true <- exp(-theta) * theta
n_vec <- 2:10
B <- 1000
df_ls <- list()
for(l in 1:length(n_vec)){
  n <- n_vec[l]
  p_hat_naive_vec <- p_hat_rb_vec <- rep(NA,B)
  for(i in 1:B){
    samps <- rpois(n, theta)
    t <- sum(samps)
    p_hat_naive_vec[i] <- sum(samps == 1) / n
    
    p_hat_rb_vec[i] <-  t/n * (1-1/n)^(t - 1)
    
  }
  mse_naive <- mean( (p_hat_naive_vec -p_true)^2)
  mse_rb <- mean( (p_hat_rb_vec -p_true)^2)
  df_ls[[l]] <- data.frame(n, naive= mse_naive, RB = mse_rb)
}

do.call(rbind, df_ls) |>
  pivot_longer(cols = c(2:3), names_to = "estimator", values_to = "MSE") |>
  ggplot(aes(x = n, y = MSE, col = estimator)) +
  geom_line() +
  labs(y = "MSE (empirical)", title = "Mushroom hunt") +
  theme(text = element_text(size = 16)) 
