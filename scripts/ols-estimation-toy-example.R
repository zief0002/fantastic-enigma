library(ggplot2)
library(dplyr)

## Toy data

toy = data.frame(
  x = c(30, 10, 30, 50, 20),
  y = c(63, 44, 40, 68, 25)
)



ggplot(data = toy, aes(x = x, y = y)) +
  geom_point(size = 4) +
  geom_abline(intercept = 28, slope = 0.8) + # Model A
  #geom_abline(intercept = 10, slope = 0.1, linetype = "dashed") + # Farcical model
  geom_abline(intercept = 20, slope = 1, linetype = "dashed") + # Model B
  theme_bw() +
  xlim(0, 75) +
  ylim(0, 75)



toy |>
  mutate(
    A_yhat = 28 + 0.8 * x,
    B_yhat = 20 + 1 * x,
    A_resid = y - A_yhat,
    B_resid = y - B_yhat,
    A_sq_resid = A_resid^2,
    B_sq_resid = B_resid^2
  ) |>
  summarize(
    A_SSE = sum(A_sq_resid),
    B_SSE = sum(B_sq_resid)
  )
