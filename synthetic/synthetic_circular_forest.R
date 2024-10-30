# Synthetic - Circular forest

# rm(list = ls())

library(tidyverse)
library(Directional)
library(circtree)

synthetic <- function(n, d = 5) {
    X <- matrix(runif(n*d, min = -1, max = 1), nrow = n)
    g <- function(x) 2*atan(x[1] - 2*x[2] + x[1]*x[2] - 2*x[3]^2)
    y <- apply(X, 1, function(.x) as.numeric(rvonmises(1, m = g(.x) + pi, k = 5)))
    data.frame(X, y)
}

###

set.seed(42)

alpha <- 0.1

n_tst <- 10^4

d <- 10

trn <- synthetic(10^4, d)
cal <- synthetic(10^4, d)
tst <- synthetic(n_tst, d)

###

alpha <- 0.1

cf <- cforest(y ~ ., data = trn, trace = TRUE)

# saveRDS(cf, file = "cf_synth.rds")

# cf <- readRDS(file = "cf_synth.rds")

mu_hat_trn <- predict(cf, newdata = trn)
mu_hat_cal <- predict(cf, newdata = cal)
mu_hat_tst <- predict(cf, newdata = tst)

mean(pi - abs(pi - abs(mu_hat_tst - tst$y)))

# rm(cf); gc()

###

trn_res <- trn |> mutate(y = pi - abs(pi - abs(y - mu_hat_trn)))

cf_res <- cforest(y ~ ., data = trn_res, trace = TRUE)

# saveRDS(cf_res, file = "cf_res_synth.rds")

# cf_res <- readRDS(file = "cf_res_synth.rds")

sig_hat_cal <- predict(cf_res, newdata = cal)
sig_hat_tst <- predict(cf_res, newdata = tst)

# rm(cf_res); gc()

###

R <- (pi - abs(pi - abs(cal$y - mu_hat_cal))) / sig_hat_cal

r_hat <- sort(R)[ceiling((1 - alpha)*(nrow(cal) + 1))]                 

lower <- ifelse(r_hat * sig_hat_tst  >= pi, 0, mu_hat_tst - r_hat * sig_hat_tst)
upper <- ifelse(r_hat * sig_hat_tst  >= pi, 2*pi, mu_hat_tst + r_hat * sig_hat_tst)

###

is_inside <- case_when(
    lower <= tst$y & tst$y <= upper ~ "YES",
    lower <= tst$y - 2*pi & tst$y - 2*pi <= upper ~ "MINUS_2PI_YES",
    lower <= tst$y + 2*pi & tst$y + 2*pi <= upper ~ "PLUS_2PI_YES",
    .default = "NO"
)

y_tst_plot <- tst$y + case_when(is_inside == "MINUS_2PI_YES" ~ -2*pi,
                                is_inside == "PLUS_2PI_YES" ~ 2*pi,
                                .default = 0)

theme_set(theme_bw())

intervals_synth_cf <- tibble(id = 1:n_tst, y_tst = tst$y, y_tst_plot, mu_hat_tst, lower, upper) |>
head(50) |> 
ggplot(mapping = aes(x = id)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray42") +
    geom_point(aes(y = y_tst_plot), color = "black", size = 1, alpha = 0.75) +
    scale_x_continuous(limits = c(0, 51), breaks = c(1, seq(10, 50, by = 10))) +
    scale_y_continuous(limits = c(-pi, 3*pi),
                       labels = c("-π", "0", "π", "2π", "3π"),
                       breaks = c(-pi, 0, pi, 2*pi, 3*pi)) +
    labs(x = "", y = "", title = "Circular forest")

intervals_synth_cf

# saveRDS(intervals_synth_cf, "intervals_synth_cf.rds")

###

median(upper - lower) # 2.0893

IQR(upper - lower) # 0.4782

mean(is_inside %in% c("YES", "MINUS_2PI_YES", "PLUS_2PI_YES")) # 0.8988
