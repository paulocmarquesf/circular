# Synthetic - Projected normal

# rm(list = ls())

library(tidyverse)
library(Directional)

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

theme_set(theme_bw())

synth_polar <- ggplot(trn, aes(x = y, y = after_stat(density))) +
    geom_histogram(color = "white", alpha = 0.85,
                   binwidth = 2 * IQR(trn$y) / nrow(trn)^(1/3)) +
    scale_x_continuous(limits = c(0, 2*pi),
                       labels = c("0", "π/4", "π/2", "3π/4", "π", "5π/4", "3π/2", "7π/4"),
                       breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4),
                       oob = scales::oob_keep) +
    coord_polar(start = -pi/2, direction = -1) +
    labs(x = "", y = "", title = "Synthetic") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

synth_polar

# saveRDS(synth_polar, "synth_polar.rds")

###

alpha <- 0.1

mu_hat_trn <- spml.reg(trn$y, trn |> select(-y), xnew = trn |> select(-y))$est
mu_hat_cal <- spml.reg(trn$y, trn |> select(-y), xnew = cal |> select(-y))$est
mu_hat_tst <- spml.reg(trn$y, trn |> select(-y), xnew = tst |> select(-y))$est

mean(pi - abs(pi - abs(mu_hat_tst - tst$y)))

###

trn_res <- trn |> mutate(y = pi - abs(pi - abs(y - mu_hat_trn)))

sig_hat_cal <- spml.reg(trn_res$y, trn_res |> select(-y), xnew = cal |> select(-y))$est
sig_hat_tst <- spml.reg(trn_res$y, trn_res |> select(-y), xnew = tst |> select(-y))$est

###

R <- (pi - abs(pi - abs(cal$y - mu_hat_cal))) / sig_hat_cal

r_hat <- sort(R)[ceiling((1 - alpha)*(nrow(cal) + 1))]                 

lower <- ifelse(r_hat * sig_hat_tst >= pi, 0, mu_hat_tst - r_hat * sig_hat_tst)
upper <- ifelse(r_hat * sig_hat_tst >= pi, 2*pi, mu_hat_tst + r_hat * sig_hat_tst)

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

intervals_synth_pn <- tibble(id = 1:n_tst, y_tst_plot, mu_hat_tst, lower, upper) |>
head(50) |>
ggplot(mapping = aes(x = id)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray42") +
    geom_point(aes(y = y_tst_plot), color = "black", size = 1, alpha = 0.75) +
    scale_x_continuous(limits = c(0, 51), breaks = c(1, seq(10, 50, by = 10))) +
    scale_y_continuous(limits = c(-pi, 3*pi),
                       labels = c("-π", "0", "π", "2π", "3π"),
                       breaks = c(-pi, 0, pi, 2*pi, 3*pi)) +
    labs(x = "", y = "", title = "Projected normal")

intervals_synth_pn

# saveRDS(intervals_synth_pn, "intervals_synth_pn.rds")

###

median(upper - lower) # 3.3951

IQR(upper - lower) # 1.1981

mean(is_inside %in% c("YES", "MINUS_2PI_YES", "PLUS_2PI_YES")) # 0.904
