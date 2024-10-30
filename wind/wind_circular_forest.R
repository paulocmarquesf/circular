# Circular forest - Wind direction

# rm(list = ls())

library(tidyverse)
library(Directional)
library(circtree)

db <- read_csv("wind.csv", show_col_types = FALSE)

set.seed(42)

db <- db[sample(1:nrow(db), size = round(0.2 * nrow(db)), replace = FALSE), ]

idx <- sample(1:3, size = nrow(db), prob = c(0.5, 0.25, 0.25), replace = TRUE)

trn <- db[idx == 1, ]
cal <- db[idx == 2, ]
tst <- db[idx == 3, ]

n_tst <- nrow(tst)

###

alpha <- 0.1

cf <- cforest(y ~ ., data = trn, trace = TRUE)

# saveRDS(cf, file = "cf_wind.rds")

# cf <- readRDS(file = "cf_wind.rds")

mu_hat_trn <- predict(cf, newdata = trn)
mu_hat_cal <- predict(cf, newdata = cal)
mu_hat_tst <- predict(cf, newdata = tst)

mean(pi - abs(pi - abs(mu_hat_tst - tst$y)))

# rm(cf); gc()

###

trn_res <- trn |> mutate(y = pi - abs(pi - abs(y - mu_hat_trn)))

cf_res <- cforest(y ~ ., data = trn_res, trace = TRUE)

# saveRDS(cf_res, file = "cf_res_wind.rds")

# cf_res <- readRDS(file = "cf_res_wind.rds")

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

intervals_wind_cf <- tibble(id = 1:n_tst, y_tst = tst$y, y_tst_plot, mu_hat_tst, lower, upper) |>
head(50) |> 
ggplot(mapping = aes(x = id)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray42") +
    geom_point(aes(y = y_tst_plot), color = "black", size = 1, alpha = 0.75) +
    scale_x_continuous(limits = c(0, 51), breaks = c(1, seq(10, 50, by = 10))) +
    scale_y_continuous(limits = c(-pi, 3*pi),
                       labels = c("-π", "0", "π", "2π", "3π"),
                       breaks = c(-pi, 0, pi, 2*pi, 3*pi)) +
    labs(x = "", y = "", title = "Circular forest")

intervals_wind_cf

# saveRDS(intervals_wind_cf, "intervals_wind_cf.rds")

###

median(upper - lower) # 2.2301

IQR(upper - lower) # 3.0362

mean(is_inside %in% c("YES", "MINUS_2PI_YES", "PLUS_2PI_YES")) # 0.9020
