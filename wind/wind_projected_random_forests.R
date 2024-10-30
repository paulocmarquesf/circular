# Projected random forests - Wind direction

# rm(list = ls())

library(tidyverse)
library(Directional)
library(ranger)

arc_tan <- function(C, S) ifelse(C < 0, atan(S/C) + pi, ifelse(S > 0, atan(S/C), atan(S/C) + 2*pi))

###

db <- read_csv("wind.csv", show_col_types = FALSE)

seed <- 42

set.seed(seed)

db <- db[sample(1:nrow(db), size = round(0.2 * nrow(db)), replace = FALSE), ]

idx <- sample(1:3, size = nrow(db), prob = c(0.5, 0.25, 0.25), replace = TRUE)

trn <- db[idx %in% c(1, 2), ]
tst <- db[idx == 3, ]

n_tst <- nrow(tst)

###

alpha <- 0.1

num_trees <- 10^3

trn1 <- trn |> mutate(y = cos(y))
trn2 <- trn |> mutate(y = sin(y))

rf1 <- ranger(y ~ ., data = trn1, keep.inbag = TRUE, num.trees = num_trees, seed = seed)
rf2 <- ranger(y ~ ., data = trn2, keep.inbag = TRUE, num.trees = num_trees, seed = seed)

out <- matrix(unlist(lapply(rf1$inbag.count, \(x) x == 0)), nrow = rf1$num.trees, byrow = TRUE)

mu_hat_trn <- arc_tan(predict(rf1, data = trn, predict.all = TRUE)$predictions |> 
		                    ( \(.x) sapply(1:nrow(trn), \(.i) mean(.x[.i, out[, .i]])) )(),
                      predict(rf2, data = trn, predict.all = TRUE)$predictions |> 
		                    ( \(.x) sapply(1:nrow(trn), \(.i) mean(.x[.i, out[, .i]])) )())

mu_hat_tst <- arc_tan(predict(rf1, data = tst)$predictions,
                      predict(rf2, data = tst)$predictions)

mean(pi - abs(pi - abs(mu_hat_tst - tst$y)))

###

trn_res <- trn |> mutate(y = pi - abs(pi - abs(y - mu_hat_trn)))

trn1_res <- trn_res |> mutate(y = cos(y))
trn2_res <- trn_res |> mutate(y = sin(y))

rf1_res <- ranger(y ~ ., data = trn1_res, keep.inbag = TRUE, num.trees = num_trees, seed = seed)
rf2_res <- ranger(y ~ ., data = trn2_res, keep.inbag = TRUE, num.trees = num_trees, seed = seed)

sig_hat_trn <- arc_tan(predict(rf1_res, data = trn2_res, predict.all = TRUE)$predictions |> 
		                     ( \(.x) sapply(1:nrow(trn2_res), \(.i) mean(.x[.i, out[, .i]])) )(),
                       predict(rf2_res, data = trn1_res, predict.all = TRUE)$predictions |> 
		                     ( \(.x) sapply(1:nrow(trn1_res), \(.i) mean(.x[.i, out[, .i]])) )())

sig_hat_tst <- arc_tan(predict(rf1_res, data = tst)$predictions,
                       predict(rf2_res, data = tst)$predictions)

###

R <- (pi - abs(pi - abs(trn$y - mu_hat_trn))) / sig_hat_trn

r_hat <- sort(R)[ceiling((1 - alpha)*(nrow(trn) + 1))]

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

intervals_wind_prf <- tibble(id = 1:n_tst, y_tst = tst$y, y_tst_plot, mu_hat_tst, lower, upper) |>
head(50) |> 
ggplot(mapping = aes(x = id)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray42") +
    geom_point(aes(y = y_tst_plot), color = "black", size = 1, alpha = 0.75) +
    scale_x_continuous(limits = c(0, 51), breaks = c(1, seq(10, 50, by = 10))) +
    scale_y_continuous(limits = c(-pi, 3*pi),
                       labels = c("-π", "0", "π", "2π", "3π"),
                       breaks = c(-pi, 0, pi, 2*pi, 3*pi)) +
    labs(x = "", y = "", title = "Projected random forests")

intervals_wind_prf

# saveRDS(intervals_wind_prf, "intervals_wind_prf.rds")

###

median(upper - lower) # 1.8966

IQR(upper - lower) # 1.8737

mean(is_inside %in% c("YES", "MINUS_2PI_YES", "PLUS_2PI_YES")) # 0.8950
