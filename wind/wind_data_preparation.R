# Wind direction - Data preparation

library(tidyverse)

# rm(list = ls())

# https://tempo.inmet.gov.br/TabelaEstacoes/A001

clean_up_names <- function(col_name) {
    col_name |>
        stringi::stri_trans_general("Latin-ASCII") |>
        str_replace_all(" \\(.*", "") |>
        str_to_lower() |>
        str_replace_all("\\W+", "_") |>
        str_replace_all("_$", "")
}

###

suppressWarnings(
    wind <- readxl::read_excel("Brasilia.xlsx") |>
                select(-Hora) |>
                rename_with(clean_up_names) |>
                mutate(data = date(data), hora = hms::as_hms(hora))
)

glimpse(wind)

summary(wind$data)

###

wind <- wind |>
    select(-data, -hora, -radiacao_global) |>
    mutate(across(everything(), ~ replace(.x, .x ==  -9999, NA))) |>
    fill(everything(), .direction = "downup") |>
    mutate(y = (vento_direcao_horaria * pi) / 180,
           cos_y_ant = cos(lag(y)),
           sin_y_ant = sin(lag(y)),
           precipitacao_ant = lag(precipitacao_total_horario),
           pressao_ant = lag(pressao_atmosferica_ao_nivel_da_estacao_horaria),
           temp_ar_bulbo_ant = lag(temperatura_do_ar_bulbo_seco_horaria),
           temp_orvalho_ant = lag(temperatura_do_ponto_de_orvalho),
           umidade_ant = lag(umidade_relativa_do_ar_horaria),
           rajada_maxima_ant = lag(vento_rajada_maxima),
           velocidade_ant = lag(vento_velocidade_horaria)) |>
    select(y, cos_y_ant, sin_y_ant,
           precipitacao_ant, pressao_ant,
           temp_ar_bulbo_ant, temp_orvalho_ant,
           umidade_ant, rajada_maxima_ant, velocidade_ant) |>
    slice(-1)

###

theme_set(theme_bw())

ggplot(wind, aes(x = y, y = after_stat(density))) +
    geom_histogram(color = "white", alpha = 0.85,
                   binwidth = 2 * IQR(wind$y) / nrow(wind)^(1/3)) +
    scale_x_continuous(limits = c(0, 2*pi),
                       labels = c("0", "π/4", "π/2", "3π/4", "π", "5π/4", "3π/2", "7π/4"),
                       breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4),
                       oob = scales::oob_keep) +
    coord_polar(start = -pi/2, direction = -1) +
    labs(x = "", y = "", title = "Wind") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

###

write.csv(wind, "wind.csv", row.names = FALSE)
