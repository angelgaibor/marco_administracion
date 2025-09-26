rm(list = ls())

library(tidyverse)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

resumen <- marco |> 
  select(id_upm, estrato, ends_with("_sel")) |>
  mutate(total = 1) |> 
  select(-id_upm) |> 
  group_by(estrato) |> 
  summarise_all(sum)

resumen01 <- resumen |> 
  cbind(utilizado = rowSums(resumen[, 2:15])) |> 
  mutate(disponible = total - utilizado)

analisis_estrato <- marco |> 
  select(id_upm, estrato, ends_with("_sel"))

analisis_estrato_01 <- analisis_estrato |> 
  cbind(utilizado = rowSums(analisis_estrato[, 3:16])) |> 
  filter(estrato == "3012") |> 
  mutate(utilizado = case_when(utilizado > 0 ~ 1,
                               T ~ 0))
