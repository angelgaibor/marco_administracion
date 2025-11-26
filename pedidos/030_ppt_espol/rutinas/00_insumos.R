library(tidyverse)

rm(list = ls())

# selección upm marco anonimizado

marco <- readRDS("productos/01_general/marco_upm_01.rds")

marco_01 <- marco |> 
  select(id_upm, pro, estrato, Mi) |> 
  arrange(id_upm) |> 
  group_by(pro) |> 
  mutate(cong_aux = row_number()) |> 
  ungroup() |> 
  mutate(id_upm = str_pad(cong_aux, 6, "left", "0")) |> 
  select(-cong_aux)

saveRDS(marco_01, "pedidos/030_ppt_espol/insumos/02_seleccion_upm/marco_upm.rds")

# 