library(tidyverse)

rm(list = ls())

library(openxlsx)

# selección upm marco anonimizado

# base enighur tamanio

bdd <-read.xlsx("pedidos/030_ppt_espol/insumos/01_tamanio/base0_tamanio_enighur.xlsx") |> 
  filter(grepl("_p", dominio)) |> 
  select(provincia = nombre_dom, d1, sd, d1_deff, N, prom_hogares_upm, mer)

write.xlsx(bdd, "pedidos/030_ppt_espol/insumos/01_tamanio/bdd_ej3_enighur.xlsx")

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