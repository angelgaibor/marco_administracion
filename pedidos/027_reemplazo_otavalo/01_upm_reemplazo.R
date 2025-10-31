rm(list = ls())

library(tidyverse)
library(rio)

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

upm_reem <- "100450901001"

estrato_reem <- marco$estrato[marco$id_upm == upm_reem]

apoyo <- marco |> 
  filter(endi3_estrato == estrato_reem) |> 
  mutate(total_sum = rowSums(across(ends_with("_sel")))) |> 
  filter(total_sum == 0) |> 
  arrange(endi3_orden) |>
  ungroup() |> 
  filter(row_number() == 1)

upm_new <- apoyo$id_upm

marco_01 <- marco |> 
  mutate(endi3_sel = case_when(id_upm == upm_reem ~ 0,
                              id_upm == upm_new ~ 1,
                              T ~ endi3_sel))

#### INCLUIR CUANDO SE EJECUTE EL CAMBIO

man_sec <- readRDS("insumos/01_general/man_sec_upm.rds") |> 
  filter(id_upm == substr(upm_new, 1, 10)) |> 
  mutate(id_upm = paste0(id_upm, "01")) |> 
  select(-viv_ocu)

export(man_sec, "pedidos/027_reemplazo_otavalo/upm_man_sec_reemplazo_otavalo.xlsx")
