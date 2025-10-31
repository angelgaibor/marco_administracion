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

saveRDS(marco_01, paste0("productos/01_general/marco_upm_", 
                         str_pad(1 + as.numeric(substr(nombre, 11, 12)), 2, "left", "0"),
                         ".rds"))
