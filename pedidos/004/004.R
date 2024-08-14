rm(list = ls())

library(tidyverse)
library(rio)

marco <- readRDS("productos/01_general/marco_upm_06.rds")

set.seed(20240814)

muestra <- marco |> 
  filter(substr(id_upm, 1, 4) == "0205",
         substr(id_upm, 1, 6) != "020551",
         substr(id_upm, 1, 6) != "020553",
         enighur_sel != 1,
         endi3_sel != 1,
         enciet_202410_sel != 1,
         enciet_202411_sel != 1,
         enciet_202412_sel != 1) |> 
  group_by(area) |> 
  sample_n(4)

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds") |> 
  filter(id_upm %in% substr(muestra$id_upm, 1, 10)) |> 
  mutate(pro = substr(man_sec, 1, 2),
         can = substr(man_sec, 3, 4),
         par = substr(man_sec, 5, 6),
         zon = substr(man_sec, 7, 9),
         sec = substr(man_sec, 10, 12),
         man = substr(man_sec, 13, 15))

export(man_sec_upm, "pedidos/004/muestra_prueba_piloto_enighur_0205.xlsx")
