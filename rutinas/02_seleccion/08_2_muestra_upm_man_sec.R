rm(list = ls())

library(tidyverse)
library(rio)

muestra_upm <- readRDS("productos/02_seleccion/06_endi3/muestra_upm.rds")

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds")

muestra_man_sec_upm <- muestra_upm |> 
  mutate(id_upm10 = substr(id_upm, 1, 10)) |> 
  left_join(man_sec_upm |> 
              select(id_upm10 = id_upm, man_sec),
            by = "id_upm10") |> 
  mutate(pro = substr(man_sec, 1, 2),
         can = substr(man_sec, 3, 4),
         par = substr(man_sec, 5, 6),
         zon = substr(man_sec, 7, 9),
         sec = substr(man_sec, 10, 12),
         man = substr(man_sec, 13, 15)) |> 
  select(id_upm, man_sec, pro, can, par, zon, sec, man)

export(muestra_man_sec_upm, "productos/02_seleccion/06_endi3/muestra_man_sec_upm.xlsx")
