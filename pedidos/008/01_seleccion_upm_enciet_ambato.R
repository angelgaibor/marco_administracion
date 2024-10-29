rm(list = ls())

library(tidyverse)
library(rio)

upm_ambato <- c("1801500054", "1801500053", "1801500040",
                "1801500055", "1801500062", "1801500063",
                "1801500119", "1801500071", "1801500037")

man_sec_upm <- readRDS("D:/MAG/marco_administracion/insumos/01_general/man_sec_upm.rds")

set.seed(20241002)

muestra_pp <- man_sec_upm |> 
  filter(id_upm %in% upm_ambato) |> 
  mutate(id_upm = paste0(id_upm, "01"),
         pro = substr(man_sec, 1, 2),
         can = substr(man_sec, 3, 4),
         par = substr(man_sec, 5, 6),
         zon = substr(man_sec, 7, 9),
         sec = substr(man_sec, 10, 12),
         man = substr(man_sec, 13, 15)) |> 
  select(-viv_ocu)

pluscode_enlace <- readRDS("pedidos/005/edif_pluscode_enlace.rds")

precenso <- readRDS("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BASE PRECENSAL/baseprecensal.rds")

muestra_viv <- precenso |> 
  mutate(man_sec = ifelse(zon == "999", paste0(pro, can, par, zon, sec), paste0(pro, can, par, zon, sec, man))) |> 
  filter(man_sec %in% muestra_pp$man_sec, c_ocup == "Particular - Ocupada") |> 
  left_join(muestra_pp |> 
              select(man_sec, id_upm),
            by = "man_sec") |> 
  group_by(id_upm) |> 
  sample_n(8) |> 
  mutate(n_edif = str_pad(n_edif, 3, "left", "0"),
         n_viv = str_pad(n_viv, 3, "left", "0"),
         edif_cod = paste0(man_sec, str_pad(n_edif, 3, "left", "0"))) |> 
  left_join(pluscode_enlace |> 
              select(edif_cod, pluscodes, link),
            by = "edif_cod") |> 
  arrange(id_upm, edif_cod, n_viv) |> 
  group_by(id_upm) |> 
  mutate(orden = row_number()) |> 
  ungroup() |> 
  select(id_upm, pro, can, par, zon, sec, man, n_edif, n_viv, orden, link, pluscodes)

export(muestra_pp, "pedidos/008/muestra_man_sec_upm_ambato.xlsx")
export(muestra_viv, "pedidos/008/muestra_viv_ambato.xlsx")
