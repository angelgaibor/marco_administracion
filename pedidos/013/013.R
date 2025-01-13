library(rio)
library(tidyverse)

marco_upm_08 <- readRDS("D:/MAG/marco_administracion/productos/01_general/marco_upm_08.rds")

upm_partir <- marco_upm_08 |> 
  mutate(id_conglomerado = substr(id_upm, 1, 10)) |> 
  group_by(id_conglomerado) |> 
  summarise(enighur_sel = max(enighur_sel),
            n_upm_con = n()) |> 
  filter(enighur_sel == 1, n_upm_con > 1)

export(upm_partir,"pedidos/013/upm_enighur_partir.xlsx")