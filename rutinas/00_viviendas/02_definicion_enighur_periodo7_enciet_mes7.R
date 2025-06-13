rm(list = ls())

library(arrow)
library(tidyverse)
source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

# enighur
enighur01 <- readRDS("insumos/00_viviendas/enighur/periodo_01-07/periodo_01.rds")
enighur02 <- readRDS("insumos/00_viviendas/enighur/periodo_01-07/periodo_02.rds")
enighur03_07 <- readRDS("insumos/00_viviendas/enighur/periodo_01-07/periodo_03-07.rds")

enighur <- rbind(enighur01, enighur02, enighur03_07)

rm(enighur01, enighur02, enighur03_07)

enighur01 <- enighur |> 
  mutate(link = "") |> 
  select(pro, can, par, zon, sec, manloc = man_nloc, n_edif_umce = n_umce, piso_n, 
         n_viv, n_hbt = tot_hbt, jefe_hogar, calle = n_via_p, pluscodes, link,
         id_upm, nap = n_aleatorio, n = n_aleatorio_orden)

marco_nuevo <- open_marco("insumos/00_viviendas/marco_viviendas")

summary(enighur01)
summary(marco_nuevo)

lol <- rbind(enighur01 |> 
               mutate(cartografia = "enighur 2025"), 
             marco_nuevo |> 
               select(-man_sec) |> 
               mutate(cartografia = "precenso 2022"))
piso_raro <- lol |> filter(piso_n %in% as.character(99:40))

save_marco(".", lol)
