rm(list = ls())

library(arrow)
library(tidyverse)
library(rio)
library(fs)
source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

round(100000000 * runif(1))
#set.seed(20250910) # septiembre
# set.seed(5258168) # octubre
set.seed(73639480) # noviembre

files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_respaldo/", full.names = TRUE)
file.remove(files_to_delete)

files_to_copy <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = FALSE)
file.copy(file.path("productos/00_viviendas/marco_viviendas_resultado/", files_to_copy),
          file.path("productos/00_viviendas/marco_viviendas_respaldo/", files_to_copy))

######
# Tratamiento extra al marco actual

marco_old <- open_marco("productos/00_viviendas/marco_viviendas_respaldo/")

muestra_enciet <- readRDS("insumos/00_viviendas/enciet/muestra_acumulada_marco_viviendas.rds")

marco_new <- marco_old |> 
  filter(pro != "20") |> 
  left_join(muestra_enciet |> 
              mutate(orden_javi = as.numeric(vivienda)) |> 
              select(encuesta_enciet = encuesta, cod_cart, id_viv_car, orden_javi), 
            by = c("id_viv_car", "cod_cart")) |> 
  mutate(orden = case_when(!is.na(orden_javi) ~ orden_javi,
                           T ~ orden)) |> 
  arrange(id_upm, cod_cart, desc(seleccion), orden) |> 
  group_by(id_upm, cod_cart) |> 
  mutate(ordenf = row_number()) |> 
  select(pro, can, par, zon, sec, manloc, n_edif_umce, piso_n, n_viv, n_hbt,
         calle, n_pm, id_upm, id_viv_car, cod_cart, pluscodes, nap, 
         orden = ordenf, seleccion, encuesta, link, jefehogar)
  

control <- marco_new |> 
  group_by(id_upm, cod_cart) |> 
  summarise(suma = sum(orden * seleccion),
            maximosel = max(orden * seleccion),
            maximo = max(orden),
            total = n()) |> 
  ungroup() |> 
  filter(suma != maximosel * (maximosel + 1 )/2 | maximo != total)

files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = TRUE)
file.remove(files_to_delete)

save_marco("productos/00_viviendas/marco_viviendas_resultado/", marco_new)
