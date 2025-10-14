rm(list = ls())

library(tidyverse)
library(rio)
library(fs)
source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_respaldo/", full.names = TRUE)
file.remove(files_to_delete)

files_to_copy <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = FALSE)
file.copy(file.path("productos/00_viviendas/marco_viviendas_resultado/", files_to_copy),
          file.path("productos/00_viviendas/marco_viviendas_respaldo/", files_to_copy))

encuesta <- "steps"

carpeta <- last(list.dirs("insumos/00_viviendas/")[grepl(encuesta, list.dirs("insumos/00_viviendas/"))])

narchivo <- list.files(carpeta, pattern = ".rds")

muestra <- readRDS(paste0(carpeta, "/", narchivo))

muestra_01 <- muestra |> 
  mutate(id_viv_car = paste0(provincia, canton, parroquia, zona, sector, manzana, num_edif, num_viv),
         cod_cart = case_when(cartografia == "ACTUAL_ENIGHUR" ~ 2,
                               cartografia == "PRECENSO 2022" ~ 1,
                               T ~ 9))

table(nchar(muestra_01$id_viv_car))
table(muestra_01$cod_cart)

marco_old <- open_marco("productos/00_viviendas/marco_viviendas_respaldo/")

marco_new <- marco_old |> 
  select(-encuesta_enciet) |> 
  mutate(encuesta_steps = case_when(paste0(id_viv_car, cod_cart) %in% paste0(muestra_01$id_viv_car, muestra_01$cod_cart) ~ 1,
                                    T ~ 0),
         seleccion1 = case_when(seleccion == 0 & encuesta_steps == 1 ~ 1,
                               seleccion == 1 & encuesta_steps == 1 ~ 2,
                               T ~ seleccion),
         encuesta1 = case_when(encuesta_steps == 1 ~ "03_steps",
                               T ~ encuesta))

table(marco_new$encuesta_steps)
table(marco_new$seleccion1)
table(marco_new$encuesta1)
  
marco_new_01 <- marco_new |> 
  select(-seleccion, -encuesta, -encuesta_steps) |> 
  rename(seleccion = seleccion1, encueta = encuesta1)


files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = TRUE)
file.remove(files_to_delete)

save_marco("productos/00_viviendas/marco_viviendas_resultado/", marco_new_01)
