rm(list = ls())

library(tidyverse)

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

######
# Tratamiento extra al marco actual

marco_old <- open_marco("productos/00_viviendas/marco_viviendas_respaldo/")

marco_old1 <- marco_old |> 
  mutate(n_hbt = case_when(n_hbt == "0" | n_hbt == "" ~ "1",
                           T ~ n_hbt))
