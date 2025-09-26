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



files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = TRUE)
file.remove(files_to_delete)

save_marco("productos/00_viviendas/marco_viviendas_resultado/", marco_viviendas_01)