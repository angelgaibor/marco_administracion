rm(list = ls())

library(tidyverse)
library(sf)

source("rutinas/funciones/KerLin/ker_lin.R")
source("rutinas/funciones/zonificar.R")
ker_lin()

marco <- readRDS("pedidos/002/marco_viv_galap.rds")

# Introducir el número del mes a conglomerar la muestra
mes <- 1

muestra <- marco |> 
  filter(.data[[paste0("sel", mes)]] == 1)

l = 8

grupos <- muestra |> 
  mutate(canton = substr(id_man, 1, 4)) |> 
  group_by(canton) |> 
  zonificar(l) |> 
  ungroup()

saveRDS(grupos, "pedidos/002/conglomeracion_galapagos.gpkg")
# 
# edif <- read_sf("pedidos/002/BNCPV22_edif_p_20.gpkg")
# 
# edif_01 <- edif |> 
#   left_join(grupos |> 
#               select(id_edif, equipo),
#             by = c("edif_cod" = "id_edif")) |> 
#   filter(!is.na(equipo))
# 
# write_sf(edif_01, "pedidos/002/muestra_gal_mes1.gpkg")
