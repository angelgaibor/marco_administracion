rm(list = ls())

library(tidyverse)
library(rio)
library(sf)

#periodo 1
puntos11 <- read_sf("pedidos/019/GPKG/001/enighur_j1.gpkg", layer = "edif_p")
puntos12 <- read_sf("pedidos/019/GPKG/001/enighur_j2.gpkg", layer = "edif_p")

puntos1 <- rbind(puntos11 |>
                   filter(!duplicated(edif_cod)) |> 
                   select(edif_cod),
                 puntos12 |>
                   filter(!duplicated(edif_cod)) |> 
                   select(edif_cod)) |> 
  filter(!duplicated(edif_cod)) |> 
  mutate(periodo = 1)

table(nchar(puntos1$edif_cod))

#periodos 2 - 9

index <- list.dirs("pedidos/019/GPKG/", recursive = F)
apoyo <- vector("list", 0)
apoyo[[1]] <- puntos1
for(i in 2:length(index)){
  nombre <- list.files(index[i])
  apoyo[[i]] <- read_sf(paste0(index[i], "/", nombre), layer = "edif_p") |> 
    filter(!duplicated(edif_cod)) |> 
    select(edif_cod) |> 
    mutate(periodo = i)
}

puntos_enighur <- do.call("rbind",apoyo)

puntos_precenso <- read_sf("../cartografia/insumos/precenso_2022/BNCPV22.gpkg", layer = "edif_p") |> 
  filter(!duplicated(edif_cod)) |> 
  select(edif_cod)

puntos_precenso_01 <- puntos_precenso |> 
  filter(edif_cod %in% puntos_enighur$edif_cod) |> 
  arrange(edif_cod) |> 
  st_zm(drop = T, what = "ZM")

puntos_enighur_01 <- puntos_enighur |> 
  filter(edif_cod %in% puntos_precenso_01$edif_cod) |> 
  filter(!duplicated(edif_cod)) |> 
  arrange(edif_cod) |> 
  st_zm(drop = T, what = "ZM")


distancia <- vector("numeric", 0)

for(i in 1:length(puntos_enighur_01$edif_cod)){
  distancia[i] <- st_distance(puntos_enighur_01[i,],
                              puntos_precenso_01[i,])
  print(i)
}

distancia <- readRDS("pedidos/019/distancia.rds")

arreglo <- data.frame(edif_cod = puntos_enighur_01$edif_cod, distancia = distancia[1:220600]) |> 
  filter(substr(edif_cod, 7, 9) != "999")

arreglo_manzana <- arreglo |> 
  group_by(id_man = substr(edif_cod, 1, 15)) |> 
  summarise(media_distancia = mean(distancia),
            sd_distancia = sd(distancia),
            cv_distancia = round(100 * sd_distancia/media_distancia, 1),
            n_edif = n()) |> 
  filter(n_edif > 10)

saveRDS(distancia, "distancia.rds")

saveRDS(arreglo, "pedidos/019/distancia_edificios.rds")
saveRDS(arreglo_manzana, "pedidos/019/resumen_distancia_manzana.rds")
