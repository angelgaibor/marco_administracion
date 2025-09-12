rm(list = ls())

library(tidyverse)
library(sf)

manzanas <- read_sf("../cartografia/insumos/precenso_2022/BNCPV22.gpkg", layer = "man_a")

sectores <- read_sf("../cartografia/insumos/precenso_2022/BNCPV22.gpkg", layer = "sec_a")

manzana_01 <- manzanas |> 
  filter(substr(man, 1, 4) == "1701") |> 
  select(man_sec = man)

sectores_01 <- sectores |> 
  filter(substr(sec, 1, 4) == "1701" & substr(sec, 7, 9) == "999") |> 
  select(man_sec = sec)

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds")

marco_upm <- readRDS("productos/01_general/marco_upm_24.rds")

auxiliar <- man_sec_upm |> 
  rename(id_conglomerado = id_upm) |> 
  full_join(marco_upm |> 
              group_by(id_conglomerado = substr(id_upm, 1, 10), estrato) |> 
              summarise() |> 
              ungroup(),
            by = "id_conglomerado") |> 
  filter(substr(id_conglomerado, 1, 4) == "1701") |> 
  mutate(area = substr(estrato, 3, 3),
         est = substr(estrato, 4, 4)) |> 
  select(man_sec, area, estrato = est)

dmq <- rbind(manzana_01, sectores_01) |> 
  left_join(auxiliar, by = "man_sec") |> 
  select(-geom)

write_sf(dmq, "pedidos/024_shape_quito_estrato/dmq_manzanas_sectores_area_estrato.gpkg")
