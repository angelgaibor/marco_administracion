rm(list = ls())

library(tidyverse)
library(rio)

marco <- readRDS("productos/01_general/marco_upm_08.rds")

set.seed(20240906)

muestra <- marco |> 
  filter(substr(id_upm, 1, 6) %in% c("100550", "160150", "140150", "230150"),
         enighur_sel != 1,
         endi3_sel != 1,
         endi6_sel != 1,
         endi7_sel != 1,
         enciet_202410_sel != 1,
         enciet_202411_sel != 1,
         enciet_202412_sel != 1) |> 
  mutate(amadis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama"),
         parroquia = substr(id_upm, 1, 6),
         nh = case_when(parroquia == "140150" ~ 2,
                        T ~ 1)) |> 
  group_by(parroquia, amadis) |> 
  sample_n(unique(nh)) |> 
  ungroup()

# se solicita el cambio de UPM de Santo Domingop por 2 UPM de Guayaquil 

set.seed(20240910)

muestra_01 <- marco |> 
  filter(substr(id_upm, 1, 6) %in% c("090150"),
         enighur_sel != 1,
         endi3_sel != 1,
         endi6_sel != 1,
         endi7_sel != 1,
         enciet_202410_sel != 1,
         enciet_202411_sel != 1,
         enciet_202412_sel != 1) |> 
  mutate(amadis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama"),
         parroquia = substr(id_upm, 1, 6), 
         nh = 2) |> 
  filter(amadis == "ama") |> 
  group_by(parroquia, amadis) |> 
  sample_n(unique(nh)) |> 
  ungroup()

# se solicita el cambio de UPM de Santo Domingop por 2 UPM de Guayaquil 

#set.seed(20240911)

muestra_011 <- marco |> 
  filter(substr(id_upm, 1, 6) %in% c("090150"),
         enighur_sel != 1,
         endi3_sel != 1,
         endi6_sel != 1,
         endi7_sel != 1,
         enciet_202410_sel != 1,
         enciet_202411_sel != 1,
         enciet_202412_sel != 1,
         !id_upm %in% muestra_01$id_upm) |> 
  mutate(amadis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama"),
         parroquia = substr(id_upm, 1, 6), 
         nh = 1) |> 
  filter(amadis == "ama") |> 
  group_by(parroquia, amadis) |> 
  sample_n(unique(nh)) |> 
  ungroup()

muestra_02 <- muestra |> 
  filter(parroquia != "230150") |> 
  rbind(muestra_01 |> 
          filter(id_upm != "090150218801")) |> 
  rbind(muestra_011)

colSums(muestra |> select(ends_with("_sel")))

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds") |> 
  filter(id_upm %in% substr(muestra_02$id_upm, 1, 10)) |> 
  mutate(pro = substr(man_sec, 1, 2),
         can = substr(man_sec, 3, 4),
         par = substr(man_sec, 5, 6),
         zon = substr(man_sec, 7, 9),
         sec = substr(man_sec, 10, 12),
         man = substr(man_sec, 13, 15),
         id_conglomerado = paste0(id_upm, "01")) |> 
  left_join(muestra_02 |> 
              mutate(id_upm = substr(id_upm, 1, 10)) |> 
              select(id_upm, estrato),
            by = "id_upm") |> 
  select(id_upm, id_conglomerado, estrato, man_sec,pro, can, par, zon, sec, man)

export(man_sec_upm, "pedidos/006/muestra_prueba_piloto_endi.xlsx")
