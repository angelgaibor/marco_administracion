rm(list = ls())

library(tidyverse)
library(rio)

correspondencia <- import("pedidos/011/correspondencia/CORRESPONDENCIA MIXTA 2010_2022.xlsx") |> 
  rename_all(tolower) |> 
  mutate(man_sec_10 = case_when(substr(man_loc_2010, 7, 9) == "999" ~ substr(man_loc_2010, 1, 12),
                                T ~ substr(man_loc_2010, 1, 14)),
         man_sec_22 = case_when(substr(man_loc_2022, 7, 9) == "999" ~ substr(man_loc_2022, 1, 12),
                                T ~ substr(man_loc_2022, 1, 15))) |> 
  group_by(man_sec_10, man_sec_22) |> 
  summarise() |> 
  ungroup()

carpeta <- last(list.dirs("insumos/00_viviendas/", full.names = F, recursive = F))

precenso <- import(paste0("insumos/00_viviendas/", carpeta, "/baseprecensal.rds"))

precenso_01 <- precenso |> 
  mutate(man_sec_22 = ifelse(zon == "999", paste0(pro, can, par, zon, sec),
                               paste0(pro, can, par, zon, sec, man)))

sum(!precenso_01$man_sec_22 %in% correspondencia$man_sec_22)

# archivo pp
man_sec_10_upm <- readRDS("pedidos/011/correspondencia_man_sec_10_id_upm.rds")

aux_cor <- correspondencia |> 
  filter(substr(man_sec_10, 7, 9) != "999") |>
  full_join(man_sec_10_upm |> 
              filter(substr(id_conglomerado, 7, 7) != "9") |> 
              group_by(id_conglomerado, man_sec_10, max_sub_con) |> 
              summarise() |> 
              ungroup(),
            by = "man_sec_10") |> 
  filter(!is.na(id_conglomerado)) |> 
  filter(substr(id_conglomerado, 1, 2) != "20") |> 
  mutate(control = ifelse(man_sec_10 %in% correspondencia$man_sec_10, 1, 0))

n_distinct(aux_cor$man_sec_22)

ocupadas <- precenso_01 |> 
  filter(c_ocup == "Particular - Ocupada") |> 
  # el número de viviendas ocupadas es de 4'595.701 en el precenso 2022
  left_join(aux_cor, by = "man_sec_22") |> 
  # el valor se incrementa a 4'808.577
  group_by(id_conglomerado, max_sub_con) |> 
  summarise(nviv = n()) |> 
  mutate(ncong = floor(nviv/30)) |> 
  filter(!is.na(id_conglomerado)) |> 
  ungroup()

n_distinct(ocupadas$id_conglomerado)
sum(ocupadas$ncong < ocupadas$max_sub_con)  

lol <- ocupadas |> filter(ncong < max_sub_con) |> 
  mutate(vivcon = nviv/max_sub_con)

man_conflicto <- precenso_01 |> 
  filter(c_ocup == "Particular - Ocupada") |> 
  # el número de viviendas ocupadas es de 4'595.701 en el precenso 2022
  left_join(aux_cor, by = "man_sec_22") |> 
  # el valor se incrementa a 4'808.577
  group_by(id_conglomerado, man_sec_22) |> 
  summarise(nvivman = n()) |> 
  ungroup() |> 
  group_by(id_conglomerado) |> 
  mutate(nvivcon = sum(nvivman)) |> 
  ungroup() |> 
  group_by(man_sec_22) |> 
  mutate(control = n()) |> 
  ungroup() |> 
  filter(control > 1)

conglomerado_man_sec_22 <- aux_cor |> 
  group_by(id_conglomerado, man_sec_22) |> 
  summarise()

set.seed(20241204)
man_conflicto <- precenso_01 |> 
  filter(c_ocup == "Particular - Ocupada") |> 
  # el número de viviendas ocupadas es de 4'595.701 en el precenso 2022
  left_join(conglomerado_man_sec_22, by = "man_sec_22") |> 
  # el valor se incrementa a 4'808.577
  group_by(id_conglomerado, man_sec_22) |> 
  summarise(nvivman = n()) |> 
  ungroup() |> 
  group_by(id_conglomerado) |> 
  mutate(nvivcon = sum(nvivman)) |> 
  ungroup() |> 
  group_by(man_sec_22) |> 
  mutate(control = n()) |> 
  ungroup() |> 
  filter(control > 1) |> 
  left_join(ocupadas |> 
              select(id_conglomerado, max_sub_con),
            by = "id_conglomerado") |> 
  mutate(aleatorio = sample(1:10000000, 3119)/10000000) |> 
  arrange(desc(nvivman), aleatorio)

index <- unique(man_conflicto$man_sec_22)

man_conflicto1 <- man_conflicto

apoyo1 <- vector("list", 0)
apoyo2 <- vector("list", 0)

for (i in 1:length(index)){
  apoyo <- man_conflicto |> 
    filter(man_sec_22 == index[i]) |> 
    arrange(nvivcon, aleatorio)
  apoyo1[[i]] <- apoyo |> 
    filter(row_number() == 1)
  apoyo2[[i]] <- apoyo |> 
    filter(row_number() != 1)
  
  a2 <- apoyo2[[i]] |> 
    group_by(id_conglomerado, nvivman1 = nvivman) |> 
    summarise() |> 
    ungroup()
  
  man_conflicto <- man_conflicto |> 
    left_join(a2, by = "id_conglomerado") |> 
    mutate(nvivman1 = ifelse(is.na(nvivman1), 0, nvivman1),
           nvivcon = nvivcon - nvivman1) |> 
    select(-nvivman1)
  
  print(i); 
  print(min(man_conflicto$nvivcon)); 
  if (min(man_conflicto$nvivcon) < 0) break
  
}

apoyo1 <- do.call(rbind, apoyo1)
apoyo2 <- do.call(rbind, apoyo2)

