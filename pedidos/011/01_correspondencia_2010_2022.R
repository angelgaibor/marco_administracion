rm(list = ls())

library(tidyverse)
library(rio)

# Se genera la correspondencia entre man_sec 2010 y 2022
man_sec_10_22 <- import("pedidos/011/correspondencia/CORRESPONDENCIA MIXTA 2010_2022.xlsx") |> 
  rename_all(tolower) |> 
  mutate(man_sec_10 = case_when(substr(man_loc_2010, 7, 9) == "999" ~ substr(man_loc_2010, 1, 12),
                                T ~ substr(man_loc_2010, 1, 14)),
         man_sec_22 = case_when(substr(man_loc_2022, 7, 9) == "999" ~ substr(man_loc_2022, 1, 12),
                                T ~ substr(man_loc_2022, 1, 15))) |> 
  group_by(man_sec_10, man_sec_22) |> 
  summarise() |> 
  ungroup()

# archivo correspondencia man_sec 2010 y conglomerado 
man_sec_10_conglomerado <- readRDS("pedidos/011/correspondencia_man_sec_10_id_upm.rds")

# se genera la correspondencia entre man_sec 2022 y conglomerado primera verisión
man_sec_22_conglomerado <- man_sec_10_22 |> 
  left_join(man_sec_10_conglomerado |> 
              group_by(id_conglomerado, man_sec_10) |> 
              summarise(),
            by = "man_sec_10") |> 
  group_by(id_conglomerado, man_sec_22) |> 
  summarise() |> 
  ungroup()

n_distinct(man_sec_22_conglomerado$id_conglomerado)
n_distinct(man_sec_22_conglomerado$man_sec_22)

# la correspondencia entre man_sec 2022 y conglomerado debe ser única

# abrimos el precenso
carpeta <- last(list.dirs("insumos/00_viviendas/", full.names = F, recursive = F))

precenso <- import(paste0("insumos/00_viviendas/", carpeta, "/baseprecensal.rds"))

precenso_01 <- precenso |> 
  mutate(man_sec_22 = ifelse(zon == "999", paste0(pro, can, par, zon, sec),
                             paste0(pro, can, par, zon, sec, man)))

set.seed(20241204)
man_conflicto <- precenso_01 |> 
  filter(c_ocup == "Particular - Ocupada") |> 
  # el número de viviendas ocupadas es de 4'595.701 en el precenso 2022
  full_join(man_sec_22_conglomerado, by = "man_sec_22") |> 
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
  filter(control > 1) |> # Actualizar numero de manzanas
  mutate(aleatorio = sample(1:10000000, 8415)/10000000) |> 
  arrange(desc(nvivman), aleatorio)

index <- unique(man_conflicto$man_sec_22)

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

man_sec_22_conglomerado_01 <- man_sec_22_conglomerado |> 
  filter(!paste0(id_conglomerado, man_sec_22) %in% paste0(apoyo2$id_conglomerado, apoyo2$man_sec_22)) |> 
  filter(!is.na(man_sec_22)) |> 
  filter(!is.na(id_conglomerado)) |> 
  group_by(man_sec_22) |> 
  mutate(n = n())

n_distinct(man_sec_22_conglomerado_01$man_sec_22)
n_distinct(man_sec_22_conglomerado_01$id_conglomerado)
table(man_sec_22_conglomerado_01$n)

marco_upm10_pre22 <- precenso_01 |> 
  filter(c_ocup == "Particular - Ocupada") |> 
  left_join(man_sec_22_conglomerado_01 |> 
              select(man_sec_22, id_conglomerado), by = "man_sec_22") |> 
  filter(!is.na(id_conglomerado)) |> 
  left_join(man_sec_10_conglomerado |> 
              group_by(id_conglomerado, ncon = max_sub_con) |> 
              summarise(),
            by = "id_conglomerado") |> 
  arrange(id_conglomerado, man_sec_22, n_loc, n_edif, n_viv) |> 
  group_by(id_conglomerado) |> 
  mutate(nviv = n(),
         orden1 = row_number() - 1) |> 
  ungroup() |> 
  arrange(id_conglomerado, desc(orden1)) |> 
  group_by(id_conglomerado) |> 
  mutate(orden2 = row_number() - 1) |> 
  ungroup() |> 
  arrange(id_conglomerado, orden1) |> 
  mutate(tamg = floor(nviv/ncon),
         residuo = nviv %% tamg,
         subcon = case_when(residuo > 0 & floor(orden1/(tamg + 1)) <= (residuo - 1) ~ 1 + floor(orden1/(tamg + 1)),
                            residuo > 0 & floor(orden1/(tamg + 1)) > (residuo - 1) ~ ncon - floor(orden2/(tamg)),
                            T ~ 1 + floor(orden1/(tamg))),
         id_upm = paste0(id_conglomerado, str_pad(subcon, 2, "left", "0"))) |> 
  select(-man_sec_22, -id_conglomerado, -ncon, -nviv, -orden1, -orden2, -tamg,
         -residuo, -subcon)

validacion <- marco_upm10_pre22 |> 
  group_by(id_conglomerado = substr(id_upm, 1, 10), subcon = substr(id_upm, 11, 12)) |> 
  summarise(n = n()) |> 
  group_by(id_conglomerado) |> 
  summarise(nmax = max(n),
            nmin = min(n)) |> 
  mutate(control = nmax - nmin)
table(validacion$control)

saveRDS(marco_upm10_pre22, "pedidos/011/marco2010_actualizado_precenso22.rds")
