rm(list = ls())

library(tidyverse)

marco_2010 <- readRDS("pedidos/011/viviendas/marco_viviendas_20210318_V21.rds")

apoyo <- marco_2010 |> 
  mutate(man_loc = case_when(substr(id_viv, 15, 15) == "-" ~ substr(id_viv, 13, 14),
                             T ~ substr(id_viv, 15, 16)),
         man_sec_10 = paste0(substr(id_viv, 1, 12), man_loc),
         sub_con = as.numeric(substr(id_upm, 11, 12))) |> 
  group_by(id_conglomerado) |> 
  mutate(max_sub_con = max(sub_con)) |> 
  group_by(id_conglomerado, man_sec_10, max_sub_con) |> 
  summarise(n_viv = n()) |> 
  filter(substr(id_conglomerado, 1, 2) != "20") |> 
  group_by(man_sec_10) |> 
  mutate(control = n()) |> 
  ungroup() |> 
  arrange(man_sec_10, desc(n_viv)) |>
  group_by(man_sec_10) |> 
  summarise(id_conglomerado = first(id_conglomerado),
            max_sub_con = first(max_sub_con)) |> 
  ungroup()
  
n_distinct(apoyo$man_sec_10)

saveRDS(apoyo, "pedidos/011/correspondencia_man_sec_10_id_upm.rds")
