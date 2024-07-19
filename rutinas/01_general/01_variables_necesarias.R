rm(list = ls())

library(tidyverse)

marco_upm <- readRDS("insumos/01_general/marco_upm.rds")

set.seed(20240618)

marco_01 <- marco_upm %>% 
  group_by(estrato) %>% 
  mutate(Mh = sum(Mi)) %>% 
  ungroup() %>% 
  mutate(nap = sample(1:10000000, 56239)/10000000,
         domgeo = substr(domest, 1, 2)) |> 
  select(id_upm, pro, area, domgeo, domest, estrato, Mi, nap)

# Se generó un error en la codificación de las UPM de Quito por lo que se corrige en el marco de upm y
# en el archivo de correspondencia id_upm mansec

apoyo <- marco_01 |> 
  filter(substr(id_upm, 1, 7) %in% c("1701509", "1701809")) |>
  group_by(id_par = substr(id_upm, 1, 6)) |> 
  mutate(cong = row_number()) |> 
  ungroup() |> 
  mutate(id_upm_cor = paste0(id_par, "9", str_pad(cong, 3, "left", "0"), "01")) 

marco_02 <- marco_01 |> 
  filter(!substr(id_upm, 1, 7) %in% c("1701509", "1701809")) |> 
  rbind(apoyo|> 
          select(id_upm = id_upm_cor, pro, area, domgeo, domest, estrato, Mi, nap)) |> 
  arrange(pro, estrato)

n_distinct(marco_02$id_upm) == dim(marco_02)[1]

saveRDS(marco_02, "productos/01_general/marco_upm_01.rds")

# man_sec_upm_final_dmq <- readRDS("D:/MAG/marco_estratificacion/productos/04_marco/marco.rds")
man_sec_upm_final_dmq <- readRDS("insumos/01_general/man_sec_upm.rds")

man_sec_upm_final_corregido <- man_sec_upm_final_dmq |>
  left_join(apoyo |> 
              mutate(id_upm = ifelse(id_par == "170150",
                                     substr(id_upm, 1, 11),
                                     substr(id_upm, 1, 10)),
                            id_upm_cor = substr(id_upm_cor, 1, 10)) |>
              group_by(id_upm, id_upm_cor) |> 
              summarise() |> 
              ungroup(), 
            by = "id_upm") |>
  mutate(id_upm_final = ifelse(!is.na(id_upm_cor), id_upm_cor, id_upm)) |>
  select(id_upm_final, man_sec, viv_ocu) |>
  rename(id_upm = id_upm_final)

table(nchar(man_sec_upm_final_corregido$id_upm))

n_distinct(man_sec_upm_final_corregido$id_upm) == n_distinct(substr(marco_02$id_upm, 1, 10))

saveRDS(man_sec_upm_final_corregido, "insumos/01_general/man_sec_upm.rds")
