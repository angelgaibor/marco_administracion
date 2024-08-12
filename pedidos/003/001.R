rm(list = ls())

library(tidyverse)

marco_upm_06 <- readRDS("productos/01_general/marco_upm_06.rds")

guayaquil <- marco_upm_06 |> 
  filter(substr(id_upm, 1, 4) == "0901") |> 
  mutate(area_dpa = ifelse(area == "1" & substr(id_upm, 5, 6) == "50", "1", "2"))

guayaquil_area_dpa <- guayaquil |> 
  group_by(area_dpa) |> 
  summarise(n_upm = n(),
            n_viv = sum(Mi)) |> 
  mutate(prop_upm = round(100*n_upm/sum(n_upm), 2),
         prop_viv = round(100*n_viv/sum(n_viv), 2)) |> 
  select(area_dpa, ends_with("upm"), ends_with("viv"))

write.table(guayaquil_area_dpa, "clipboard")

guayaquil_area_2000 <- guayaquil |> 
  group_by(area) |> 
  summarise(n_upm = n(),
            n_viv = sum(Mi)) |> 
  mutate(prop_upm = round(100*n_upm/sum(n_upm), 2),
         prop_viv = round(100*n_viv/sum(n_viv), 2)) |> 
  select(area, ends_with("upm"), ends_with("viv"))

write.table(guayaquil_area_2000, "clipboard", row.names = F)


riobamba <- marco_upm_06 |> 
  filter(substr(id_upm, 1, 4) == "0601") |> 
  mutate(area_dpa = ifelse(area == "1" & substr(id_upm, 5, 6) == "50", "1", "2"))

riobamba_area_dpa <- riobamba |> 
  group_by(area_dpa) |> 
  summarise(n_upm = n(),
            n_viv = sum(Mi)) |> 
  mutate(prop_upm = round(100*n_upm/sum(n_upm), 2),
         prop_viv = round(100*n_viv/sum(n_viv), 2)) |> 
  select(area_dpa, ends_with("upm"), ends_with("viv"))

write.table(riobamba_area_dpa, "clipboard")

riobamba_area_2000 <- riobamba |> 
  group_by(area) |> 
  summarise(n_upm = n(),
            n_viv = sum(Mi)) |> 
  mutate(prop_upm = round(100*n_upm/sum(n_upm), 2),
         prop_viv = round(100*n_viv/sum(n_viv), 2)) |> 
  select(area, ends_with("upm"), ends_with("viv"))

write.table(riobamba_area_2000, "clipboard", row.names = F)
