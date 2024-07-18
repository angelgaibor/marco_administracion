rm(list = ls())

library(tidyverse)
library(rio)

par_excluir <- import("pedidos/001/informacion_excluir.xlsx", which = "parroquia")
zon_excluir <- import("pedidos/001/informacion_excluir.xlsx", which = "zona")
sec_excluir <- import("pedidos/001/informacion_excluir.xlsx", which = "sector")
upm_excluir <- import("pedidos/001/informacion_excluir.xlsx", which = "upm")

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds")

marco_upm <- readRDS("productos/01_general/marco_upm_01.rds") |> 
  mutate(id_conglomerado = substr(id_upm, 1, 10)) |> 
  group_by(id_upm = id_conglomerado, estrato, pro) |> 
  summarise() |> 
  ungroup()

apoyo_estrato <- man_sec_upm |> 
  mutate(id_par = substr(id_upm, 1, 6),
         id_zon = substr(man_sec, 1, 9),
         id_sec = substr(man_sec, 1, 12)) |> 
  left_join(par_excluir, by = "id_par") |> 
  left_join(zon_excluir, by = "id_zon") |> 
  left_join(sec_excluir, by = "id_sec") |> 
  left_join(upm_excluir, by = "id_upm") |> 
  mutate(tipo = case_when(!is.na(tipo_par) ~ tipo_par,
                          !is.na(tipo_zon) ~ tipo_zon,
                          !is.na(tipo_sec) ~ tipo_sec,
                          !is.na(tipo_upm) ~ tipo_upm,
                          T ~ "s"),
         tipo = case_when(tipo == "p" ~ "Peligrosidad",
                          tipo == "c" ~ "Costo",
                          tipo == "s" ~ "Seguro",
                          T ~ "lala")) |> 
  left_join(marco_upm, by = "id_upm") |> 
  mutate(dominio = substr(estrato, 1, 2)) |> 
  filter(dominio %in% c("14", "16", "22", "40")) |> 
  group_by(dominio, estrato, tipo) |> 
  summarise(viv = sum(viv_ocu)) |> 
  group_by(dominio, estrato) |> 
  mutate(prop = round(100 * viv/sum(viv), 2),
         n = n()) |> 
  ungroup() |> 
  arrange(dominio, estrato, tipo) |> 
  filter(n > 1) |> 
  select(-n)

apoyo_provincia <- man_sec_upm |> 
  mutate(id_par = substr(id_upm, 1, 6),
         id_zon = substr(man_sec, 1, 9),
         id_sec = substr(man_sec, 1, 12)) |> 
  left_join(par_excluir, by = "id_par") |> 
  left_join(zon_excluir, by = "id_zon") |> 
  left_join(sec_excluir, by = "id_sec") |> 
  left_join(upm_excluir, by = "id_upm") |> 
  mutate(tipo = case_when(!is.na(tipo_par) ~ tipo_par,
                          !is.na(tipo_zon) ~ tipo_zon,
                          !is.na(tipo_sec) ~ tipo_sec,
                          !is.na(tipo_upm) ~ tipo_upm,
                          T ~ "s"),
         tipo = case_when(tipo == "p" ~ "Peligrosidad",
                          tipo == "c" ~ "Costo",
                          tipo == "s" ~ "Seguro",
                          T ~ "lala")) |> 
  left_join(marco_upm, by = "id_upm") |> 
  mutate(dominio = substr(estrato, 1, 2)) |> 
  filter(dominio %in% c("14", "16", "22", "40")) |> 
  group_by(dominio, tipo) |> 
  summarise(viv = sum(viv_ocu)) |> 
  group_by(dominio) |> 
  mutate(prop = round(100 * viv/sum(viv), 2),
         n = n()) |> 
  ungroup() |> 
  arrange(dominio, tipo) |> 
  filter(n > 1) |> 
  select(-n)

export(list("provincia" = apoyo_provincia, "estrato" = apoyo_estrato),
       "pedidos/001/exclusion_pel_cos_pro_estrato.xlsx")
