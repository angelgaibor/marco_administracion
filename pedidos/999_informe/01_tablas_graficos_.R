rm(list = ls())

library(tidyverse)

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

encuestas <- marco |> 
  select(ends_with("_sel")) |> 
  summarise_all(sum) |> 
  pivot_longer(cols = ends_with("_sel"), names_to = "encuesta", values_to = "upm_sel") |> 
  mutate(encuesta = substr(encuesta, 1, nchar(encuesta) - 4))

interseccion <- marco |> 
  select(id_upm, estrato, ends_with("_sel")) |> 
  mutate(control = rowSums(across(where(is.numeric)))) |> 
  filter(control > 1) |> 
  pivot_longer(cols = ends_with("_sel"), names_to = "encuesta", values_to = "sel") |> 
  filter(sel == 1) |> 
  mutate(encuesta = substr(encuesta, 1, nchar(encuesta) - 4)) |> 
  group_by(id_upm) |> 
  mutate(orden = row_number()) |> 
  ungroup() |> 
  pivot_wider(names_from = orden, names_prefix = "seleccion_", values_from = encuesta)

interseccion |> 
  group_by(seleccion_1, seleccion_2) |> 
  summarise(frecuencia = n())

lal <- interseccion |> 
  group_by(estrato, seleccion_1, seleccion_2) |> 
  summarise(frecuencia = n())

lol <- marco |> 
  select(id_upm, estrato, ends_with("_sel")) |> 
  mutate(control = rowSums(across(where(is.numeric)))) |> 
  group_by(estrato, control) |> 
  summarise(nupm = n()) |> 
  pivot_wider(names_from = control, names_prefix = "num_sel_", values_from = nupm, values_fill = 0)


