rm(list = ls())

library(tidyverse)
library(TeachingSampling)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

# Fijamos el umbral de corte del tamanio de UPM

Mi_min <- 20

# Incluir el nombre de la encuesta para las variables
encuesta <- "enighur"

# Se abre el tamanio por estrato

tam_estrato <- readRDS("insumos/02_seleccion/01_enighur24/distribucion_enighur.rds") |> 
  rename(aux_estrato = estrato,
         aux_nh = nh)

marco_01 <- marco |> 
  #filter(pro != "20") |> 
  # Creamos la variable dominio de en función del corte
  mutate(aux_estrato = ifelse(Mi < Mi_min, "9999", estrato),
         aux_seleccionable = case_when(T ~ 1)) |> 
  # Se agrega la variable de tamanio por estrato
  left_join(tam_estrato, by = "aux_estrato") |> 
  # Se corrige el tamaño
  mutate(aux_nh = ifelse(is.na(aux_nh), 0,
                         aux_nh)) |> 
  group_by(aux_estrato) |> 
  mutate(aux_pii = PikPPS(aux_nh, Mi)) |> 
  ungroup() |> 
  mutate(aux_pareto = case_when(aux_pii == 1 ~ 0, 
                                aux_pii == 0 ~ 1000000,
                                T ~ (nap/(1-nap))/(aux_pii/(1-aux_pii)))) |> 
  arrange(aux_estrato, aux_seleccionable, aux_pareto) |> 
  group_by(aux_estrato) |> 
  mutate(aux_orden = row_number()) |> 
  ungroup() |> 
  mutate(aux_sel = ifelse(aux_orden <= aux_nh, 1, 0))

names(marco_01)[substr(names(marco_01), 1, 4) == "aux_"] <- gsub("aux", encuesta, 
                                                                 names(marco_01)[substr(names(marco_01), 1, 4) == "aux_"])

saveRDS(marco_01, paste0("productos/01_general/marco_upm_", 
                         str_pad(1 + as.numeric(substr(nombre, 11, 12)), 2, "left", "0"),
                         ".rds"))
