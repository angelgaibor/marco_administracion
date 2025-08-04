rm(list = ls())

library(tidyverse)
library(rio)

resumen_distancia_manzana <- distancia_edificios <- readRDS("pedidos/019/resumen_distancia_manzana.rds")

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))
marco_upm <- readRDS(paste0("productos/01_general/",nombre))

conglomerados_partidos <- marco_upm |> 
  mutate(parte = as.numeric(substr(id_upm, 11, 12)),
         id_conglomerado = substr(id_upm, 1, 10)) |> 
  group_by(id_conglomerado) |> 
  summarise(partes_marco = max(parte),
            viv_pre = sum(Mi)) |> 
  filter(partes_marco > 1)

enlistamiento <- read.csv("insumos/00_viviendas/enighur/Base_muestral___20250623_095031.csv", 
                          sep = ";", colClasses = "character")

enlistamiento_01 <- enlistamiento |> 
  mutate(manloc = case_when(zon == "999" ~ n_loc,
                            T ~ man)) |> 
  filter(c_ocup %in% c("Ocupada con personas Ausentes", "Ocupada con personas Presentes")) |> 
  mutate(jefe_hogar = paste(primernjh, segundonjh, primerajh, segudonjh, sep = " ")) |> 
  select(pro, can, par, zon, sec, manloc, n_edif_umce = n_umce, piso_n, 
         n_viv, n_hbt = tot_hbt, jefe_hogar, calle = n_via_p, 
         id_upm = upm)


######### particioón conglomerados

marco_viviendas <- open_marco("insumos/00_viviendas/marco_viviendas")

edificio_upm <- marco_viviendas |> 
  filter(substr(id_upm, 1, 10) %in% conglomerados_partidos$id_conglomerado) |> 
  mutate(n_edif_umce = str_pad(n_edif_umce, 3, "left", "0"),
         id_edif_umce = paste0(pro, can, par, zon, sec, manloc,n_edif_umce)) |> 
  group_by(id_edif_umce, id_upm) |> 
  summarise(vivpre = n())

n_distinct(edificio_upm$id_edif_umce)

redistribucion <- enlistamiento_01 |> 
  filter(substr(id_upm, 1, 10) %in% conglomerados_partidos$id_conglomerado) |> 
  mutate(n_edif_umce = str_pad(n_edif_umce, 3, "left", "0"),
         id_edif_umce = paste0(pro, can, par, zon, sec, manloc,n_edif_umce),
         id_conglomerado = substr(id_upm, 1, 10)) |> 
  group_by(id_edif_umce, id_conglomerado) |> 
  summarise(vivenl = n()) |> 
  left_join(edificio_upm, by = "id_edif_umce")

n = dim(redistribucion)[1]

id_upm = redistribucion$id_upm
id_conglomerado = redistribucion$id_conglomerado

id_upm1 <- vector("character", n)
id_upm2 <- vector("character", n)

id_upm1[1] = id_upm[1]
id_upm1[n] = id_upm[n]

for (i in 2:n){
  if(!is.na(id_upm[i])){
    id_upm1[i] = id_upm[i]
  }
  if(!is.na(id_upm[n-i+1])){
    id_upm2[n-i+1] = id_upm[n-i+1]
  }
  if(is.na(id_upm[i])){
    id_upm1[i] = id_upm1[i-1]
  }
  if(is.na(id_upm[n-i+1])){
    id_upm2[n-i+1] = id_upm2[n-i+2]
  }
}

redistribucion_01 <- redistribucion |> 
  cbind(id_upm1 = id_upm1, id_upm2 = id_upm2) |> 
  mutate(id_upm3 = case_when(is.na(id_upm1) & is.na(id_upm2) ~ "revisar",
                             id_upm1 == id_upm2 & id_conglomerado == substr(id_upm1, 1, 10) ~ id_upm1,
                             is.na(id_upm1) & !is.na(id_upm2) & id_conglomerado == substr(id_upm2, 1, 10) ~ id_upm2,
                             is.na(id_upm2) & !is.na(id_upm1) & id_conglomerado == substr(id_upm1, 1, 10) ~ id_upm1,
                             !is.na(id_upm1) & !is.na(id_upm2) & id_conglomerado == substr(id_upm2, 1, 10) &  substr(id_upm2, 1, 10) != substr(id_upm1, 1, 10) ~ id_upm2,
                             !is.na(id_upm2) & !is.na(id_upm1) & id_conglomerado == substr(id_upm1, 1, 10) &  substr(id_upm2, 1, 10) != substr(id_upm1, 1, 10) ~ id_upm1,
                             !is.na(id_upm1) & !is.na(id_upm2) & id_upm1 != id_upm2 & id_conglomerado == substr(id_upm1, 1, 10) & id_conglomerado == substr(id_upm2, 1, 10) ~ "asignar",
                             is.na(id_upm1) & !is.na(id_upm2) & id_conglomerado != substr(id_upm2, 1, 10) ~ "revisar",
                             is.na(id_upm2) & !is.na(id_upm1) & id_conglomerado != substr(id_upm1, 1, 10) ~ "revisar",
                             T ~ "revisar"))

control_conglomerado <- redistribucion_01 |> 
  group_by(id_conglomerado) |> 
  summarise(nde = n(),
            revisar = sum(id_upm3 == "revisar"),
            asignar = sum(id_upm3 == "asignar"),
            vivenl = sum(vivenl),
            vivpre = sum(vivpre))

control_upm <- redistribucion_01 |> 
  group_by(id_conglomerado, id_upm3) |> 
  summarise(vivenl = sum(vivenl))

resumen_distancia_manzana_01 <- resumen_distancia_manzana |> 
  filter(id_man %in% substr(redistribucion_01$id_edif_umce, 1, 15))
