rm(list = ls())

library(arrow)
library(tidyverse)
library(rio)
source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

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
  
enlistamiento <- read.csv("insumos/00_viviendas/enighur/Base_muestral___20250724_140509.csv", 
                                  sep = ";", colClasses = "character", encoding = "latin1")

enlistamiento_01 <- enlistamiento |> 
  mutate(manloc = case_when(zon == "999" ~ n_loc,
                            T ~ man)) |> 
  filter(c_ocup %in% c("Ocupada con personas Ausentes", "Ocupada con personas Presentes", "Rechazo")) |> 
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
  summarise(vivpre = n()) |> 
  ungroup()

n_distinct(edificio_upm$id_edif_umce)

redistribucion <- enlistamiento_01 |> 
  filter(substr(id_upm, 1, 10) %in% conglomerados_partidos$id_conglomerado) |> 
  mutate(n_edif_umce = str_pad(n_edif_umce, 3, "left", "0"),
         id_edif_umce = paste0(pro, can, par, zon, sec, manloc,n_edif_umce),
         id_conglomerado = substr(id_upm, 1, 10)) |> 
  group_by(id_conglomerado) |>
  mutate(orden = row_number()) |> 
  ungroup() |> 
  group_by(id_conglomerado, id_edif_umce) |> 
  summarise(vivenl = n(),
            orden = min(orden)) |>
  ungroup() |> 
  arrange(id_conglomerado, orden) |> 
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
  mutate(id_upm3 = case_when(# Casos genearles
    is.na(id_upm1) & is.na(id_upm2) ~ "revisar",
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
  summarise(vivenl = sum(vivenl)) |>
  ungroup() |> 
  group_by(id_conglomerado) |> 
  mutate(n_partes_eni = n()) |> 
  ungroup() |> 
  left_join(conglomerados_partidos |> 
              select(-viv_pre), by = "id_conglomerado") |> 
  mutate(control = case_when(n_partes_eni != partes_marco ~ 1,
                             T ~ 0))

hist(control_upm$vivenl[control_upm$control==0], breaks = 20)

control_upm |> 
  filter(control == 0) |> 
  group_by(id_conglomerado) |> 
  filter(min(vivenl) < 60) 

control_upm_revisar <- control_upm |> 
  filter(control == 1)

redistribucion_02 <- redistribucion_01 |> 
  mutate(id_upm4 = case_when(# Casos particulares
    id_edif_umce == "010150024009001286" ~ "010150007102",
    substr(id_edif_umce, 1, 15) == "080150015003002" ~ "080150018101",
    substr(id_edif_umce, 1, 15) == "080150015003003" & orden < 92 ~ "080150018101",
    substr(id_edif_umce, 1, 15) == "080150015003003" & orden >= 92 ~ "080150018102",
    id_conglomerado == "1101500063" & orden < 131 ~ "110150006301",
    id_conglomerado == "1101500063" & orden >= 131 ~ "110150006302",
    id_edif_umce == "120750005011001114" ~ "120750006402",
    id_edif_umce == "130850013008001226" ~ "130850021602",
    id_edif_umce == "130850013008001227" ~ "130850021602",
    id_edif_umce == "170150105007001130" ~ "170150191002",
    id_edif_umce == "170150105007001131" ~ "170150191002",
    id_edif_umce == "170150105007001133" ~ "170150191002",
    id_edif_umce == "170150071001001155" ~ "170150315902",
    id_edif_umce == "170150071001001156" ~ "170150315902",
    id_edif_umce == "170150071001001157" ~ "170150315902",
    id_edif_umce == "170150071001001160" ~ "170150315902",
    id_edif_umce == "170150071001001161" ~ "170150315902",
    id_edif_umce == "170150071001001162" ~ "170150315902",
    id_edif_umce == "170150072003001257" ~ "170150328902",
    id_edif_umce == "170150450002001247" ~ "170150539002",
    id_edif_umce == "170150450002001248" ~ "170150539002",
    id_edif_umce == "170155026002001469" ~ "170155039401",
    id_edif_umce == "170155026002001226" ~ "170155039403",
    id_edif_umce == "170155026002001536" ~ "170155039403",
    id_edif_umce == "170155026002001538" ~ "170155039403",
    id_edif_umce == "170155026002001547" ~ "170155039403",
    id_edif_umce == "170155026002001548" ~ "170155039403",
    id_edif_umce == "170155040004001199" ~ "170155050101",
    id_edif_umce == "170156022011001197" ~ "170156025103",
    id_edif_umce == "170550015001003023" ~ "170550020901",
    id_edif_umce == "170550015001003024" ~ "170550020901",
    id_edif_umce == "170550015001001119" ~ "170550020902",
    substr(id_edif_umce, 1, 15) == "180150029004003" ~ "180150034201",
    substr(id_edif_umce, 1, 15) == "180150029004004" ~ "180150034202",
    T ~ id_upm3))


control_conglomerado <- redistribucion_02 |> 
  group_by(id_conglomerado) |> 
  summarise(nde = n(),
            revisar = sum(id_upm4 == "revisar"),
            asignar = sum(id_upm4 == "asignar"),
            vivenl = sum(vivenl),
            vivpre = sum(vivpre))

control_upm <- redistribucion_02 |> 
  group_by(id_conglomerado, id_upm4) |> 
  summarise(vivenl = sum(vivenl)) |>
  ungroup() |> 
  group_by(id_conglomerado) |> 
  mutate(n_partes_eni = n()) |> 
  ungroup() |> 
  left_join(conglomerados_partidos |> 
              select(-viv_pre), by = "id_conglomerado") |> 
  mutate(control = case_when(n_partes_eni != partes_marco ~ 1,
                             T ~ 0))

hist(control_upm$vivenl[control_upm$control==0], breaks = 20)

control_upm |> 
  filter(control == 0) |> 
  group_by(id_conglomerado) |> 
  filter(min(vivenl) < 60) 

control_upm_revisar <- control_upm |> 
  filter(control == 1)


# generación marco actualizado enighur

seleccion_enighur <- readRDS("insumos/00_viviendas/enighur/marco_enighur_01_10.rds") |> 
  mutate(id_viv_car = paste0(substr(id_viv_car, 1, 18), substr(id_viv_car, 20, 22))) |> 
  filter(substr(id_upm, 1, 10) %in% unique(substr(enlistamiento_01$id_upm, 1, 10)))

enlistamiento_02 <- enlistamiento_01 |> 
  mutate(n_edif_umce = str_pad(n_edif_umce, 3, "left", "0"),
         id_edif_umce = paste0(pro, can, par, zon, sec, manloc,n_edif_umce)) |> 
  left_join(redistribucion_02 |> 
              select(id_edif_umce, id_upm4),
            by = "id_edif_umce") |> 
  mutate(id_upm = case_when(is.na(id_upm4) ~ id_upm,
                            T ~ id_upm4),
         id_viv_car = paste0(id_edif_umce, str_pad(n_viv, 3,"left", "0"))) |> 
  filter(substr(id_upm, 1, 10) %in% unique(substr(seleccion_enighur$id_upm, 1, 10))) |> 
  left_join(seleccion_enighur |> select(-id_upm), by = "id_viv_car")
  



lol <- seleccion_enighur |> 
  filter(!id_viv_car %in% enlistamiento_02$id_viv_car)

lal <- enlis |> 
  filter(!substr(id_upm, 1, 10) %in% unique(substr(enlistamiento_02$id_upm, 1, 10)))


