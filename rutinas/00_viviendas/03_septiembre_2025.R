rm(list = ls())

library(tidyverse)
library(rio)
library(fs)
source("rutinas/funciones/open_marco.R")
source("rutinas/funciones/save_marco.R")

files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_respaldo/", full.names = TRUE)
file.remove(files_to_delete)

files_to_copy <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = FALSE)
file.copy(file.path("productos/00_viviendas/marco_viviendas_resultado/", files_to_copy),
          file.path("productos/00_viviendas/marco_viviendas_respaldo/", files_to_copy))

######
# Tratamiento extra al marco actual

marco_old <- open_marco("productos/00_viviendas/marco_viviendas_respaldo/")

marco_old1 <- marco_old |> 
  separate(jefe_hogar, into = c("n1", "n2", "a1", "a2"), sep = " ", extra = "merge")

nombres <- c("n1","n2","a1","a2")

for (var in nombres) {
  marco_old1 <- marco_old1 %>% 
    mutate(nomb = tolower(.data[[var]]),
           nomb = gsub("\\s+", " ", nomb),
           nomb = trimws(nomb),
           nomb = gsub("[[:punct:]]", "", nomb),
           nomb = gsub("\\d+", "", nomb),
           nomb = case_when(grepl("rechaz",nomb) ~ "",
                            grepl("nadie",nomb) ~ "",
                            is.na(nomb) ~ "",
                            nomb == "nn" ~ "",
                            nomb == "n" ~ "",
                            nomb == "r" ~ "",
                            nomb == "nm" ~ "",
                            nomb == "na" ~ "",
                            nomb == "sn" ~ "",
                            nomb == "nnn" ~ "",
                            nomb == "nnnn" ~ "",
                            nchar(nomb) <= 1 ~ "",
                            T ~ nomb),
           "{var}_t" := nomb) %>% 
    select(-nomb)
}

marco_old2 <- marco_old1 %>% 
  mutate(jefehogar = paste0(n1_t," ",n2_t," ",a1_t," ",a2_t),
         jefehogar = gsub("\\s+", " ", jefehogar),
         jefehogar = trimws(jefehogar),
         jefehogar = ifelse(jefehogar == "" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "en casa" , "sin nombre" , jefehogar),
         # jefehogar = ifelse(nchar(jefehogar) <= 2 , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "en casa en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "n n" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "recahzo" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "rechzo" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "rechao" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadiie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadi en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadi een casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadine en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "casa casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "naide en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nedie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "ndie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(nchar(jefehogar) <= 2 , "sin nombre" , jefehogar),
         jefehogar = toupper(jefehogar))

rev1 <- table(marco_old2$jefehogar) %>% as.data.frame() %>% 
  mutate(Var1 = as.character(Var1),
         caract = nchar(Var1))

marco_old3 <- marco_old2 |> 
  select(-n1, -n2, -a1, -a2, -n1_t, -n2_t, -a1_t, -a2_t)

rm(marco_old, marco_old1, marco_old2, files_to_copy, files_to_delete, nombres, var)


#####
# ENIGHUR
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

nenlistamiento <- last(list.files("insumos/00_viviendas/enighur/", pattern = ".csv"))
enlistamiento <- read.csv(paste0("insumos/00_viviendas/enighur/", nenlistamiento), 
                          sep = ";", colClasses = "character", encoding = "latin1")

enlistamiento_01 <- enlistamiento |> 
  filter(!substr(upm, 1, 10) %in% unique(substr(marco_old3$id_upm[marco_old3$cod_cart == 2], 1, 10))) |> 
  mutate(manloc = case_when(zon == "999" ~ n_loc,
                            T ~ man)) |> 
  filter(c_ocup %in% c("Ocupada con personas Ausentes", "Ocupada con personas Presentes", "Rechazo")) |> 
  mutate(jefe_hogar = paste(primernjh, segundonjh, primerajh, segudonjh, sep = " ")) |> 
  select(pro, can, par, zon, sec, manloc, n_edif_umce = n_umce, piso_n, 
         n_viv, n_hbt = tot_hbt, jefe_hogar, calle = n_via_p, n_pm,
         id_upm = upm)


######### particioón conglomerados

marco_viviendas_ori <- open_marco("insumos/00_viviendas/marco_viviendas")

edificio_upm <- marco_viviendas_ori |> 
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
            vivenl = sum(vivenl, na.rm = T),
            vivpre = sum(vivpre, na.rm = T))

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
    id_conglomerado == "0801500016" & id_upm3 == "asignar" ~ "080150001603",
    id_conglomerado == "1701840127" & id_upm3 == "asignar" ~ "170184012702",
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

muestra_enciet <- readRDS("insumos/00_viviendas/enciet/muestra_acumulada_marco_viviendas.rds")

seleccion_enighur <- readRDS("insumos/00_viviendas/enighur/muestra_acumulada_enighur_marco_viviendas.rds") |> 
  mutate(id_viv_car = paste0(substr(id_viv_car, 1, 18), substr(id_viv_car, 20, 22))) |> 
  filter(substr(id_upm, 1, 10) %in% unique(substr(enlistamiento_01$id_upm, 1, 10)))

set.seed(20250910)

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
  left_join(seleccion_enighur |> select(-id_upm, -encuesta), by = "id_viv_car") |> 
  select(-id_edif_umce, -id_upm4) |> 
  mutate(seleccion = case_when(id_viv_car %in% seleccion_enighur$id_viv_car[seleccion_enighur$n <= 12] ~ 1,
                               T ~ 0),
         encuesta = case_when(id_viv_car %in% seleccion_enighur$id_viv_car[seleccion_enighur$n <= 12] ~ "01_enighur",
                              T ~ "99_libre"),
         link = pluscodes,
         cod_cart = "2") |> 
  rename(orden = n)

# Corrección jefe de hogar

enlistamiento_03 <- enlistamiento_02 |> 
  separate(jefe_hogar, into = c("n1", "n2", "a1", "a2"), sep = " ", extra = "merge")

nombres <- c("n1","n2","a1","a2")

for (var in nombres) {
  enlistamiento_03 <- enlistamiento_03 %>% 
    mutate(nomb = tolower(.data[[var]]),
           nomb = gsub("\\s+", " ", nomb),
           nomb = trimws(nomb),
           nomb = gsub("[[:punct:]]", "", nomb),
           nomb = gsub("\\d+", "", nomb),
           nomb = case_when(grepl("rechaz",nomb) ~ "",
                            grepl("nadie",nomb) ~ "",
                            is.na(nomb) ~ "",
                            nomb == "nn" ~ "",
                            nomb == "n" ~ "",
                            nomb == "r" ~ "",
                            nomb == "nm" ~ "",
                            nomb == "na" ~ "",
                            nomb == "sn" ~ "",
                            nomb == "nnn" ~ "",
                            nomb == "nnnn" ~ "",
                            nchar(nomb) <= 1 ~ "",
                            T ~ nomb),
           "{var}_t" := nomb) %>% 
    select(-nomb)
}

enlistamiento_04 <- enlistamiento_03 %>% 
  mutate(jefehogar = paste0(n1_t," ",n2_t," ",a1_t," ",a2_t),
         jefehogar = gsub("\\s+", " ", jefehogar),
         jefehogar = trimws(jefehogar),
         jefehogar = ifelse(jefehogar == "" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "en casa" , "sin nombre" , jefehogar),
         # jefehogar = ifelse(nchar(jefehogar) <= 2 , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "en casa en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "n n" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "recahzo" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "rechzo" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "rechao" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadiie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadi en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadi een casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nadine en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "casa casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "naide en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "nedie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(jefehogar == "ndie en casa" , "sin nombre" , jefehogar),
         jefehogar = ifelse(nchar(jefehogar) <= 2 , "sin nombre" , jefehogar),
         jefehogar = toupper(jefehogar))

rev2 <- table(enlistamiento_04$jefehogar) %>% as.data.frame() %>% 
  mutate(Var1 = as.character(Var1),
         caract = nchar(Var1))

enlistamiento_05 <- enlistamiento_04 |> 
  select(-n1, -n2, -a1, -a2, -n1_t, -n2_t, -a1_t, -a2_t)

colSums(is.na(enlistamiento_05))

table(enlistamiento_05$orden, enlistamiento_05$seleccion)

names(enlistamiento_05)[!names(enlistamiento_05) %in% names(marco_old3)]
names(marco_old3)[!names(marco_old3) %in% names(enlistamiento_05)]

marco_viviendas_01 <- marco_old3 |> 
  mutate(id_viv_car = paste0(pro, can, par, zon, sec, manloc, n_edif_umce, n_viv)) |> 
  select(names(enlistamiento_05)) |> 
  rbind(enlistamiento_05) |> 
  left_join(muestra_enciet |> 
              rename(encuesta_enciet = encuesta), 
            by = c("id_viv_car", "cod_cart")) |> 
  mutate(encuesta = case_when(!is.na(encuesta_enciet) ~ "02_enciet",
                              T ~ encuesta),
         seleccion = case_when(!is.na(encuesta_enciet) ~ 1,
                               T ~ seleccion),
         n_viv = str_pad(n_viv, 3, "left", "0"))


lol <- marco_viviendas_01 |> 
  group_by(id_upm, cod_cart, encuesta) |> 
  summarise(m = n(), min = min(orden), max = max(orden), suma = sum(orden), 
            sumeni = sum(encuesta == "01_enighur"),
            sumcie = sum(encuesta == "02_enciet", na.rm = T),
            contol = n_distinct(nap)) |> 
  group_by(id_upm) |> 
  mutate(n = n_distinct(encuesta)) |> 
  ungroup()

files_to_delete <- list.files("productos/00_viviendas/marco_viviendas_resultado/", full.names = TRUE)
file.remove(files_to_delete)

save_marco("productos/00_viviendas/marco_viviendas_resultado/", marco_viviendas_01)
