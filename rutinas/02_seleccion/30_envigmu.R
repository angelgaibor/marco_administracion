rm(list = ls())

library(tidyverse)
library(TeachingSampling)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

# Fijamos el umbral de corte del tamanio de UPM

Mi_min <- 20

# Incluir el nombre de la encuesta para las variables
encuesta <- "envigmu"

if(sum(grepl(encuesta, names(marco))) == 0){
  # Se abre el tamanio por estrato
  
  carpeta <- list.files("insumos/02_seleccion", pattern = encuesta, full.names = T)
  
  archivo <- list.files(carpeta, pattern = ".rds")
  
  tam_estrato <- readRDS(paste0(carpeta, "/", archivo)) |> 
    rename(aux_estrato = estrato,
           aux_nh = nh)
  
  marco_01 <- marco |> 
    #filter(pro != "20") |> 
    # Creamos la variable dominio de en función del corte
    
    mutate(aux_estrato = ifelse(Mi < Mi_min, "9999", estrato),
           
           aux_seleccionable = case_when(enciet_202511_sel == 1 ~ 15,
                                         enciet_202510_sel == 1 ~ 14,
                                         enciet_202509_sel == 1 ~ 13,
                                         enciet_202508_sel == 1 ~ 12,
                                         enciet_202507_sel == 1 ~ 11,
                                         enciet_202506_sel == 1 ~ 10,
                                         enciet_202505_sel == 1 ~ 9,
                                         enciet_202504_sel == 1 ~ 8,
                                         enciet_202503_sel == 1 ~ 7,
                                         enciet_202502_sel == 1 ~ 6,
                                         enciet_202501_sel == 1 ~ 5,
                                         enciet_202412_sel == 1 ~ 4,
                                         enciet_202411_sel == 1 ~ 3,
                                         steps_202506_sel == 1 ~ 17,
                                         endi3_sel == 1 ~ 16,
                                         enighur_sel == 1 ~ 1,
                                         T ~ 2)) |>  
    
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
    mutate(aux_sel = ifelse(aux_orden <= aux_nh, 1, 0)) |> 
    rename_with(.fn = ~ gsub("aux", encuesta, .x))
  
  muestra <- marco_01 |>
    filter(.data[[paste0(encuesta, "_sel")]] == 1) |>
    select(c("id_upm", "pro", "area", "estrato", "Mi", paste0(encuesta, "_pii")))
  
  saveRDS(marco_01, paste0("productos/01_general/marco_upm_", 
                           str_pad(1 + as.numeric(substr(nombre, 11, 12)), 2, "left", "0"),
                           ".rds"))
  
  dir.create(gsub("insumos", "productos", carpeta), showWarnings = F)
  
  saveRDS(muestra, paste0(gsub("insumos", "productos", carpeta), "/muestra.rds"))
  
}else{
  print("La selección ya fue realizada para la encuesta verificar la versión del marco o el nombre de la encuesta")
}

