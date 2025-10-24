#
rm(list = ls())

library(rio)
library(tidyverse)
library(TeachingSampling)

# Abrimos el último marco
nombre <- last(list.files("productos/01_general/", pattern = ".rds"))
marco <- readRDS(paste0("productos/01_general/",nombre))

m1 <- marco |> 
  select(id_upm, pro, area, domgeo, estrato, Mi,
         ends_with("_sel")) |> 
  mutate(usada = enighur_sel + enciet_202411_sel + enciet_202412_sel + enciet_202501_sel +
           enciet_202502_sel + enciet_202503_sel + enciet_202504_sel + enciet_202505_sel +
           endi3_sel + enciet_202506_sel + enciet_202507_sel + enciet_202508_sel +
           enciet_202509_sel + steps_202506_sel + enciet_202510_sel + enciet_202511_sel,
         usada = ifelse(usada >= 1, 1, usada),
         usada_act = enighur_sel + endi3_sel,
         usada_act = ifelse(usada_act >= 1, 1, usada_act))

rm(marco, nombre)

# abrimos el tamaño por estrato a actualizar
tam_estrato <- readRDS("insumos/02_seleccion/03_enciet/202511/dis_enciet_estrato.rds")

# ahora, vamos a evaluar lo que necesitamos
b1 <- m1 |> 
  group_by(pro, area, estrato) |> 
  summarise(n_upm = n(),
            usadas = sum(usada),
            usadas_act = sum(usada_act),
            prom_vivocu_h = ceiling(mean(Mi))) |> 
  ungroup() |> 
  filter(substr(estrato, 1, 2) != "20") |> 
  left_join(tam_estrato, by = "estrato") |> 
  mutate(dis = n_upm - usadas,
         prop_uso_dis = round(100 * usadas/n_upm, 1),
         paquetes_dis = floor(dis/nh),
         # ahora veamos para sin enciet
         dis_act = n_upm - usadas_act,
         prop_uso_dis_act = round(100 * usadas_act/n_upm, 1),
         paquetes_dis_act = floor(dis_act/nh),
         considerado = ifelse(pro %in% c(15, 16, 19) | estrato == "2429", 0, 1)) |> 
  select(pro, area, estrato,
         n_upm, prom_vivocu_h, nh,
         usadas, dis, prop_uso_dis, paquetes_dis,
         usadas_act, dis_act, prop_uso_dis_act, paquetes_dis_act, considerado) |> 
  mutate(req = nh*22,
         aa = ifelse(dis_act <= req, dis_act, req))

sum(b1$aa)

b2 <- b1 |> 
  mutate(area = case_when(area == 1 ~ "Urbano",
                          area == 2 ~ "Rural",
                          T ~ "LaLaLa")) |> 
  group_by(pro, area) |> 
  summarise(upm_act = sum(aa)) |> 
  ungroup() |> 
  pivot_wider(names_from = area, values_from = upm_act) |> 
  mutate(total_upm = Rural + Urbano) |> 
  arrange(pro) |> 
  select(pro, Urbano, Rural, total_upm)

b3 <- b1 |> 
  mutate(area = case_when(area == 1 ~ "Urbano",
                          area == 2 ~ "Rural",
                          T ~ "LaLaLa")) |> 
  group_by(pro, area) |> 
  summarise(upm_act = sum(aa),
            prom_vivocu = ceiling(sum(n_upm * prom_vivocu_h)/sum(n_upm))) |> 
  ungroup() |> 
  arrange(pro)

export(list("documento" = b2,
            "dica" = b3), "pedidos/025_tamaño_upm_a_actualizar/upm_a_actualizar.xlsx")  
  
  
  