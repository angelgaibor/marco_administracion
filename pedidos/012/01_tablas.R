rm(list = ls())

library(tidyverse)
library(janitor)

estr_previo <- vector("list", 0)

index <- list.files("../marco_estratificacion/intermedios/03_estratificacion/estratos/")

for (i in 1 : length(index)){
  estr_previo[[i]] <- readRDS(paste0("../marco_estratificacion/intermedios/03_estratificacion/estratos/",
                                     index[i])) |> 
    mutate(domest = substr(index[[i]], 1, 3))
}

estr_previo <- do.call("rbind", estr_previo)


apoyo <- estr_previo |> 
  filter(substr(id_upm, 1, 7) %in% c("1701509", "1701809")) |>
  group_by(id_par = substr(id_upm, 1, 6)) |> 
  mutate(cong = row_number()) |> 
  ungroup() |> 
  mutate(id_upm_cor = paste0(id_par, "9", str_pad(cong, 3, "left", "0"))) 

estr_previo_01 <- estr_previo |> 
  filter(!substr(id_upm, 1, 7) %in% c("1701509", "1701809")) |> 
  rbind(apoyo|> 
          select(id_upm = id_upm_cor, est_e1, metodo_e1, edg_e1, est_e2, 
          metodo_e2, edg_e2, domest))




marco_upm <- readRDS("productos/01_general/marco_upm_11.rds")

resumen_upm <- marco_upm |> 
  select(id_upm, estrato) |> 
  mutate(id_upm10 = substr(id_upm, 1, 10)) |> 
  left_join(estr_previo_01 |> 
              mutate(estratoini = paste0(domest, est_e1)) |> 
              select(id_upm10 = id_upm, estratoini, domest),
            by = "id_upm10") |> 
  mutate(domgeo = substr(domest, 1, 2))

lol <- resumen_upm |> 
  filter(is.na(estratoini))

table(substr(lol$id_upm, 1, 6))

tab4 <- resumen_upm |> 
  group_by(pro = substr(id_upm, 1, 2)) |> 
  summarise(n.estrato = n_distinct(estrato),
            n.upm = n()) |> 
  ungroup() |> 
  adorn_totals("row") |> 
  mutate(var = paste0(pro, " & ", n.estrato, " & ", n.upm, "\\\\"))

write.table(tab4 |> 
              select(var),
            "clipboard", row.names = F, quote = F)

auxini <- resumen_upm |> 
  mutate(pro = substr(id_upm, 1, 2),
         area = substr(estrato, 3, 3)) |> 
  group_by(domest) |> 
  mutate(n_upm_domest = n()) |> 
  ungroup() |> 
  group_by(pro, domest, area, estratoini, n_upm_domest) |> 
  summarise(n_upm_estrato = n()) |> 
  ungroup() |> 
  mutate(prop = round(100 * n_upm_estrato / n_upm_domest, 2),
         prop = as.character(prop),
         var = paste0(pro, " & ", domest, " & ", area, " & ", estratoini, " & ",
                      n_upm_domest, " & ", n_upm_estrato, " & ", prop, "\\\\")) 

write.table(auxini |> 
              filter(pro == "17") |> 
              select(var),
            "clipboard", row.names = F, quote = F)


aux <- resumen_upm |> 
  mutate(pro = substr(id_upm, 1, 2),
         area = substr(estrato, 3, 3)) |> 
  group_by(domest) |> 
  mutate(n_upm_domest = n()) |> 
  ungroup() |> 
  group_by(pro, domest, area, estrato, n_upm_domest) |> 
  summarise(n_upm_estrato = n()) |> 
  ungroup() |> 
  mutate(prop = round(100 * n_upm_estrato / n_upm_domest, 2),
         prop = as.character(prop),
         var = paste0(pro, " & ", domest, " & ", area, " & ", estrato, " & ",
                      n_upm_domest, " & ", n_upm_estrato, " & ", prop, "\\\\")) 

write.table(aux |> 
              filter(pro == "17") |> 
              select(var),
            "clipboard", row.names = F, quote = F)

write.table(aux |> 
              filter(pro == "09") |> 
              select(var),
            "clipboard", row.names = F, quote = F)
