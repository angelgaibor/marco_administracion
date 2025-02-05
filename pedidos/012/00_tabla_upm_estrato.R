rm(list = ls())

library(tidyverse)

marco <- readRDS("productos/01_general/marco_upm_08.rds")
load("../ENDI2/insumos/03_muestra_usm/dpa_2022.RData")


resumen <- marco |> 
  group_by(estrato) |> 
  summarise(nupm = n()) |> 
  ungroup() |> 
  mutate(pro = case_when(substr(estrato, 1, 2) == "30" ~ "01",
                         substr(estrato, 1, 2) == "31" ~ "06",
                         substr(estrato, 1, 2) == "32" ~ "11",
                         substr(estrato, 1, 2) == "33" ~ "17",
                         substr(estrato, 1, 2) == "34" ~ "18",
                         substr(estrato, 1, 2) == "40" ~ "07",
                         substr(estrato, 1, 2) == "41" ~ "08",
                         substr(estrato, 1, 2) == "42" ~ "09",
                         substr(estrato, 1, 2) == "43" ~ "13",
                         substr(estrato, 1, 2) == "44" ~ "23",
                         T ~ substr(estrato, 1, 2)),
         domgeo = substr(estrato, 1, 2),
         area = substr(estrato, 3, 3)) |> 
  arrange(pro, estrato) |> 
  mutate(sintaxis = paste0(pro, " & ", domgeo, " & ", area, " & ", estrato, " & ", nupm, " \\\\"))

write.table(resumen |>  select(sintaxis), "clipboard", row.names = F, quote = F)
