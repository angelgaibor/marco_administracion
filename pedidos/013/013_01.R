rm(list = ls())

library(tidyverse)

categoria <- readRDS("../marco_estratificacion/intermedios/02_clasificacion_variables/variables_categorias_final.rds")

resumen <- categoria |> 
  filter(variable == "techo") |> 
  pivot_wider(names_from = categoria, values_from = bienestar, names_prefix = "opcion_") |> 
  mutate(sintaxis = paste(domest, opcion_1, opcion_2, opcion_3, opcion_4, opcion_5, opcion_6,
                          sep = " & "))

write.table(resumen |> 
              select(sintaxis),
            "clipboard", row.names = F, quote = F)
