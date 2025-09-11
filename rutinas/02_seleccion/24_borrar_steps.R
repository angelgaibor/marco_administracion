rm(list = ls())

library(tidyverse)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

marco1 <- marco |> 
  select(-starts_with("steps"))

saveRDS(marco1, paste0("productos/01_general/marco_upm_", 
                       str_pad(1 + as.numeric(substr(nombre, 11, 12)), 2, "left", "0"),
                       ".rds"))
