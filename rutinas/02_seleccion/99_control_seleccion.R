rm(list = ls())

library(tidyverse)

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

encuesta <- "enciet_202510"

var <- names(marco)[grepl("_sel" ,names(marco)) & !grepl("_seleccionable" ,names(marco)) & !grepl(encuesta ,names(marco))]

for(i in 1:length(var)){
  lol1 <- var[i]
  lol2 <- paste0(encuesta, "_sel")
  
  aux <- marco |> 
    group_by(.data[[lol1]], .data[[lol2]]) |> 
    summarise(n = n()) |> 
    pivot_wider(names_from = .data[[lol1]], 
                values_from = n, 
                names_prefix = paste0(var[i], "_"),
                values_fill = 0)
  print(aux)
}
