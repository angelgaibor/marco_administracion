

rm(list=ls()) 
library(rio) 
library(tidyverse) 
library(readr)



source("sintaxis/funciones/read_zip.R")
  
  
  archivo_zip <- "ENEMDU.zip"  
  direc  <- "bases"
  
personas_ene <- read_zip(direc, archivo_zip, "Personas_Confidencial.sav"  )  
vivienda_h_ene <- read_zip(direc, archivo_zip, "Vivienda_hogar.sav"  )  
vivienda_d_ene <- read_zip(direc, archivo_zip, "Viviendas_Dies.sav"  )  



archivo_zip <- "ENCIET.zip"  
direc  <- "bases"

personas_enc <- read_zip(direc, archivo_zip, "Personas.csv"  )  
vivienda_h_enc <- read_zip(direc, archivo_zip, "Vivienda_hogar.csv"  )  
vivienda_d_enc <- read_zip(direc, archivo_zip, "Vivienda_dies.csv"  )  




nombres <- names(personas_enc) 
nombres_enemdu <- names(personas_ene)

variable = nombres[nombres %in% nombres_enemdu] 

resul = NULL

for (i in 1:length(nombres)) {
  
    a <-ls(personas_ene,pattern = paste0("^",nombres[i], "$"))
    print(a)
    resul = rbind(a, resul) %>% as.data.frame() 
}


