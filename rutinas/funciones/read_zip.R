
read_zip <- function(direc, archivo_zip, documento ){
  
  index <- unzip(zipfile = paste0(direc,"/",archivo_zip),exdir=paste0(direc, "/temporal"))
  
  resultado <- import(index[grepl(pattern = documento,index )])
  
  
  unlink((paste0(direc, "/temporal")), recursive = T)  
  resultado
}