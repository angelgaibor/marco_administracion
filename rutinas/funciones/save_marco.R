save_marco <- function(folder, df){
  library(arrow)
  # Carpeta para guardar los bloques
  dir.create(folder, showWarnings = FALSE)
  
  # Tamaño máximo por archivo (en bytes)
  max_size <- 90 * 1024^2  # 90 MB
  
  # Variables para iterar
  i <- 1
  start <- 1
  n <- nrow(df)
  
  while (start <= n) {
    block_size <- 500000  # número inicial de filas por bloque (ajustable)
    repeat {
      end <- min(start + block_size - 1, n)
      chunk <- df[start:end, ]
      
      temp_file <- tempfile(fileext = ".parquet")
      write_parquet(chunk, temp_file)
      size <- file.info(temp_file)$size
      
      if (size > max_size) {
        if (block_size <= 1000) {
          break  # no se puede reducir más
        }
        block_size <- floor(block_size * 0.75)  # reducir el tamaño del bloque
      } else {
        final_file <- sprintf(paste0(folder, "/data_part_%03d.parquet"), i)
        file.rename(temp_file, final_file)
        message(sprintf("Escribiendo bloque %d: %d filas, %.2f MB", i, nrow(chunk), size / 1024^2))
        i <- i + 1
        start <- end + 1
        break
      }
    }
  }
}