open_marco <- function(folder){
  library(arrow)
  dataset <- open_dataset(folder, format = "parquet")
  
  df_reconstruido <- collect(dataset)
}