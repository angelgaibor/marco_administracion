open_marco <- function(folder){
  dataset <- open_dataset(folder, format = "parquet")
  
  df_reconstruido <- collect(dataset)
}