caja <- function(puntos){
  bb <- sf::st_bbox(puntos)
  p<-matrix(
    c(bb["xmin"],bb["ymin"],
      bb["xmin"],bb["ymax"],
      bb["xmax"],bb["ymax"],
      bb["xmax"],bb["ymin"],
      bb["xmin"],bb["ymin"]),
    ncol=2,byrow = T
  )
  box <- sf::st_polygon(list(p))
}