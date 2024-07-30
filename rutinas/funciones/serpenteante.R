serpenteante <- function(voronoi, idp = NULL){
  library(surveillance)
  library(spdep)
  
  voronoi <- voronoi %>% 
    rename(id = {{idp}})
  
  conglomerados_centroides <- voronoi %>% 
    st_centroid(.)
  x <- as.numeric(st_coordinates(conglomerados_centroides)[,1])
  y <- as.numeric(st_coordinates(conglomerados_centroides)[,2])
  prueba <- as.data.frame(cbind(x,y)) %>% 
    mutate(x=as.numeric(x),
           y=as.numeric(y))
  x <- prueba$x
  y <- prueba$y
  n <- length(x)
  if(n>1){
    #A <- poly2adjmat(voronoi)
    A <- matinc(voronoi, tol = 0, id = "id")
    diag(A) <- 0
    com_x <- matrix(0,n,n)
    com_y <- matrix(0,n,n)
    dis <- matrix(0,n,n)
    for (i in 1:n) {
      com_x[i,] <- x-x[i]
      com_y[i,] <- y-y[i]
      dis[i,] <- sqrt((x-x[i])^2+(y-y[i])^2)
    }
    # Creamos h el vector de orden
    h <- rep(0,n)
    #Paso 1. Identificamos la manzana cuyo componente en $y$ sea mayor.
    man_0 <- match(max(y),y)
    #man_0 <- 1
    i=1
    h[man_0]=i
    y[man_0]=min(y)-1
    
    while (min(h)==0){
      i=i+1
      #Paso 2. Identificamos todas las manzanas con las que comparte frontera.
      fron <- c(1:dim(A)[2])[A[man_0,]==1]
      #Paso 3. Si existe más de una manzana hacia la derecha se escoje aquella cuya 
      if(length(fron)!=0){
        man_1 <- fron[match(max(com_y[man_0,fron]),com_y[man_0,fron])]
      }
      else{
        man_1 <- match(max(y),y)
      }
      y[man_1]=min(y)-1
      A[man_0,]=0
      A[,man_0]=0
      man_0=man_1
      h[man_0]=i
      print(man_0)
    }
    prueba_01 <- cbind(voronoi,orden=h) %>% 
      rename({{idp}} := id)
  }
  if(n==1){
    prueba_01 <- cbind(voronoi,orden=1) %>% 
    rename({{idp}} := id)
  }
  return(prueba_01)
}