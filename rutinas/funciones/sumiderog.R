sumiderog <- function(i,A,peso,su,sl){
  #Inicializamos el cluster
  clus1 <- c(i)
  #Se calucla el tamaño de la matriz
  n <- dim(A)[1]
  #Se inicializa el peso del cluster
  cw <- peso[i]
  # Se inicializa la capacidad del cluster
  cap <- su-cw
  #Se determina la incidencia del cluster
  inci <- A[clus1,]
  #
  clus <- c(i)-1
  while(cap > 0 & sum(inci)>0 & !(identical(clus,clus1))){
    clus <- clus1
    #Se determinan los nodos adyacentes al cluster 
    nodos <- (1:n)[inci>=1]
    nodos <- nodos[!(nodos %in% clus)]
    #Se determina el peso de los nodos adyacentes
    w <- peso[nodos]
    # Se determina el vector de ganancias
    p <- inci[nodos] 
    # Se calcula un vector lógico en función de los pesos vs cap
    nodos <- nodos[w<=cap]
    #Se determina el peso de los nodos adyacentes
    w <- peso[nodos]
    w1 <- rep(0, length(w))
    w1[w == max(w)] <- 5
    # Se determina el vector de ganancias
    p <- inci[nodos] + w1
    # Aplicamos el problema de la mochila
    if(length(nodos)>0){
      is <- knapsack(w, p, cap )
      # Actualizamos el peso del clúster
      cw <- cw + is$capacity
      # Calculamos lo que falta para llegar a la restricción de viviendas
      cap <- su - cw
      # Añadimos los nodos al clúster
      clus1 <- c( clus, nodos[is$indices])
    }
    #Se determina la incidencia del cluster
    if(length(clus1)>1){
      inci <- colSums(A[clus1,])
    }else{
      inci <- A[clus1,]
    }
  }
  return(clus1)
}