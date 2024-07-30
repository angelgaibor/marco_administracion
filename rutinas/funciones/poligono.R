poligono <- function(apoyo){
  x <- as.numeric(apoyo$x)
  y <- as.numeric(apoyo$y)
  n <- length(x)
  M <- matrix(0,n*(n-1)/2,3)
  K <- matrix(0,n*(n-1)/2,2)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      k=j-i+(i-1)*(2*n-i)/2
      M[k,1] = y[i]-y[j]
      M[k,2] = x[j]-x[i]
      M[k,3] = -y[i]*M[k,2]-x[i]*M[k,1]
      K[k,]=c(i,j)
    }
  }  
  M1 <- matrix(0,3,length(x))
  M1[1,] <- x
  M1[2,] <- y
  M1[3,] <- 1
  
  C <- round(M%*%M1,4)
  D1 <- apply(C>=0,1,sum)
  D2 <- apply(C<=0,1,sum)
  
  A <- K[D1==n | D2==n,]
  n <- dim(A)[1]
  a <- rep(0,n) 
  
  a[1] <- A[1,1]
  a[2] <- A[1,2]
  
  for (i in 3:(n-1)){
    A=A[rowSums(A==a[i-2])!=1,]
    v=A[rowSums(A==a[i-1])==1]
    if (v[1]==a[i-1]){
      a[i]=v[2]
    }
    else{
      a[i]=v[1]
    }
  }
  A=A[rowSums(A==a[n-2])!=1,]
  if(A[1]==a[n-1]){
    a[n]=A[2]
  }
  if(A[2]==a[n-1]){
    a[n]=A[1]
  }
  
  apoyo1 <- apoyo[a,] %>% 
    mutate(equipo=as.numeric(equipo),
           nequipo=paste0("Eq_",str_pad(equipo,2,"left",0)),
           orden_pol=c(1:n),
           x=as.numeric(x),
           y=as.numeric(y))
}