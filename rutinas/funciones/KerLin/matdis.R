matdis<-function(x,y){
  n=length(x)
  C=matrix(0,n,n)
  for (i in 1:n){
    C[i,]=sqrt((x[i]-x)^2+(y[i]-y)^2)
  }
  return(C)
}