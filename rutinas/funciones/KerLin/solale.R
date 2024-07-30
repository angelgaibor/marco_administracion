solale<-function(n,k){
  p=ceiling(n/k)
  m=p*k-n
  s=matrix(c(sample(1:n,n),rep(0,m)),k,p)
}