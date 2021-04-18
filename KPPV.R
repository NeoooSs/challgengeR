distance=function(x,y){
  return(sqrt(sum((as.numeric(x)-as.numeric(y))^2)))
}

dist_voisins<-function(vecteur, data){
  return(apply(data, 1, distance, x = vecteur))
}

kppv=function(vecteur,k,data){
  dist=dist_voisins(vecteur,data)
  return(order(dist)[1:k])
}

classerKPPV=function(vecteur, k, data) {
  v=kppv(vecteur,k,data)
  classes=data[v,ncol(data)]
  t=table(classes)
  return (names(t)[which.max(t)])
}