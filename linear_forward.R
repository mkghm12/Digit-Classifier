# this function is used to compute Z = WX+b forlayer
# X is dataset having dimensions (n,m) , where n is no. of features and m is no. of examples
linear_forward <- function(X,W,b) {
  #X=rbind(rep(1,dim(X)[2]),X)
  b=as.vector(b)
  A_prev=X
  Z=W%*%X
  Z=Z+b
  return(list("Z"=Z,"cache"=list("A_prev"=A_prev,"W"=W,"b"=b)))
}