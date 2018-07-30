# this function is used to compute partial derivatives(gradient) of cost with respect to activation
# function(A_prev) and parameters(W,b)
# dZ - gradient of cost w.r.t. "Z"
# linear cache - list of A_prev,W,b of current layer
linear_backward <- function(dZ,linear_cache){
  A_prev=linear_cache$A_prev
  W=linear_cache$W
  b=linear_cache$b
  m=dim(A_prev)[2]
  dW=(dZ%*%(t(A_prev)))/m
  db=apply(dZ,1,sum)/m
  dA_prev=t(W)%*%dZ
  return(list("dA_prev"=dA_prev,"dW"=dW,"db"=db))
}