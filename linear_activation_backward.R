# this function is used to compute gradient at layer l 
# cache - contains list of all value computed in forward propagation
# dA - gradient of cost w.r.t to A of l+1 layer
source('./nonlinear_backward.R')
source('./linear_backward.R')
linear_activation_backward = function(dA,cache,activation){
  linear_cache = cache$linear_cache
  activation_cache = cache$activation_cache
  if(activation == "relu"){
    dZ=relu_backward(dA,activation_cache)
    p =linear_backward(dZ,linear_cache)
    dA_prev = p$dA_prev
    dW = p$dW
    db = p$db
  }else if(activation == "sigmoid"){
    dZ=sigmoid_backward(dA,activation_cache)
    p =linear_backward(dZ,linear_cache)
    dA_prev = p$dA_prev
    dW = p$dW
    db = p$db
  }
  r=list(dA_prev,dW,db)
  return(r)
}