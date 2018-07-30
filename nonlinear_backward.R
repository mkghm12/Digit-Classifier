# this function is used to compute gradient of cost w.r.t. Z
# activation_cache - Z
# dA - gradient of A
source('./derivative_activation.R')
sigmoid_backward <- function(dA,activation_cache){
  dZ=dA*derivative_sigmoid(activation_cache)
  return(dZ)
}

Relu_backward <- function(dA,activation_cache){
  dZ=dA*derivative_ReLU(activation_cache)
  return(dZ)
}