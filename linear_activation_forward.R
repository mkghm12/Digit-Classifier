# this function is used to compute output at layer in forward propagation
# A_prev is output of previous layer and input to current layer. for input-layer A_prev = X 
# W,b is weight and biaas for current layer
# activation is a string either "relu" or "sigmoid" depending upon your activation functions which
# you want to use.
source('./linear_forward.R')
source('./nonlinear.R')
linear_activation_forward<- function(A_prev,W,b,activation){
  if(activation=="relu"){
    p=linear_forward(A_prev,W,b)
    Z=p$Z
    linear_cache = p$cache
    p=ReLU(Z)
    A=p$A
    activation_cache=p$activation_cache
  }else if(activation=="sigmoid"){
    p=linear_forward(A_prev,W,b)
    Z=p$Z
    linear_cache = p$cache
    p=Sigmoid(Z)
    A=p$A
    activation_cache=p$activation_cache
  }
  return(list("A"=A,"cache"=list("linear_cache"=linear_cache,"activation_cache"=activation_cache)))
}