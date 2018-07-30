# this function is a model for forward propagation in neural network
# X is dataset having dimensions (n,m) where n is no. of features and m is no. of examples
# parameters is a list of original weights and bias.
# this return a list of output at output layer and a list of caches 
# cache contain value of every output, input and parameters at every layer
source('./linear_activation_forward.R')
L_model_forward<- function(X,parameters){
  caches=list()
  A=X
  L=length(parameters)%/%2
  for(l in c(1:(L-1))){
    A_prev=A
    p=linear_activation_forward(A_prev,parameters[[(2*l)-1]],parameters[[(2*l)]],activation="sigmoid")
    A=p$A  
    cache=p$cache
    caches=c(caches,list(cache))
  }
  p=linear_activation_forward(A,parameters[[(2*L)-1]],parameters[[(2*L)]],activation="sigmoid")
  AL=p$A  
  cache=p$cache
  caches=c(caches,list(cache))
  return(list("AL"=AL,"caches"=caches))
}