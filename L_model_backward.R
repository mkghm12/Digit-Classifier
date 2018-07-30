# this function generates model for back propagation for all layers in network at one iteration
# AL- output of output layer generated in forward propagation. dimensions (10,m)
# Y - labels of dataset. dimensions -(10,m) in this case
# returns gradients of all parameters at every layer after backpropagation.

source('./linear_activation_backward.R')
L_model_backward<- function(AL,Y,caches){
  grads=list()
  L=length(caches)
  m=dim(AL)[2]
  dAL=-((Y/AL)-((1-Y)/(1-AL)))
  current_cache = caches[[L]]
  p=linear_activation_backward(dAL,current_cache,activation = "sigmoid")
  grad_dA=p[[1]]
  grad_dW=p[[2]]
  grad_db=p[[3]]
  grads=c(list(grad_dA,grad_dW,grad_db),grads)
  for (l in c((L-1):1)) {
    current_cache=caches[[l]]
    p=linear_activation_backward(grads[[1]],current_cache,activation = "sigmoid")
    grad_dA=p[[1]]
    grad_dW=p[[2]]
    grad_db=p[[3]]
    grads=c(list(grad_dA,grad_dW,grad_db),grads)
  } 
  return(grads)
}