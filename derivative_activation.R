# these functions computes derivative of sigmoid and ReLu functions
# returns value of  derivatives of sigmoid and Relu of any object
source('./nonlinear.R')
derivative_sigmoid<- function(Z){
  h=Sigmoid(Z)$A
  h1=1-h
  return(h*h1)
}

derivative_ReLU<- function(Z){
  h=ReLU(Z)$A
  h1=1-h
  return(h*h1)
}