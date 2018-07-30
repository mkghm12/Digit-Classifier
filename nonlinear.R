# functions to compute sigmoid = 1/1+exp(-z) and Relu = max(0,z)
# these are non-linear functions
# return value is list of activation function output and value of input to it for later use in 
# backpropagation
Sigmoid<- function(Z){
  A=exp(-Z)
  A=1+A
  A=1/A
  return(list("A"=A,"activation_cache"=Z))
}

ReLU<- function(Z){
  A=Z
  A[A<0]=0
  return(list("A"=A,"activation_cache"=Z))
}
