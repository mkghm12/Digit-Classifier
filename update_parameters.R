# this function is used to update value of each parameters after each iteration of backpropagation
# parameter is a list of parameters
# learning_rate is learning rate for optimization of cost
# grads is a list of gradients of activation and parameters of each layer 
# computed along backpropagation
# m is number of training examples
# returnValue is updated parameters
update_parameters <- function(parameters,grads,learning_rate,m,lambda=0){
  L=length(parameters)%/%2
  for (l in c(1:L)) {
    reg=(lambda*(parameters[[(2*l)-1]]))/m
    parameters[[(2*l)-1]]=parameters[[(2*l)-1]]-(learning_rate*grads[[(3*l)-1]])-reg
    parameters[[(2*l)]]=parameters[[(2*l)]]-(learning_rate*grads[[(3*l)]])
  }
  return(parameters)
}