# this function is used to model full neural network at a certain learning rate and no.of iterations
# returns a list of vector of iterations and and optimized parameters 
source('./initialize_parameters1.R')
source('./L_model_forward.R')
source('./L_model_backward.R')
source('./update_parameters.R')
source('./computeCost.R')
N_layer_model <- function(X,Y,layers_dim,learning_rate,num_iterations,print_cost=FALSE,lambda=0){
  parameters =initialize_parameters1(layers_dim)
  J_iter=rep(0,num_iterations)
  m=dim(X)[2]
  for (i in c(1:num_iterations)) {
    p=L_model_forward(X,parameters)
    AL=p$AL
    caches = p$caches
    cost=computeCost(AL,Y,parameters,lambda=lambda)
    J_iter[i]=cost
    if(print_cost && (i%%100==0)){
      cat("cost after iterations ",i," :",cost,"\n")  
    }
    grads=L_model_backward(AL,Y,caches)
    para = parameters
    parameters = update_parameters(parameters,grads,learning_rate,m,lambda = lambda)
  }
  return(list(para,J_iter))
}