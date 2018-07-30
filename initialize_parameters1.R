# this function is used to initialize random parameters for traing neural network
# layers_size is a vector containing no. of nodes in every layer of network including input layer
# return value is parameters in which every odd indexed value is original weight(W) and 
# every even indexed value is bias(b) for that layer

initialize_parameters1 <- function(layers_size) {
  L = length(layers_size)
  parameters = list()
  for (l in c(2:L)) {
    L_in = layers_size[l-1]
    L_out = layers_size[l]
    episilon = sqrt(6/(L_in+L_out))
    p=(matrix(runif((1+L_in)*L_out),nrow = L_out,byrow = FALSE)*2*episilon)-episilon
    parameters[[(2*l)-3]]=p[,-1]
    parameters[[(2*l)-2]]=p[,1]
  }
  return(parameters)
}

