# this is function to compute final cost at output of layer-L with regularization considered
# AL is activation output of layer-L. dimensions (10,m)
# parameters contain a list of W1,b1,W2,b2,.....,Wl,bl
# lambda is regularization 
# Y is labelled output (each vector of length 10) for every example.so has (10,m) dimensions
# function returns computed cost
computeCost <- function(AL,Y,parameters,lambda=0) {
  W1=parameters[[1]]
  W2=parameters[[3]]
  m=dim(Y)[2]
  W1=W1**2
  W2=W2**2
  reg=lambda*(sum(W1)+sum(W2))
  reg=reg/(2*m)
  cost=-(Y*log(AL))-((1-Y)*log(1-AL))
  cost=sum(cost)/m
  cost=cost+reg
  return(cost)
}
