# function for generating confusion matrix
# original is the original label for data. and is a vector which contains value from 1 to 10
# prediction is the predicted label for data. and is a vector which also contains value from 1 to 10
# return value is matrix which is confusion matrix
confusionMatrix <- function(original,prediction){
  cm=matrix(0,nrow = 10,ncol = 10)
  for(i in (1:10)){
    for(j in (1:10)){
      cm[i,j]= sum((original == i & prediction == j))
    }
  }
  return(cm)
}