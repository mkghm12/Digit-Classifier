# this function is used to display our dataset of pixels into images 
# X is matrix . it can be only matrix. for proper image no. of examples should of length of square
# of an integer.    
# example_width is width(integer) of image. for proper image example_width = sqrt(dim(X)[2]) 

displaydata<- function(X,example_width=NULL){
  if(is.null(example_width) || example_width==0){
      example_width=round(sqrt(dim(X)[1]))  
  }
  m=dim(X)[2]
  n=dim(X)[1]  
  example_height=(n/example_width)
  display_rows = floor(sqrt(m))
  display_cols = ceiling(m/display_rows)
  pad=1
  
  display_array = matrix(-1,nrow=(pad+display_rows*(example_height+pad)),ncol = (pad+display_cols*(example_width+pad)))
  current_ex =1
  for(j in 1:display_cols){
    for (i in 1:display_rows) {
      if(current_ex>m){
        break
      }
      max_val =max(abs(X[,current_ex]))
      Y=as.matrix(X[,current_ex],byrow=TRUE)
      dim(Y)=c(example_height,example_width)
      display_array[(pad+(j-1)*(example_height+pad)+(1:example_height)),(pad+(i-1)*(example_width+pad)+(1:example_width))]=Y/max_val
      current_ex=current_ex+1
      if(current_ex>m){
        break
      }
      
    }
  }
  display_array <- apply(display_array, 2, rev)
  h=image(t(display_array),col =gray.colors(256),xaxt='n',yaxt='n')
}