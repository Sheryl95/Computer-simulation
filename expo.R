# First of all generate a exponentially distributed random variable #
expo <- function(n, lambda, seed = NULL){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  u1 = runif(n)
  num1 = -log(u1)/lambda
  return(num1)
}