# Generating standard normal distributed random variable from the exponential majorizing function #
snorm <- function (num, seed){
  if (!is.null(seed)){
    set.seed(seed)
  }
  c = (2*exp(1)/pi)^0.5
  x = 0
  iter = 0
  while (length(x) < num + 1){
    urv1 = runif(1, min = 0, max = 1)
    yexp = -log(urv1)
    urv2 = runif(1, min = 0, max = 1)
    if(urv2 <= exp(-(yexp-1)^2)/2 ){
      urv3 = runif(1, min = 0, max = 1)
      if (urv3 <= 0.5){
        x = append(x, yexp)
      }else {
        x = append(x, -yexp)
      }
    }
    iter = iter + 1
  }
  return(x[2:length(x)])
}