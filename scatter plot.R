# testing for independence #
iid_test <- function(x){
  xi.plus.one = x[2:length(x)]
  xi = x[1:length(x)-1]
  plot(xi,xi.plus.one)
  hist(x, freq = FALSE)
}