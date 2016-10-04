source("F:/sem 7/computer simulation/expo.R")

# Simulating a m/m/1 queue #

lambda = 10
mu = 10
arr = 0
dep = 0
cust = 0
t = 0

qt = vector('numeric', length(t)-1)

set.seed(1)

while(length(dep) < 10){
  if (cust[length(cust)] == 0){
    ta = expo(1, lambda)
    t = append(t,t[length(t)] + ta)
    cust = append(cust, 1)
    ts = expo(1, mu)
    td = ts
    arr = append(arr, t[length(t)])
    dep = append(dep, t[length(t)-1]+ts)
  }else {
    if (arr[length(arr)] < dep[length(dep)]){
      cust = append(cust, cust[length(cust)] + 1)
      ta = expo(1, lambda)
      t = append(t,t[length(t)] + ta)
      arr = append(arr, t[length(t)])
    }else {
      cust = append(cust, cust[length(cust)] - 1)
      ts = expo(1, mu)
      td = ts
      t = append(t, t[length(t)] + td)
      dep = append(dep, t[length(t)])
    }
  }
}

l = length(t)

for (i in 1:(l-1)){
  qt[i] = cust[i] * (t[i+1] - t[i])
}

avg.len = sum(qt)/t[l]

z = which(cust != 0)

util = sum(t[z+1] - t[z]) / t[l]
