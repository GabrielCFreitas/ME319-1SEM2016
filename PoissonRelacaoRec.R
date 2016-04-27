rpois2 = function(n, lambda){
  amostrPOI = rep(0,n)
  for(i in 1:n){
    x=0
    Fx = exp(-lambda)*lambda^x/factorial(x)
    p=exp(-lambda)
    amUN = runif(1)
    while(amUN > Fx){
      x=x+1
      Fx = Fx+p*lambda/x
      p=p*lambda/x
    }
    amostrPOI[i] = x
  }
  return(amostrPOI)
}

t1 = proc.time()
VApois = rpois2(10000,5)
hist(VApois, xlab="x",ylab="f(x)")
curve(exp(-5)*5^x/factorial(x), add="TRUE")
(t1=proc.time()-t1)

q1p = qpois(0.25,5)
q2p = qpois(0.50,5)
q3p = qpois(0.75,5)
q4p = qpois(1,5)
quartisPOI = c(0,q1p,q2p,q3p,q4p)

pobs = cut(VApois,quartisPOI, include.lowest = TRUE)
obsPOI = as.integer(table(pobs))

espPOI1 = ppois(q1p,5)
espPOI2 = ppois(q2p,5)-ppois(q1p,5)
espPOI3 = ppois(q3p,5)-ppois(q2p,5)
espPOI4 = ppois(q4p,5)-ppois(q3p,5)
espPOI = c(espPOI1,espPOI2,espPOI3,espPOI4)*10000

chisq.test(cbind(obsPOI,espPOI))