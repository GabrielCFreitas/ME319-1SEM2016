p=0.5
n=30
m=1000
alpha=0.05
za = qnorm(1-0.05/2)

IC = matrix(0,m,2)
colnames(IC) = c("lower","upper")

for(i in 1:m){
  x = rbinom(n,1,p)
  IC[i,] = mean(x)+c(-1,1)*za*sqrt(mean(x)*(1-mean(x)))/sqrt(n)
}

y = as.numeric((IC[,1]<p & p<IC[,2]))
mean(y)
