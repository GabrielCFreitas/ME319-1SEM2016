alpha = seq(from = 0.001, to = 0.05, by = 0.00001)
beta = 0.05-alpha
n=51
m=1000
q1 = qchisq(alpha,n-1)
q2 = qchisq(1-beta,n-1)

pIC = pchisq(q2,n-1)-pchisq(q1,n-1)

x = rnorm(n,0,50)
S2 = var(x)
largura = (n-1)*S2*(1/q1-1/q2)
plot(q1,largura)

idx = which.min(largura)

#Largura mínima quando q1 e q2 são:

q_1 = q1[idx]
q_2 = q2[idx]


#IC = matrix(0,m,2)
#colnames(IC) = c("lower","upper")
#j = idx #Escolha um q1 e q2 usando o indice de P(Q<q1) que você quiser.
#for(i in 1:m){
 # x = rnorm(n+1,0,20)
  #S2 = var(x)
  #IC[i,] = c(n*S2/q2[j], n*S2/q1[j])
#}
#y = (IC[,1]<400 & 400<IC[,2])
#mean(y)


#Ou seja, P(Q<q1) e P(Q>q2) são:

pq_1 = alpha[idx]
pq_2 = beta[idx]
