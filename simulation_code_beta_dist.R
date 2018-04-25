utilization=seq(0.01,1,0.01)
final_out=NULL
for (m in utilization){
sim=matrix(0,nrow=41,ncol = 20000)
sim[1,]=(rep(m,20000))
out=vector(length = 41)
out[1]=m
a[1]=0.0484
for (i in 1:40){
  for (j in 1:20000){
    m=sim[i,j]
    if (m>0.01){
    a[2]=(0.0484+(1-m)^2-(1-m))
    a[3]=2*(2*m-1)*(1-m)-(2*m-1)
    a[4]=(2*m-1)^2
    #print(m)
    k=polyroot(c(a[[4]],a[[3]],a[[2]],a[[1]]))
    k=k[which(Im(k)<0.00000001)]
    k=max(Re(k))
    alpha=m*(k-2)+1
    beta=k-alpha
    sim[(i+1),j]=rbeta(1,alpha,beta)}
    else {
      sim[(i+1),j]=rbeta(1,0.023405,0.98)
    }
  }
  out[i+1]=as.numeric(quantile(sim[(i+1),],0.95))
}
final_out=rbind(final_out,out)
}
write.csv(final_out,"pe_by_util_maturity.csv")
# 
# print(c(alpha,beta))
# write.csv(rbeta(100000,alpha,beta),"beta_sample.csv")