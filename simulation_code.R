# #Pseudo Code
# 
# 1) run 2 separate simulations with 2 sds
# 2) select only values that are less than .14 for the thin curve & values greater than .14 for the dispersed one
# 3) keep only 4/15th of the no. of observations of the first simulation
# 4) combine the two to generate the final distribution



simulation_func=function(sd1,sd2,len1,len2,nz_z_prob,z_nz_prob,initial_value,nmonths){
## INitializing Output
output=NULL
output$month0=rep(initial_value,8*(len1+len2))
output=as.data.frame(output)

## Simulating Balances for each month
## Sim 1 & 2 represent the 2 distributions for non zero balances
## Zeroes are introduced randomly using the probabilities
## When a facility goes from 0 to non 0 it is assumed to withdraw 90% of committed amount.
## This can be changed in the last line of the for loop(Line 41)
for (i in 1:nmonths){  
sim1=rnorm(200000,0,sd1)
sim2=rnorm(200000,0,sd2)
sim1=sim1[abs(sim1)<0.14]
sim2=sim2[abs(sim2)>0.14]
sim1=sim1[1:(8*len1)]
sim2=sim2[1:(len2*length(sim1)/len1)]
value_sim1=exp(-(sd1^2)/2+sim1)
value_sim2=exp(-(sd2^2)/2+sim2)
x=ifelse(output[,i]>0,1,0)
for (j in 1:nrow(output)){
  if (x[j]==0){x[j]=ifelse(runif(1) < z_nz_prob,1,0)}
  else {x[j]=ifelse(runif(1) < nz_z_prob,0,1)}
}

x2=c(value_sim1,value_sim2)
x_final=x*x2

eval(parse(text=paste0("output$month",i,"=x_final")))

output[,(i+1)]=ifelse(output[,i]==0 & x==1,1,output[,(i+1)])
}
return(output)
}

final_simulation=simulation_func(0.07,3.5,15000,4000,0.005,0.005,1,12)
write.csv(final_simulation,"final_simulation.csv")