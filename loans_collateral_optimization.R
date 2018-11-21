## 2 exposures, 3 risk mitigants and 1 special constraint
i=2
j=3
q=1
qx=2
qy=3
risk_mit=c(4,8,0.5)
ead=c(2,12)
rw_basic=rep(1,i)
#rw_basic=runif(i,0.9,1.1)
rw_shift=matrix(0,nrow = i, ncol = j)
#rw_shift=matrix(runif(i*j,0.2,0.3),nrow = i, ncol = j)
obj=vector(length =  (i*j))
##Change this in pseudocode
for (x2 in (1:j)){
  for (x1 in (1:i)){
      obj[((x2-1)*i+x1)]=rw_basic[x1]*risk_mit[x2]-rw_shift[x1,x2]*risk_mit[x2]
  }
}
obj=c(obj,rep(0,(i+j+q)))

n_slack=i+j+q
n_var=(i*j+n_slack)

#tableau construction
M=matrix(0,nrow=(i+j+q), ncol = n_var)
for (n in (1:j)){
  M[n,]=c(rep(0,(n-1)*i),rep(1,i),rep(0,i*(j-n)),rep(0,n-1),1,rep(0,(n_slack-n)))
}

## Change this in pseudocode
for (n in (j+1):(j+i)){
  for (m in (-1:(j-2))){
    M[n,n+m*j]=risk_mit[m+2]
  }
  M[n,i*j+n]=1
}

## Change this in pseudo code
for (n in (j+i+1):(j+i+q)){
  M[n,(qy-1)*i+qx]=1
  M[n,i*j+n]=1
}

b=c(rep(1,j),ead,rep(0,q))
M=cbind(b,M)
BV=(i*j+1):(i*j+i+j+q)
Zt=rep(0,ncol(M))
Pt=rep(0,(length(obj)+1))
Pb=vector(length = i+j+q)
##Iterations Start

for (k in 1:(i*j+i+j+q)){

for (n in 1:length(Pb)){
  Pb[n]=obj[BV[n]]
}

for (n in 1:length(obj)){
  Pt[n+1]=obj[n]
}

for (n in 1:ncol(M)){
  Zt[n]=Pb%*%M[,n]-Pt[n]
}

if(min(Zt)>=0){break}

Col=min(which(Zt==min(Zt)))
rv=rep(0,nrow(M))
for (n in 1:nrow(M)){
  rv[n]=ifelse(M[n,Col]>0,M[n,1]/M[n,Col],0)
}
row=which(rv==min(rv[rv>0]))

BV[row]=Col-1

for (n in 1:ncol(M)){
  M[row,n]=M[row,n]/M[row,Col]
}

for (r in 1:nrow(M)){
  mult_fac=M[r,Col]
  if (r==row){next}
  for (n in 1:ncol(M)){
    M[r,n]=M[r,n]-mult_fac*M[row,n]
  }
}

}