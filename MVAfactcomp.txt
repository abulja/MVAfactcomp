#load data 
comp=read.table(file="BusinessCompetitiveness.txt")

#define variable names
colnames(comp)=c("QES","QMSE","FLTA","FDI","NCA","PPS","CI","QSRI","CSR&D","UICR&D","GPATP","ASE","PCTP")

#define number of row and col
n=nrow(comp)
p=ncol(comp)

#matrix of standardized values
xs=scale(comp)

#covariance matrix 
r=cov(xs)

#computing eigenvalues and eigenvectors 
e=eigen(r)
values=e$values
vectors=e$vectors

#percentage of variance explained
perc=values/sum(values)	
cumperc=cumsum(perc)
table=cbind(values,perc,cumperc)

#scree plot 
plot(values, xlab="Number", ylab="Eigenvalues", main="Scree plot", lwd=2, cex=2, col="deeppink3")

#factor loadings matrix 
z=xs%*%vectors
r1=cor(cbind(z,xs))
compet2=r1[14:26,1:2]       # factor loadings unrotated

#percentage of variance explained by common factors 
common=diag(compet2%*%t(compet2))

#specific variance
specific=diag(r)-common

#factor loadings rotated
rot=varimax(compet2)
rotloadings=rot$loadings

#factor scores before rotation 
fa=xs%*%solve(r)%*%compet2

plot(fa,type="n",xlab="Factor scores 1",ylab="Factor scores 2",main="Representation of countries before rotation",cex.lab=1.2,cex.axis=1.2,lwd=2)
abline(h=0,v=0)
text(fa,c("Austria","Belgium","Croatia","Cyprus","Estonia","Iceland","Latvia","Lithuania","Malta","Montenegro","Portugal","SlovakRepublic","Slovenia","Switzerland"),cex=1.2)


#factor scores after rotation 
rotfa=xs%*%solve(r)%*%rotloadings

plot(rotfa,type="n",xlab="Factor scores 1",ylab="Factor scores 2",main="Representation of countries after rotation",cex.lab=1.2,cex.axis=1.2,lwd=2)
abline(h=0,v=0)
text(fa,c("Austria","Belgium","Croatia","Cyprus","Estonia","Iceland","Latvia","Lithuania","Malta","Montenegro","Portugal","SlovakRepublic","Slovenia","Switzerland"),cex=1.2)




