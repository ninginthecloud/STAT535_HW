####################
#STAT 535 HW2      #
####################
library(MASS)#for multinormal distribution

#problem one
#data set generation function 
data.generate<-function(Num,Mu=matrix(c(-1.6,1.6,0,0),c(2,2)),Sigma=diag(c(1,1))){
Y<-sample(c(-1,1),size=Num,replace=T,prob=c(.5,.5))
n0<-sum(Y==-1)
X<-rbind(mvrnorm(n=n0,Mu[1,],Sigma),mvrnorm(n=Num-n0,Mu[2,],Sigma))
return(data.frame(x1=X[,1],x2=X[,2],label=rep(c(-1,1),c(n0,Num-n0))))
}
#implement KNN
#each data contains a column to store which group it belongs to, 
#let's name it label(in homework, it's named as Y)
knn.fun<-function(test.point,train.data,K,dimension){
#diff=apply(train.data[,1:dimension],1,function(x){x-unlist(c(test.point[1,1:dimension]))})
#distance<-apply(diff,2,function(x){sqrt(sum(x^2))})
diff<-train.data[,1:dimension]-c(test.point)[1:dimension]
distance<-apply(diff,1,function(x){sqrt(sum(x^2))})
location<-order(distance)[1:K]
group<-as.numeric(mean(train.data[location,"label"])>0)
#here our k is always a odd value, mean() cannot be 0 exactly
return(c(-1,1)[group+1])
}
#l_b function to test if the predict function is good or notExp
lb<-function(train.data,K,dimension=2){
	l<-NULL;
	for(index in 1:dim(train.data)[1]){
		l<-c(l,knn.fun(train.data[index,],train.data,K,dimension))
		}
	return(mean(l!=train.data[,"label"]))
}
#L_b function to test for testing data
Lb<-function(test.data,Y.estimate){
	return(mean(Y.estimate!=test.data[,"label"]))
}

#y estimate function
Y.est<-function(test.data,train.data,K,dimension=2){
y<-NULL;
	for(index in 1:dim(test.data)[1]){
		y<-c(y,knn.fun(test.data[index,],train.data,K,dimension))
		}
	return(y)
}
#V.function
V.fun<-function(Y){
P<-apply(Y,2,function(x){mean((1+x)/2)})
return(mean(P*(1-P)))
}
#generate testing data D
test.data<-data.generate(Num=1000)
Kpool<-c(1,3,7,11,15,19,21,23,25,31,37,40)
BNum=30
mean.l<-NULL;
mean.L<-NULL;
for(k in Kpool){
l.b<-NULL;
L.b<-NULL;
Y<-NULL;
for(b in 1:BNum){
train.data<-data.generate(Num=100)
y<-Y.est(test.data,train.data,K=k,dimension=2)
Y<-rbind(Y,y)
l.b<-c(l.b,lb(train.data,K=k,dimension=2))
L.b<-c(L.b,Lb(test.data,y))
}
V<-V.fun(Y)
mean.l<-c(result.l,mean(l.b))
mean.L<-c(result.L,mean(L.b))
sd.l<-c(sd.l,sd(l.b))
sd.L<-c(sd.L,sd(l.b))
}

