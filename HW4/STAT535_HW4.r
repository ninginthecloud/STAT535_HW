#########################
#STAT535 HW4
#########################
#problem one
library(ggplot2)
data<-data.frame(x=c(1,1,2,2),y=c(4,3,4,3),z=as.factor(c("+1","-1","+1","-1")))
g<-ggplot(data=data,aes(x=x,y=y,colour=z))+geom_point(size=5)
pdf<-pdf("HW4_1.pdf",width=8,height=6)
g
dev.off()
#problem two
library(zoo)#used for find local max/min
#stochastic gradient descent

#problem three
test<-read.table("E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW4\\hw4-1D-clean.dat")
test<-read.table("E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW4\\hw4-1D-noisy-labels.dat")
#local max
localmax<-function(index,value){
reorder=index[order(value[index],decreasing=T)]
gap<-NULL;
for(i in reorder){
gap=c(gap,min(abs(diff(value[(i-1):(i+1)]))))
}
target=reorder[gap>=mean(gap)][1]
target
}
#stumps
#Y in -1 or +1
#d is the dimension
#N is the number of data points
stumps<-function(X,Y,W,N,d,R=1){
#local max

	X<-matrix(X,c(N,d))
	b<-matrix(rep(0,2*d),c(d,2))
	err<-rep(0,d)
	for(j in 1:d){
		x<-X[,j]
		order.x<-order(x)
		error<-sapply(0:N,function(i){sum(W[order.x]*Y[order.x]*((1:N>i)*2-1))})
        #error<-sapply(0:N,function(i){sum(W[order.x]*Y[order.x]*sign(N-i))})
		zoo.error <- as.zoo(error)
		min.value=rollapply(zoo.error, 3, function(x) which.min(x)==2) 
		max.value=rollapply(zoo.error, 3, function(x) which.max(x)==2)
		l.mius<-index(min.value)[coredata(min.value)]
		l.plus<-index(max.value)[coredata(max.value)]
		plot(0:N,error,type="l")
		if(!length(l.plus)){E.plus=-10000} else{#L.plus=localmax(l.plus,error);
												#E.plus=error[L.plus];
												E.plus=max(error[l.plus]);
												np.plus=l.plus[ error[l.plus]==E.plus];
											    L.plus=np.plus[sample.int(length(np.plus),1)]
												abline(v=L.plus,col="red");abline(h=E.plus,col="red")
												}
		if(!length(l.mius)){E.mius=-10000} else{#L.mius=localmax(l.mius,-error);
												#E.mius=-error[L.mius];
		
												E.mius=max(-error[l.mius]);
												np.mius=(l.mius[-error[l.mius]==E.mius]);
												
												#L.mius=np.mius[sample.int(length(np.mius),1)]
												abline(v=L.mius,col="green");abline(h=-E.mius,col="green")
												}
		
		
		
			
	
	if(E.plus>E.mius){b[j,]<-c(1,-mean(sort(x)[(L.plus):(L.plus+1)]));err[j]<-(1-E.plus)/2}
	else{b[j,]<-c(-1,mean(sort(x)[(L.mius):(L.mius+1)]));err[j]<-(1-E.mius)/2}
	}
	return(c(b=b,err=err))	
}
stumps(test[,1],test[,2],W=rep(1/dim(test)[1],dim(test)[1]),N=dim(test)[1],d=1)
stumps(train[,1],train[,3],W=rep(1/149,149),N=149,d=1)



train<-read.table("E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW4\\hw4-linear-train.dat")
test<-read.table("E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW4\\hw4-linear-test.dat")
#adaboost

#this one is simplified.
#but it doesnt work
adaboost<-function(X,Y,N,M){
	while(!is.matrix(X)){
	X<-data.matrix(X)}
	y=as.numeric(Y>0)
	W<-rep(1/N,N)
	B<-NULL;
	Beta<-NULL;
	error<-NULL;
	
	for(k in 1:M){
	j<-2-(k%%2)#j=1, for x1 axis; j=2, for x2 axis.
	stump<-stumps(X[,j],Y,W,N,d=1)
	B<-cbind(B,stump[1:2])
	correct<-((stump[1]*X[,j]+stump[2])>0)==y
	error<-c(error,stump[3])
	Beta<-c(Beta,.5*log((1-error[k])/error[k]))
	W=correct*W/(2*(1-error[k]))+(!correct)*W/(2*error[k])	
	}
	B1=B[,seq(1,M,2)];
	B2=B[,seq(2,M,2)];
	error1=error[seq(1,M,2)];
	error2=error[seq(2,M,2)];
	Beta1=Beta[seq(1,M,2)];
	Beta2=Beta[seq(2,M,2)];
	
	#error rate
	m<-rbind((B1[1,]%*%t(X[,1])+B1[2,]),(B2[1,]%*%t(X[,2])+B2[2,]))
	TF<-apply(m,2,function(x){x>=0})*2-1
	Error=mean((apply(TF*Beta,2,sum)>=0)!=y)
return(list(B1=B1,B2=B2,W=W,error1=error1,error2=error2,Beta1=Beta1,Beta2=Beta2,Error=Error))
}

#################3333
#this complete version works
#especially the sign()
adaboostold<-function(X,Y,N,M,X.test,Y.test){
	while(!is.matrix(X)){
	X<-data.matrix(X)}
	while(!is.matrix(X.test)){
	X.test<-data.matrix(X.test)}
	
	W<-rep(1/N,N)
	B<-NULL;
	Beta<-NULL;
	error<-NULL;
	train.f<-0;
	test.f<-0;
	train.error<-NULL;
	test.error<-NULL;
	
	for(k in 1:M){
	j<-2-(k%%2)#j=1, for x1 axis; j=2, for x2 axis.
	stump<-stumps(X[,j],Y,W,N,d=1)
	B<-cbind(B,stump[1:2])
	bk=sign(stump[1]*X[,j]+stump[2])#-1,+1
	yb<-bk*Y
	error<-c(error,sum(W*(1-yb)/2))
	Beta<-c(Beta,.5*log((1-error[k])/error[k]))
	W=W*exp(-Beta[k]*yb)
	Z=sum(W);
	W=W/Z;
	
	print(sum(bk==0))
	train.f<-train.f+Beta[k]*bk;
	test.f<-test.f+Beta[k]*(sign(stump[1]*X.test[,j]+stump[2]))
	train.error<-c(train.error,mean(sign(train.f)!=Y))
	test.error<-c(test.error,mean(sign(test.f)!=Y.test))
}
return(list(B=B,Beta=Beta,train.f=train.f,test.f,train.error=train.error,test.error=test.error))
}

m=200
result<-adaboostold(X=train[,1:2],Y=train[,3],N=149,M=m,X.test=test[,1:2],Y.test=test[,3])

#adaboostold(X=train[,1:2],Y=train[,3],N=149,M=2,X.test=test[,1:2],Y.test=test[,3])

###############################
#plot
################################
library(ggplot2)

error.data<-data.frame(iteration=rep(1:m,2),error=c(result$train.error,result$test.error),type=as.factor(c(rep("training.error",m),rep("testing error",m))))
error.plot<-ggplot(data=error.data,aes(x=iteration,y=error,col=type))+
			geom_line()
pdf<-pdf("HW4_4_1.pdf",width=8,height=6)			
error.plot
dev.off()

bk=result$B
beta=result$Beta
x1=seq(0,1,by=0.1)
x2=seq(0,1,by=0.1)
y2=rep(x1,11)
k=1
y1=rep(NA,121)
for(j in 1:length(x2))
{
  y1[k:(k+10)]=rep(x2[j],11)
  k=k+11
}
y1
con=data.frame(y1,y2)
q=0
for (k in 1:m)
{
  j=ifelse((k%%2)==1,1,2)
  q=q+beta[k]*sign(bk[1,k]*con[,j]+bk[2,k])
}
z=sign(q)
z
con=data.frame(y1,y2,z)
colnames(train)<-c("x1","x2","y")
train$type=as.factor(train$y)
train.plot<-ggplot()+geom_point(data=train,aes(x=x1,y=x2,shape=type,colour=type))+stat_contour(data=con,aes(y1,y2,z=z),breaks=0)+ggtitle("train dataset")
colnames(test)<-c("x1","x2","y")
test$type=as.factor(test$y)
test.plot<-ggplot()+geom_point(data=test,aes(x=x1,y=x2,shape=type,colour=type))+stat_contour(data=con,aes(y1,y2,z=z),breaks=0)+ggtitle("test dataset")

raw.plot<-ggplot()+geom_point(data=train,aes(x=x1,y=x2,shape=type,colour=type))
pdf<-pdf("HW4_4_2.pdf",width=8,height=6)
raw.plot
dev.off()

pdf<-pdf("HW4_4_train.pdf",width=8,height=6)
train.plot
dev.off()

pdf<-pdf("HW4_4_test.pdf",width=8,height=6)
test.plot
dev.off()

####################
#test on circle data

train.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-train-10000.dat")
colnames(train.data)<-c("x1","x2","y")
test.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-test.dat")
colnames(train.data)<-c("x1","x2","y")


m=40
result<-adaboostold(X=train.data[,1:2],Y=train.data[,3],N=10000,M=m,X.test=test.data[,1:2],Y.test=test.data[,3])
library(ggplot2)

error.data<-data.frame(iteration=rep(1:m,2),error=c(result$train.error,result$test.error),type=as.factor(c(rep("training.error",m),rep("testing error",m))))
error.plot<-ggplot(data=error.data,aes(x=iteration,y=error,col=type))+
			geom_line()
pdf<-pdf("HW4_4_c1.pdf",width=8,height=6)			
error.plot
dev.off()

bk=result$B
beta=result$Beta
x1=seq(0,1,by=0.01)
x2=seq(0,1,by=0.01)
y2=rep(x1,101)
k=1
y1=rep(NA,101*101)
for(j in 1:length(x2))
{
  y1[k:(k+100)]=rep(x2[j],101)
  k=k+101
}
y1
con=data.frame(y1,y2)
q=0
for (k in 1:m)
{
  j=ifelse((k%%2)==1,1,2)
  q=q+beta[k]*sign(bk[1,k]*con[,j]+bk[2,k])
}
z=sign(q)
z
con=data.frame(y1,y2,z)
colnames(train.data)<-c("x1","x2","y")
train.data$type=as.factor(train.data$y)
train.plot<-ggplot()+geom_point(data=train.data,aes(x=x1,y=x2,shape=type,colour=type))+stat_contour(data=con,aes(y1,y2,z=z),breaks=0)+ggtitle("train dataset")
colnames(test.data)<-c("x1","x2","y")
test.data$type=as.factor(test.data$y)
test.plot<-ggplot()+geom_point(data=test.data,aes(x=x1,y=x2,shape=type,colour=type))+stat_contour(data=con,aes(y1,y2,z=z),breaks=0)+ggtitle("test dataset")

raw.plot<-ggplot()+geom_point(data=train.data,aes(x=x1,y=x2,shape=type,colour=type))


s=0
for (k in 1:m)
{
  j=ifelse((k%%2)==1,1,2)
  s=s+beta[k]*sign(bk[1,k]*train.data[,j]+bk[2,k])
}
plotdata=train.data
plotdata$cluster=as.factor(sign(s))
ggplot()+geom_point(data=train.data,aes(x=x1,y=x2,shape=type,colour=type))+geom_point(data=plotdata,aes(x=x1,y=x2,shape=cluster,colour=cluster))


plotdata=data.frame(x1=train.data[,1],x2=train.data[,2],y=s,type=as.factor(train.data[,3]))
plot1 <- ggplot(data=plotdata, aes(x=x1, y=x2, z = y))+
       stat_contour(breaks=4,colour=2,size=2)
	  
plot1

pdf<-pdf("HW4_4_2c.pdf",width=8,height=6)
raw.plot
dev.off()

pdf<-pdf("HW4_4_trainc.pdf",width=8,height=6)
train.plot
dev.off()

pdf<-pdf("HW4_4_testc.pdf",width=8,height=6)
test.plot
dev.off()