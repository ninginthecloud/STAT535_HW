##############
#STAT535 HW3 #
##############
library(ggplot2)
##NEURAL NETWORK
#basic function
sigmoid<-function(x){return(exp(x)/ (1 + exp(x)))}
dsigmoid<-function(x){return(sigmoid(x)*(1-sigmoid(x)))}
outa<-function(x){return(as.numeric(x>0.5))}
#crossentropycost
deltaout<-function(a,y){return(y-a)}
loss<-function(a,y){return(mean(-y*log(a)-(1-y)*log(1-a)))}
#feedforward function
forward.fun<-function(W1,W2,B1,B2,X,Y){
y<-as.numeric(Y>0)
#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
return(list(Z1=Z1,Z2=Z2,a1=a1,a2=a2,loss=loss(a2,y)))
}
#line search
#line.search<-function(W1,W2,B1,B2,gw1,gw2,gb1,gb2,,X,Y,c,beta,step,eta){
for(i in 1:step){
old=forward.fun(W1,W2,B1,B2)
new=forward.fun(W1-gw1*eta[1],W2,B1,B2)
if(old-new<-eta[1]*c*gw1){}


}
}
#network for test
test.fun<-function(W1,W2,B1,B2,X,Y){
#initialize
y<-as.numeric(Y>=0)
X<-data.matrix(X)
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
return(loss(a2,y))
}
#L is the number of layers. Here we set it as 3, only one hidden layer
#j: the hidden units number
#N: training sample size

#GFIX: steepest descent with fixed step size
#GLS: line search
#SG: stochastic gradient

#abs(1-rev(loss.vec)[1]/rev(loss.vec)[2])>tol)
#GFIX neural network
net.GFIX<-function(W1,W2,B1,B2,X,Y,j,eta,X.test,Y.test,tol=10^(-4)){
#initialize
y<-as.numeric(Y>=0)
N = dim(X)[1]
p = dim(X)[2]
X<-data.matrix(X)
X.test=data.matrix(X.test)
Y.test=as.numeric(Y.test>=0)
step=0
loss.test=0;
error.test=NULL;
error.train=NULL;
loss.vec=Inf;#default the largest loss
while(abs(1-rev(loss.vec)[1]/rev(loss.vec)[2])>tol|step==0)
{#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
loss.vec<-c(loss.vec,loss(a2,y))
#output error
delta2 = matrix(deltaout(a2,y),c(1,N))#1*N
#backpropagate
delta1 = (t(W2)%*%delta2)*dsigmoid(Z1)#j*N
#gradient
gw2=-(delta2%*%t(a1))/N;#1*j
gb2=-apply(delta2,1,mean)
gw1=-(delta1%*%X)/N#j*p
gb1=-apply(delta1,1,mean)
#GFIX: steepest descent with fixed step size
W1=W1-gw1*eta
W2=W2-gw2*eta
B1=B1-gb1*eta
B2=B2-gb2*eta
step=step+1
test<-forward.fun(W1,W2,B1,B2,X.test,Y.test)
train<-forward.fun(W1,W2,B1,B2,X,y)
loss.test<-c(loss.test,test$loss)
error.test<-c(error.test,mean((test$a2>=0.5)!=Y.test))
error.train<-c(error.train,mean((train$a2>=0.5)!=y))
}
return(list(loss.train=loss.vec,step=step,W1=W1,W2=W2,B1=B1,B2=B2,train=error.train,loss.test=loss.test,test=error.test))
}

#line search
#GFIX neural network
#net.GLS<-function(X,Y,j,eta,tol=10^(-4),iteration=50000){
#initialize
y<-as.numeric(Y>=0)
N = dim(X)[1]
p = dim(X)[2]
X<-data.matrix(X)
W1=matrix(sample(c(-1,1)/sqrt(j),size=p*j,replace=TRUE),c(j,p))
W2=matrix(sample(c(-1,1),size=j*1,replace=TRUE),c(1,j))
B1 = rnorm(n=j,c(-1,1))
B2 = rnorm(n=1,c(-1,1))
step=0
est=forward.fun(W1,W2,B1,B2,X,Y)
loss.vec=loss(est$a2,y)#default the largest loss
while(step<=iteration)
{
#output error
delta2 = matrix(deltaout(a2,y),c(1,N))#1*N
#backpropagate
delta1 = (t(W2)%*%delta2)*dsigmoid(Z1)#j*N
#gradient
gw2=-(delta2%*%t(a1))/N;#1*j
gb2=-apply(delta2,1,mean)
gw1=-(delta1%*%X)/N#j*p
gb1=-apply(delta1,1,mean)
#GF: steepest descent with best step size based on ...
while(loss(est$a2,y)-loss(forward.fun(W1-gw1*eta,W2-gw2*eta,B1-gb1*eta)))
W1=W1-gw1*eta
W2=W1-gw1*eta
B1=B1-gb1*eta
B2=B2-gb2*eta
step=step+1
est=forward.fun(W1,W2,B1,B2,X,Y)
loss.vec<-c(loss.vec,loss(est$a2,y))
}
return(list(loss=loss.vec,step=step,W1,W2,B1,B2,as.numeric(a2>=.5)))
}

#SG neural network
net.SG<-function(W1,W2,B1,B2,X,Y,j,X.test,Y.test,eta,tol=10^(-4),iteration=1000){
#initialize
y<-as.numeric(Y>=0)
N = dim(X)[1]
p = dim(X)[2]
X<-data.matrix(X)
Y.test<-as.numeric(Y.test>=0)
X.test<-data.matrix(X.test)
epoch=0
loss.test=NULL;
error.test=NULL;
#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
loss.vec<-loss(a2,y)
record.W1<-matrix(rep(0,j*p),c(j,p))
record.W2<-matrix(rep(0,j),c(1,j))
record.B1<-rep(0,j)
record.B2=0
error.train=mean((a2>=0.5)!=y)
while(epoch<iteration)
{
permutation<-sample.int(N,size=N,replace=FALSE)
step=1
	for(index in permutation){
#output error
delta2 = deltaout(a2[index],y[index])# point 1*1
#backpropagate
delta1 = (t(W2)*delta2)*dsigmoid(Z1[,index])#j*1
#gradient
gw2=-delta2*t(a1[,index]);#1*j
gb2=-delta2#1*1
gw1=-delta1%*%X[index,]#j*p
gb1=-as.vector(delta1)#j*1
#steepest descent with fixed step size
W1=W1-gw1*eta/step
W2=W2-gw2*eta/step
B1=B1-gb1*eta/step
B2=B2-gb2*eta/step
#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
step=step+1
	}
	if(iteration-epoch<50){
	record.W1=record.W1+W1
	record.W2=record.W2+W2
	record.B1=record.B1+B1
	record.B2=record.B2+B2
	
	}
epoch=epoch+1
print(epoch)
loss.vec<-c(loss.vec,loss(a2,y))
test<-forward.fun(W1,W2,B1,B2,X.test,Y.test)
train<-forward.fun(W1,W2,B1,B2,X,y)
loss.test<-c(loss.test,test$loss)
error.test<-c(error.test,mean((test$a2>=0.5)!=Y.test))
error.train<-c(error.train,mean((train$a2>=0.5)!=y))
}
W1=record.W1/50
W2=record.W2/50
B1=record.B1/50
B2=record.B2/50
test<-forward.fun(W1,W2,B1,B2,X.test,Y.test)
return(list(loss.train=loss.vec,epoch=epoch,W1=W1,W2=W2,B1=B1,B2=B2,train=error.train,loss.test=loss.test,test=error.test,final=c(error=mean((test$a2>=0.5)!=Y.test),loss=test$loss)))
}


train.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-train-100.dat")
colnames(train.data)<-c("x1","x2","y")
test.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-test.dat")
colnames(train.data)<-c("x1","x2","y")
#hidden units 5
#initialize
j=5;p=2
W.1= matrix(runif(n=p*j,min=-sqrt(5),max=sqrt(5)),c(j,p))
W.2= matrix(runif(n=j*1,min=-sqrt(5),max=sqrt(5)),c(1,j))
W.1= matrix(sample(c(-sqrt(5),sqrt(5)),size=j*p,replace=TRUE),c(j,p))
W.2= matrix(sample(c(-sqrt(5),sqrt(5)),size=j,replace=TRUE),c(1,j))
W.1= matrix(rep(sqrt(5),j*p),c(j,p))
W.2= matrix(rep(sqrt(5),p),c(1,p))
B.1 = runif(n=j,min=-sqrt(5),max=sqrt(5))
B.2 = runif(n=1,min=-sqrt(5),max=sqrt(5))

haha<-list(W.1,W.2,B.1,B.2)


#save(haha,file="initialize.RData")
#GFIX
result.GFIX5<-net.GFIX(W.1,W.2,B.1,B.2,X=train.data[,1:2],Y=train.data[,3],j=5,eta=0.8,X.test=test.data[,1:2],Y.test=test.data[,3],tol=10^(-4))
result.GFIX5$train
result.GFIX5$step

#SG
result.SG<-net.SG(W.1,W.2,B.1,B.2,X=train.data[,1:2],Y=train.data[,3],j=5,eta=1.5,X.test=test.data[,1:2],Y.test=test.data[,3],iteration=2500)
result.SG$test
result.SG$final


#Newton 

###########
#plot area#
###########
orig.train100<-ggplot(data=data.frame(train.data),aes(x=x1,y=x2,colour=as.factor(y)))+geom_point()
pdf<-pdf("HW3_1.pdf",width=8,height=6)
orig.train100
dev.off()

#GFIX
#plotdata

lossdata<-data.frame(loss=c(result.GFIX5$loss.train[-1],result.GFIX5$loss.test[-1]),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GFIX.loss
errordata<-data.frame(error=c(result.GFIX5$train,result.GFIX5$test),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GFIX.error
W1.test<-result.GFIX5$W1
W2.test<-result.GFIX5$W2
B1.test<-result.GFIX5$B1
B2.test<-result.GFIX5$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GFIX_bound.pdf",width=8,height=6)
bound
dev.off()

pdf<-pdf("HW3_GFIX_error.pdf",width=8,height=6)
GFIX.error
dev.off()
pdf<-pdf("HW3_GFIX_loss.pdf",width=8,height=6)
GFIX.loss
dev.off()

#SG
#fixed step
lossdata<-data.frame(loss=c(result.SG$loss.train[-1],result.SG$loss.test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GS.loss
errordata<-data.frame(error=c(result.SG$train[-1],result.SG$test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GS.error
W1.test<-result.SG$W1
W2.test<-result.SG$W2
B1.test<-result.SG$B1
B2.test<-result.SG$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GS_bound.pdf",width=8,height=6)
bound
dev.off()
pdf<-pdf("HW3_GS_error.pdf",width=8,height=6)
GS.error
dev.off()
pdf<-pdf("HW3_GS_loss.pdf",width=8,height=6)
GS.loss
dev.off()


###############
#another
##############

train.data10000=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-train-10000.dat")
colnames(train.data10000)<-c("x1","x2","y")
test.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-test.dat")
colnames(train.data)<-c("x1","x2","y")

#GFIX
result.GFIX5<-net.GFIX(W.1,W.2,B.1,B.2,X=train.data10000[,1:2],Y=train.data10000[,3],j=5,eta=0.8,X.test=test.data[,1:2],Y.test=test.data[,3],tol=10^(-4))
result.GFIX5$train
result.GFIX5$step

#SG
result.SG<-net.SG(W.1,W.2,B.1,B.2,X=train.data10000[,1:2],Y=train.data10000[,3],j=5,eta=1.5,X.test=test.data[,1:2],Y.test=test.data[,3],iteration=100)
result.SG$test



#Newton 

###########
#plot area#
###########
orig.train100<-ggplot(data=data.frame(train.data10000),aes(x=x1,y=x2,colour=as.factor(y)))+geom_point()
pdf<-pdf("HW3_1_a.pdf",width=8,height=6)
orig.train100
dev.off()

#GFIX
#plotdata
lossdata<-data.frame(loss=c(result.GFIX5$loss.train[-1],result.GFIX5$loss.test[-1]),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GFIX.loss
errordata<-data.frame(error=c(result.GFIX5$train,result.GFIX5$test),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GFIX.error
W1.test<-result.GFIX5$W1
W2.test<-result.GFIX5$W2
B1.test<-result.GFIX5$B1
B2.test<-result.GFIX5$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GFIX_bound_b.pdf",width=8,height=6)
bound
dev.off()

pdf<-pdf("HW3_GFIX_error_b.pdf",width=8,height=6)
GFIX.error
dev.off()
pdf<-pdf("HW3_GFIX_loss_b.pdf",width=8,height=6)
GFIX.loss
dev.off()

#SG
#fixed step
lossdata<-data.frame(loss=c(result.SG$loss.train[-1],result.SG$loss.test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GS.loss
errordata<-data.frame(error=c(result.SG$train[-1],result.SG$test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GS.error
W1.test<-result.SG$W1
W2.test<-result.SG$W2
B1.test<-result.SG$B1
B2.test<-result.SG$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GS_bound_b.pdf",width=8,height=6)
bound
dev.off()
pdf<-pdf("HW3_GS_error_b.pdf",width=8,height=6)
GS.error
dev.off()
pdf<-pdf("HW3_GS_loss_b.pdf",width=8,height=6)
GS.loss
dev.off()

####################3
#different int
#GFIX
result.GFIX5<-net.GFIX(W.1,W.2,B.1,B.2,X=train.data[,1:2],Y=train.data[,3],j=5,eta=0.8,X.test=test.data[,1:2],Y.test=test.data[,3],tol=10^(-4))
W1.GFIX<-result.GFIX5$W1
W2.GFIX<-result.GFIX5$W2
B1.GFIX<-result.GFIX5$B1
B2.GFIX<-result.GFIX5$B2


#SG
result.SG<-net.SG(W.1,W.2,B.1,B.2,X=train.data[,1:2],Y=train.data[,3],j=5,eta=1.5,X.test=test.data[,1:2],Y.test=test.data[,3],iteration=2500)
W1.SG<-result.SG$W1
W2.SG<-result.SG$W2
B1.SG<-result.SG$B1
B2.SG<-result.SG$B2

result.GFIX5<-net.GFIX(W1.GFIX,W2.GFIX,B1.GFIX,B2.GFIX,X=train.data10000[,1:2],Y=train.data10000[,3],j=5,eta=0.8,X.test=test.data[,1:2],Y.test=test.data[,3],tol=10^(-4))
result.GFIX5$train
result.GFIX5$step

#SG
result.SG<-net.SG(W1.SG,W2.SG,B1.SG,B2.SG,X=train.data10000[,1:2],Y=train.data10000[,3],j=5,eta=1.5,X.test=test.data[,1:2],Y.test=test.data[,3],iteration=2500)
result.SG$test

#GFIX
#plotdata
lossdata<-data.frame(loss=c(result.GFIX5$loss.train[-1],result.GFIX5$loss.test[-1]),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GFIX.loss
errordata<-data.frame(error=c(result.GFIX5$train,result.GFIX5$test),group=as.factor(rep(c("train","test"),each=result.GFIX5$step)),step=rep(1:result.GFIX5$step,2))
GFIX.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GFIX.error
W1.test<-result.GFIX5$W1
W2.test<-result.GFIX5$W2
B1.test<-result.GFIX5$B1
B2.test<-result.GFIX5$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GFIX_bound_c.pdf",width=8,height=6)
bound
dev.off()

pdf<-pdf("HW3_GFIX_error_c.pdf",width=8,height=6)
GFIX.error
dev.off()
pdf<-pdf("HW3_GFIX_loss_c.pdf",width=8,height=6)
GFIX.loss
dev.off()

#SG
#fixed step
lossdata<-data.frame(loss=c(result.SG$loss.train[-1],result.SG$loss.test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.loss<-ggplot(data=data.frame(lossdata),aes(x=step,y=loss,colour=group))+geom_line()
GS.loss
errordata<-data.frame(error=c(result.SG$train[-1],result.SG$test),group=as.factor(rep(c("train","test"),each=result.SG$epoch)),step=rep(1:2500,2))
GS.error<-ggplot(data=data.frame(errordata),aes(x=step,y=error,colour=group))+geom_line()
GS.error
W1.test<-result.SG$W1
W2.test<-result.SG$W2
B1.test<-result.SG$B1
B2.test<-result.SG$B2
bounddata<-data.frame(X1=test.data[,1],X2=test.data[,2],Y=as.factor(forward.fun(W1.test,W2.test,B1.test,B2.test,data.matrix(test.data[,1:2]),test.data[,3])$a2>=0.5))
bound<-ggplot(data=bounddata,aes(x=X1,y=X2,colour=Y))+geom_point()
pdf<-pdf("HW3_GS_bound_c.pdf",width=8,height=6)
bound
dev.off()
pdf<-pdf("HW3_GS_error_c.pdf",width=8,height=6)
GS.error
dev.off()
pdf<-pdf("HW3_GS_loss_c.pdf",width=8,height=6)
GS.loss
dev.off()

