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
net.GFIX<-function(X,Y,j,eta,tol=10^(-4),iteration=50000){
#initialize
y<-as.numeric(Y>=0)
N = dim(X)[1]
p = dim(X)[2]
X<-data.matrix(X)
#W1 = matrix(rnorm(n=p*j,sd=1/sqrt(p)),c(j,p))#j*p
#W2 = matrix(rnorm(n=j*1,sd=1/sqrt(j)),c(1,j))#1*j
#B1 = rnorm(n=j)
#B2 = rnorm(n=1)
W1 = matrix(runif(n=p*j,c(-1,1)),c(j,p))
W2 = matrix(runif(n=j*1,c(-1,1)),c(1,j))
B1 = rnorm(n=j,c(-1,1))
B2 = rnorm(n=1,c(-1,1))
step=0
loss.vec=Inf#default the largest loss
while(step<=iteration)
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
}
return(list(loss=loss.vec,step=step,W1,W2,B1,B2,as.numeric(a2>=.5)))
}

#SG neural network
net.SG<-function(X,Y,j,eta,tol=10^(-4),iteration=30000){
#initialize
y<-as.numeric(Y>=0)
N = dim(X)[1]
p = dim(X)[2]
X<-data.matrix(X)
#W1 = matrix(rnorm(n=p*j,sd=1/sqrt(p)),c(j,p))#j*p
#W2 = matrix(rnorm(n=j*1,sd=1/sqrt(j)),c(1,j))#1*j
#B1 = rnorm(n=j)
#B2 = rnorm(n=1)
W1 = matrix(runif(n=p*j,c(-1,1)),c(j,p))
W2 = matrix(runif(n=j*1,c(-1,1)),c(1,j))
B1 = rnorm(n=j,c(-1,1))
B2 = rnorm(n=1,c(-1,1))
step=0
#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
loss.vec<-loss(a2,y)
while(step<iteration)
{
permutation<-sample.int(N,size=N,replace=FALSE)
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
W1=W1-gw1*eta
W2=W2-gw2*eta
B1=B1-gb1*eta
B2=B2-gb2*eta
#feedforward
Z1 = W1%*%t(X)+B1#j*N
a1 = sigmoid(Z1)#j*N
Z2 = W2%*%a1+B2#1*N
a2 = sigmoid(Z2)#1*N
	}
step=step+1
loss.vec<-c(loss.vec,loss(a2,y))
}
return(list(loss=loss.vec,step=step,W1,W2,B1,B2,as.numeric(a2>=.5)))
}


train.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-train-100.dat")
colnames(train.data)<-c("x1","x2","y")
test.data=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW3\\hw3-nn-test.dat")
colnames(train.data)<-c("x1","x2","y")

#GFIX
result.GFIX3<-net.GFIX(X=train.data[,1:2],Y=train.data[,3],j=3,eta=.05)
result.GFIX5<-net.GFIX(X=train.data[,1:2],Y=train.data[,3],j=5,eta=.05)
result.GFIX10<-net.GFIX(X=train.data[,1:2],Y=train.data[,3],j=10,eta=.05)
mean(result.GFIX3[[7]]!=as.numeric(train.data[,3]>0))
mean(result.GFIX5[[7]]!=as.numeric(train.data[,3]>0))
mean(result.GFIX10[[7]]!=as.numeric(train.data[,3]>0))

test.fun(W1,W2,B1,B2,X=test.data[,1:2],Y=test.data[,3])
#SG
result.SG<-net.SG(X=train.data[,1:2],Y=train.data[,3],j=5,eta=.15)
mean(result[[5]]!=as.numeric(train.data[,3]>0))
test.fun(W1,W2,B1,B2,X=test.data[,1:2],Y=test.data[,3])

#Newton 

###########
#plot area#
###########
orig.train100<-ggplot(data=data.frame(train.data),aes(x=x1,y=x2,colour=as.factor(y)))+geom_point()
orig.train100


#package 
a<-nnet(x=train.data[,1:2], y=as.numeric(train.data[,3]>=1), size=5,entropy = TRUE)
mean(as.numeric(a$fitted.values>=0.5)!=as.numeric(train.data[,3]>=0))



