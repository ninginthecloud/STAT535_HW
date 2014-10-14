####################
#STAT 535 HW2      #
####################
library(MASS)#for multinormal distribution
library(ggplot2)# for plot
##problem one
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
#Kpool<-c(1,3,7)
BNum=30
mean.l<-NULL;
mean.L<-NULL;
sd.l<-NULL;
sd.L<-NULL;
V<-NULL;
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
V<-c(V,V.fun(Y))
mean.l<-c(mean.l,mean(l.b))
mean.L<-c(mean.L,mean(L.b))
sd.l<-c(sd.l,sd(l.b))
sd.L<-c(sd.L,sd(l.b))
}

#generate data frame
l.plot <- data.frame(K.l=Kpool,l.hat=mean.l,se.l = sd.l)
L.plot <- data.frame(K.L=Kpool,L=mean.L,se.L = sd.L)
V.plot <- data.frame(K.v=Kpool,V=V)
#ggplot2
limits1 <- aes(ymax = l.hat + se.l, ymin=l.hat-se.l)
p <- ggplot(data=l.plot, aes(x=K.l,y=l.hat))+
	geom_point()+
	geom_line(colour="red",size=0.7)+
	geom_errorbar(limits1,colour="blue",size=.7,width=.25)

limits2 <- aes(ymax = L + se.L, ymin=L-se.L)
q <- ggplot(data=L.plot, aes(x=K.L,y=L))+
	geom_point()+
	geom_line(colour="orange",size=0.7)+
	geom_errorbar(limits2,colour="blue",size=.7,width=.25)

v <- ggplot(data=V.plot, aes(x=K.v,y=V))+
	geom_point()+
	geom_line(colour="pink",size=0.7)
pdf<-pdf("HW2_1a.pdf",width=10,height=8)
p
dev.off()

pdf<-pdf("HW2_1b.pdf",width=10,height=8)
q
dev.off()

pdf<-pdf("HW2_1c.pdf",width=10,height=8)
v
dev.off()

total<-data.frame(K=rep(Kpool,3),l=c(mean.l,mean.L,V),se=c(sd.l,sd.L,rep(0,length(Kpool))),group=as.factor(rep(c("training error","test error","Variance"),each=length(Kpool))))
combine<-ggplot(data=total,aes(x=K,y=l,colour=group))+
		geom_point(aes(x=K,y=l,colour=group))+
		geom_line(size=1)+
		geom_errorbar(aes(ymin=l-se,ymax=l+se,colour=group),width=.25)+
		geom_abline(intercept = 0.054799, slope = 0,size=1.2,colour="orange",linetype="dashed")
pdf<-pdf("HW2_1d.pdf",width=10,height=8)
combine
dev.off()		
		
#Bayes error
1/2*((1-pnorm(0,mean=-1.6,sd=1))+pnorm(0,mean=1.6,sd=1))
#0.05479929
#combined plot

##problem 2
#read data .dat type
wkdata<-read.table("E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW2\\hw2-1d-train.dat") 
dim(wkdata)
#estimate p=P(Y=1)
p.hat<-mean(wkdata[,2]==1)
#estimate mu+ and mu_
mu.positive<-mean(wkdata[wkdata[,2]==1,1])
mu.negative<-mean(wkdata[wkdata[,2]==-1,1])

#boundary for LDA
theta_g<-(log((1-p.hat)/p.hat)+1/2*(mu.positive^2-mu.negative^2))/(mu.positive-mu.negative)#0.1245507

#linear classifier
#s=1, theta-L
emprical.error<-function(x,theta){
Y.est<-c(-1,1)[(x[,1]>=theta)+1]
return(mean(Y.est!=x[,2]))
}

err<-NULL;
Thetapool<-seq(-2,5,.001)
for(i in Thetapool){
err<-c(err,emprical.error(wkdata,i))
}
err.data<-data.frame(Theta=Thetapool,error=err)
theta_L<-err.data[which.min(err.data[,2]),1]
err.plot<-ggplot(data=err.data,aes(y=error,x=Theta))+
		geom_line(colour="#0066FF",size=1)
pdf<-pdf(file="HW2_2a.pdf",width=10,height=8)
err.plot
dev.off()

#distribution
X<-seq(-2.5,2.5,0.01)
P1<-1/(1+2*exp(-4*X))
P2<-1-1/(1+2*exp(-4*X))

plot.data<-data.frame(x=rep(X,2),y=c(P1,P2),group=rep(c("P(Y=+1|X)","P(Y=-1|X)"),each=length(X)))
plot.dist<-ggplot(data=plot.data,aes(x=x,y=y,colour=group))+
geom_line(size=1)
pdf<-pdf("HW2_d1.pdf",width=10,height=8)
plot.dist
dev.off()

X1<-seq(-2.5,log(2)/4,.01)
X2<-seq(log(2)/4,2.5,0.01)
P1<-1/(1+2*exp(-4*X1))
P2<-1-1/(1+2*exp(-4*X2))
plot.data<-data.frame(x=c(X1,X2),y=c(P1,P2),group=rep(c("P(Y=+1|X)","P(Y=-1|X)"),c(length(X1),length(X2))))
plot.dist<-ggplot(data=plot.data,aes(x=x,y=y,colour=group))+
geom_line(size=1)
pdf<-pdf("HW2_d2.pdf",width=10,height=8)
plot.dist
dev.off()
theta_s<-log(2)/4
#
X<-seq(-6,6,0.01)
p<-1/3
mu1<--2
mu2<-2
P1<-dnorm(X,mu1,1)
P2<-dnorm(X,mu2,1)

plot.data<-data.frame(x=rep(X,2),y=c((1-p)*P1,p*P2),group=rep(c("(1-p)g_","pg+"),each=length(X)))
plot.dist<-ggplot(data=plot.data,aes(x=x,y=y,colour=group))+
geom_line(size=1)+
geom_vline(xintercept=mu1,linetype="dotted",size=1)+
geom_vline(xintercept=mu2,linetype="dotted",size=1)+
geom_vline(xintercept=theta_g,linetype="dashed")+
geom_vline(xintercept=theta_L,linetype="dotdash")+
geom_vline(xintercept=theta_s,linetype="longdash")
pdf<-pdf("HW2_2f.pdf",width=10,height=8)
plot.dist
dev.off()
#outlier
new.data<-rbind(wkdata,c(100,1))
p.hat.new<-mean(new.data[,2]==1)
mu.positive.new<-mean(new.data[new.data[,2]==1,1])
mu.negative.new<-mean(new.data[new.data[,2]==-1,1])
err.new<-NULL;
Thetapool<-seq(-2,5,.01)
for(i in Thetapool){
err.new<-c(err.new,emprical.error(new.data,i))
}
err.data.new<-data.frame(Theta=Thetapool,error=err.new)


(log((1-p.hat.new)/p.hat.new)+1/2*(mu.positive.new^2-mu.negative.new^2))/(mu.positive.new-mu.negative.new)#0.5721825
##proble three
# 1-NN
point1<-data.frame(x1=c(-.5,.5,.5,.5,1.5),x2=c(.5,-.5,.5,1.5,.5),z=as.factor(c(-1,-1,+1,-1,-1)))

plot<-ggplot()+
	geom_point(data=point1,aes(x=x2,y=x1,colour=z,size=5))+
	geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0),linetype="dotted",size=1)+
	geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1),linetype="dotted",size=1)
pdf<-pdf("1nn.pdf",width=10,height=8)
plot
dev.off()

#3-NN
center.x<-function(centered,shift){
return(c(centered-shift,centered+shift,centered+shift,centered-shift))}
center.y<-function(centered,shift){
return(c(centered-shift,centered-shift,centered+shift,centered+shift))}
s<-0.1
point1<-data.frame(x1=c(center.x(-.5,s),center.x(.5,s),center.x(.5,s),center.x(.5,s),center.x(1.5,s)),x2=c(center.y(.5,s),center.y(-.5,s),center.y(.5,s),center.y(1.5,s),center.y(.5,s)),z=as.factor(rep(c(-1,-1,1,-1,-1),each=4)))

plot<-ggplot()+
	geom_point(data=point1,aes(x=x2,y=x1,colour=z,size=5))+
	geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0),linetype="dotted",size=1)+
	geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1),linetype="dotted",size=1)
pdf<-pdf("3nn.pdf",width=10,height=8)
plot
dev.off()

#naive bayesian classifier
plot<-ggplot()+
	geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0),linetype="dotted",size=1)+
	geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1),linetype="dotted",size=1)+
	geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1),linetype="dotted",size=1)+
	xlim(c(-1,2))+
	ylim(c(-1,2))
pdf<-pdf(file="naive bayesian classifier.pdf",width=10,height=8)
plot
dev.off()
	
