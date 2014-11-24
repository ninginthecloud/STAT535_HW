############
#STAT535 HW6
############
library(ggplot2)
#problem one
#k-means with Power Initialization
onestep<-function(Data,center){
	n<-dim(Data)[1];
	k<-dim(center)[1];
	distance<-NULL;
	for(i in 1:k){
		distance<-cbind(distance,sqrt((Data[,1]-center[i,1])^2+(Data[,2]-center[i,2])^2))	
	}
	label=apply(distance,1,function(x){which.min(x)})
	return(label)
}
PI<-function(Data,K,c=1){
	n=dim(Data)[1]
	#Power Initialization
	Kprime=round(c*K*log(K));
	threthold=n/(exp(1)*Kprime);
	center.prime=Data[sample(1:n,Kprime),];
	label=onestep(Data,center.prime);
	remain.center=center.prime[table(label)>=threthold,]
	k=dim(remain.center)[1];k
	mu<-matrix(rep(0,K*dim(Data)[2]),nrow=K)
	mu[1,]<-data.matrix(remain.center[sample(1:k,1),]);
	for(i in 2:K){
		d<-NULL;
		for(j in 1:(i-1)){
		d=cbind(d,sqrt(apply((remain.center-matrix(rep(mu[j,],each=k),ncol=dim(Data)[2]))^2,1,sum)));
		}
		min.d=apply(d,1,function(x){min(x)})
		mu[i,]<-data.matrix(remain.center[which.max(min.d),])
	}
	return(mu)
}
NI<-function(Data,K){
	n=dim(Data)[1];
	mu=Data[sample(1:n,K),];
	return(data.matrix(mu));
}

Kmeans<-function(Data,K,iteration=100,c=1,method){
if(method=="PI"){center=PI(Data,K,c)}
if(method=="NI"){center=NI(Data,K)}
L<--999;
iter<-1;
while(iter<=iteration){
label=onestep(Data,center)
cost=0
for(k in 1:K){
group=Data[label==k,]
center[k,]=apply(group,2,mean)
cost=cost+sum(((group[,1]-center[k,1])^2+(group[,2]-center[k,2])^2))
}
L<-c(L,cost);
if(abs(L[iter+1]-L[iter])<1e-6){break;}
iter=iter+1;
}
return(list(cluster=label,center=center,L=L[-1]))
}


##########
#data
data1=read.table(file="E:\\UW\\autumn 2014\\STAT535\\HW\\STAT535_HW\\HW6\\hw6-cluster5-data1000.dat")
colnames(data1)<-c("x1","x2")
#plot
raw=ggplot(data=data1,aes(x=x1,y=x2))+geom_point(color="blue")+ggtitle("original data")
pdf<-pdf("HW6_1.pdf",width=8,height=6)
raw
dev.off()


result=Kmeans(data1,K=4,method="PI");
result; 
plotdata<-cbind(data1,cluster=as.factor(result$cluster))
center=data.frame(result$center)
PI=ggplot()+geom_point(data=plotdata,aes(x=x1,y=x2,color=cluster))+ggtitle("K=4 Power Initialization")+
	geom_point(data=center, aes(x=X1,y=X2),shape=18,color="black",size=3)
PI

pdf<-pdf("HW6_1.pdf",width=8,height=6)
PI
dev.off()


result=Kmeans(data1,K=4,method="NI");
result; 
plotdata<-cbind(data1,cluster=as.factor(result$cluster))
center=data.frame(result$center)
NI=ggplot()+geom_point(data=plotdata,aes(x=x1,y=x2,color=cluster))+ggtitle("K=4 Naive Initialization")+
	geom_point(data=center, aes(x=x1,y=x2),shape=18,color="black",size=3)
NI
pdf<-pdf("HW6_1.pdf",width=8,height=6)
NI
dev.off()


#Problem two


plot(x=1:length(result$L),y=result$L)
