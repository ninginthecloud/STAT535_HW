##########
#STAT 535
##########
require(ggplot2)

#problem 1
#neural network
#function definition
##sigmoid function
phi<-function(u){
return(exp(u)/(1.0+exp(u)))}
##output function
network<-function(X,beta,beta0){
U<-crossprod(W,X)
Z<-phi(U)
return(beta0+crossprod(beta,Z))
}

#initialization
Beta<-rep(1,5)
beta0<--4.9
W<-20*matrix(c(1,0,2,2,2,1,1,0,-1,-.5,-1,1,-1,0,1),c(3,5),byrow=TRUE)
#input
#X<-c(1,x_1,x_2)

#simulate data
result<-data.frame(matrix(c(0,0,0),c(1,3)))
colnames(result)<-c("x1","x2","y")
for(x_1 in seq(-5,5,.1))
for(x_2 in seq(-5,5,.1)){
result<-rbind(result,c(x_1,x_2,(network(c(1,x_1,x_2),Beta,beta0))))
}
result<-result[-1,]
mean(result$y<0)
plot1 <- ggplot(result, aes(x=x1, y=x2, z = y))+
       stat_contour(breaks=0,colour=2,size=2)+
	   geom_text(data=NULL,x=1,y=0,label="D+:positive",size=9)+
	 geom_text(data=NULL,x=0,y=-1,label="D_:negative",size=9)
plot1
pdf<-pdf(file="HW1_1_1.pdf",width=10,height=8)
plot1
dev.off()
 
 
 beta0<--3.9
#simulate data
result<-data.frame(matrix(c(0,0,0),c(1,3)))
colnames(result)<-c("x1","x2","y")
for(x_1 in seq(-6,8,.1))
for(x_2 in seq(-6,8,.1)){
result<-rbind(result,c(x_1,x_2,(network(c(1,x_1,x_2),Beta,beta0))))
}
result<-result[-1,]
mean(result$y<0)
plot2 <- ggplot(result, aes(x=x1, y=x2, z = y))+
      stat_contour(breaks=0,colour=2,size=2)+
	  geom_text(data=NULL,x=1,y=0,label="D+:positive",size=9)+
	 geom_text(data=NULL,x=4,y=-2.5,label="D_:negative",size=9)
plot2 
pdf<-pdf(file="HW1_1_2.pdf",width=10,height=8)
plot2
dev.off()


#problem 2
##########
require(ggplot2)

#problem 1
#neural network
#function definition
##sigmoid function
phi<-function(u){
return(exp(u)/(1.0+exp(u)))}
##output function
network<-function(X,beta,beta0,W){
U<-crossprod(W,X)
Z<-phi(U)
return(beta0+crossprod(beta,Z))
}

#initialization
Beta<-rep(1,4)
beta0<--3.5
W<-20*matrix(c(0,1,0,1,1,-1,0,0,0,0,1,-1),c(3,4),byrow=TRUE)
#input
#X<-c(1,x_1,x_2)

#simulate data
result<-data.frame(matrix(c(0,0,0),c(1,3)))
colnames(result)<-c("x1","x2","y")
for(x_1 in seq(-.5,1.5,.01)){
        for(x_2 in seq(-.5,1.5,.01)){
             result<-rbind(result,c(x_1,x_2,(network(c(1,x_1,x_2),Beta,beta0,W))))
        }
}
result<-result[-1,]
mean(result$y<0)
plot1 <- ggplot(result, aes(x=x1, y=x2, z = y))+
	   geom_tile(aes(fill = y))+
	   stat_contour(breaks=0,colour=2,size=1)
	  # geom_text(data=NULL,x=1,y=0,label="D+:positive")+
	 #geom_text(data=NULL,x=0,y=-1,label="D_:negative")
plot1
pdf<-pdf(file="HW1_2_c.pdf",width=10,height=8)
plot1
dev.off()



	