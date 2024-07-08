library(tolerance)
#a1<-9.5;b1<-20.5;a2<-17.0;b2<-8.0
a1<-0.5;b1<-0.5;a2<-1.0;b2<-1.0

dpos<-function(x){
(x>0)*(x<=1)*beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)
}
dneg<-function(x){
(x<0)*(x>=-1)*beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)
}

done<-function(x){
if (x>0 & x<=1){beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)}
else if (x<0 & x>=-1) {beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)}
else if (x==0 & a1+a2>1 & b1+b2>1) {beta(a1+a2-1,b1+b2-1)/beta(a1,b1)/beta(a2,b2)}
else {0}
}

#x<-c(1:1000)/1000
#y1<-y2<-rep(-999,length(x))
#for (i in 1:length(x)){
#y1[i]<-dpos(x[i])
#y2[i]<-dneg(-x[i])
#}
#cbind(x,y1,y2)
#round(y1-y2,0.00001)

x<-c(-1000:1000)/1000
y<-z<-rep(-999,length(x))
for (i in 1:length(x)) y[i]<-done(x[i])
mn<-a1/(a1+b1)-a2/(a2+b2)
var=a1*b1/(a1+b1)^2/(a1+b1+1)+a2*b2/(a2+b2)^2/(a2+b2+1)
z<-dnorm(x,mean=mn,sd=sqrt(var))

plot(x,y,type="l")
lines(x,z,lty=2)


a1<-1.1;b1<-5.1;a2<-6.1;b2<-0.1

dpos<-function(x){
(x>0)*(x<=1)*beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)
}
dneg<-function(x){
(x<0)*(x>=-1)*beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)
}

done<-function(x){
if (x>0 & x<=1){beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)}
else if (x<0 & x>=-1) {beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)}
else if (x==0 & a1+a2>1 & b1+b2>1) {beta(a1+a2-1,b1+b2-1)/beta(a1,b1)/beta(a2,b2)}
else {0}
}

#x<-c(1:1000)/1000
#y1<-y2<-rep(-999,length(x))
#for (i in 1:length(x)){
#y1[i]<-dpos(x[i])
#y2[i]<-dneg(-x[i])
#}
#cbind(x,y1,y2)
#round(y1-y2,0.00001)

x<-c(-1000:1000)/1000
y<-z<-rep(-999,length(x))
for (i in 1:length(x)) y[i]<-done(x[i])
mn<-a1/(a1+b1)-a2/(a2+b2)
var=a1*b1/(a1+b1)^2/(a1+b1+1)+a2*b2/(a2+b2)^2/(a2+b2+1)
z<-dnorm(x,mean=mn,sd=sqrt(var))

plot(x,y,type="l")
lines(x,z,lty=2)


a<-1;b<-0.15
integrate(dneg,-a,-b); integrate(dpos,b,a)

r1<-rbeta(2000000,0.5,0.5)
r2<-rbeta(2000000,1.0,1.0)
mean(r1-r2 < -0.15)
mean(r1-r2 > 0.15)

x<-c(1500:10000)/10000
int<-0
for (i in 1:(length(x)-1)){
int<-int+0.5/10000*(dpos(x[i])+dpos(x[i+1]))
}
int

x<-c(1500:10000)/10000
int<-0
for (i in 1:(length(x)-1)){
int<-int+0.5/10000*(dneg(-x[i])+dneg(-x[i+1]))
}
int

library(cubature)
hcubature(dneg,-a,-b); hcubature(dpos,b,a); pcubature(dneg,-a,-b); pcubature(dpos,b,a)



