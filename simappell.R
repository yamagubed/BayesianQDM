library(tolerance)
library(cubature)

t1<-Sys.time()

alpha1<-rep(1,3);beta1<-rep(1,3);alpha2<-c(1,4.5,2.75);beta2<-c(1,7.5,4.25)

pp<-array(rep(-9,24*21*3),dim=c(3,24,21))

for (h in 1:3){
  for (i in 0:23) {
    for (j in 0:20){
      a1<-alpha1[h]+i
      b1<-beta1[h]+23-i
      a2<-alpha2[h]+j
      b2<-beta2[h]+20-j
      done<-function(x){
        if (x>0 & x<=1){beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)}
        else if (x<0 & x>=-1) {beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)}
        else if (x==0 & a1+a2>1 & b1+b2>1) {beta(a1+a2-1,b1+b2-1)/beta(a1,b1)/beta(a2,b2)}
        else {0}
      }
      pp[h,i+1,j+1]<-pcubature(done,0.15,1)$integral
    } 
  }
}

p1<-c(0.35,0.45,0.50,0.55);p2<-0.35
th<-c(0.3,0.25,0.2)

for (i in 1:4){
  l<-dbinom(c(0:23),23,p1[i])%o%dbinom(c(0:20),20,p2)
  for (j in 1:3){
    for (k in 1:3){
      x<-sum(l*(pp[k,,]<th[j]))
      print(c(p1[i],p2,th[j],k,x))
    }
  }
}

t2<-Sys.time()
t2-t1

#library(xlsx)
#write.xlsx(pp[1,,], "c:/users/am00406060/onedrive - astellas pharma inc/A1009 and Cx601/pp.xlsx", sheetName = "beta(1,1)", append = F)
#write.xlsx(pp[2,,], "c:/users/am00406060/onedrive - astellas pharma inc/A1009 and Cx601/pp.xlsx", sheetName = "beta(4.5,7.5)", append = T)
#write.xlsx(pp[3,,], "c:/users/am00406060/onedrive - astellas pharma inc/A1009 and Cx601/pp.xlsx", sheetName = "beta(2.75,4.25)", append = T)



