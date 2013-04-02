#10.2
load(url("http://www.keihirose.com/spam.Rdata"))
#X.train,spam.train,X.test,spam.test

p<-mean(spam.train)	
q<-1-p 			

data.num<-length(X.train[,1])
word.num<-length(X.train[1,])

p.word<-c(1:word.num)
pn.word<-c(1:word.num)

for(i in 1:word.num){
 p.word[i]<-mean(X.train[spam.train[]==1,i])
 pn.word[i]<-mean(X.train[spam.train[]==0,i])
}

prob.after<-function(x,y){ 
 tmp<-1
 if(y==1){
  for(i in 1:word.num){
   if(x[i]==1){ 	tmp<-tmp*p.word[i]
   }else{ 		tmp<-tmp*(1-p.word[i]) }
  }
 }else{
  for(i in 1:word.num){
   if(x[i]==1){	tmp<-tmp*pn.word[i]
   }else{		tmp<-tmp*(1-pn.word[i])}
  }
 }
 return (tmp)
}

is.spam<-function(x){
 prob<-(prob.after(x,1)*p/(prob.after(x,0)*q+prob.after(x,1)*p))
 return(prob>0.5)
}

test.num<-length(spam.test[])

tru<-0
for(i in 1:test.num){
 if(is.spam(X.test[i,])==spam.test[i]) tru<-tru+1
}

err.rate<-1-(tru/test.num) 
err.rate