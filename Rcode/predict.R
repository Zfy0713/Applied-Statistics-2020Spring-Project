teeth2

train_id = sample(1:n,floor(2*n/3),replace = F)
train_data = teeth2[train_id,]
test_data = teeth2[-train_id,]

LC_train <- smcure(Surv(Time,censor)~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,
                     cureform=~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,model="ph",data=train_data,Var=FALSE)

gamma_train = LC_train$b


# Initial values for the single-index/Cox mixture cure model estimation
attach(train_data)
Z <- cbind(Age,Gender_id, BOP,Plaque,Pdmean,CALmean)
gamma.init <- glm(censor~Z,family=binomial(link=logit),data=train_data)$coefficients
beta.init <- coxph(Surv(Time,censor)~Z,subset = censor!=0,data = train_data,method = "breslow")$coef 

# EM algorithm
SICmm.estim1 <- SIC(Time=Time,Status=censor,gamma.init,beta.init,Z,Z,LCMM=LCmm.estim,eps=1e-4,emmax=200,dataset=train_data,rescale=TRUE)

SICmm.estim1$uncured

