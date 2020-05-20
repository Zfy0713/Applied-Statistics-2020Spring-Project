library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(survival)
library(survminer)
library(smcure)
setwd("D:/2018+/2020spring/高级应用统计/proposal")
teeth2 <- read.csv("teethdata/Teeth_ILDoData_1.csv")

# teeth2 <- filter(teeth, tooth==2) ## select data with tooth=2

# dim(teeth2)
# str(teeth2)
# head(teeth2)
# names(teeth2)
teeth2 = rename(teeth2,  censor = event..1...fail..0...cens., ## rename
                Time = time..years.) 

# teeth2 = select(teeth2, c(id, censor,Time,Plaque,Pdmean,CALmean,Age,Gender))
teeth2 = select(teeth2, -c(id, tooth, Mobility, Molar.Tooth., D.F.sites))

str(teeth2)
attach(teeth2)
# Estimation of a logistic/Cox mixture cure model
LCmm.estim <- smcure(Surv(Time,censor)~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,
                     cureform=~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,model="ph",data=teeth2,Var=FALSE)

# Bootstrap of LC
nboot <- 20
inb <- 0
ir <- 1
n <- dim(teeth2)[1]
gamma_boot <- matrix(0,ncol = 7,nrow=nboot)
beta_boot <- matrix(0,ncol = 6,nrow=nboot)
while (inb < nboot) {
  set.seed(ir)
  id <- sample(1:n,n,replace=TRUE)
  bootdata <- teeth2[id,]
  boot.est <- smcure(Surv(Time,censor)~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,
                     cureform=~Age+Gender_id+BOP+Plaque+Pdmean+CALmean,model="ph",data=bootdata,Var=FALSE)
  gamma_boot[inb,] = boot.est$b
  beta_boot[inb,] = boot.est$beta
  inb = inb + 1
  ir = ir + 1
}
gamma_se <- sqrt(apply(gamma_boot,2,var))
gamma_zvalue <- LCmm.estim$b/gamma_se
gamma_pvalue <- (1-pnorm(abs(gamma_zvalue)))*2
beta_se <- sqrt(apply(beta_boot,2,var))
beta_zvalue <- LCmm.estim$beta/beta_se
beta_pvalue <- (1-pnorm(abs(beta_zvalue)))*2


###---------------------------------------------------------------------------------
# Initial values for the single-index/Cox mixture cure model estimation
Z <- cbind(Age,Gender_id, BOP,Plaque,Pdmean,CALmean)
gamma.init <- glm(censor~Z,family=binomial(link=logit),data=teeth2)$coefficients
beta.init <- coxph(Surv(Time,censor)~Z,subset = censor!=0,data = teeth2,method = "breslow")$coef 

# EM algorithm
SICmm.estim1 <- SIC(Time=Time,Status=censor,gamma.init,beta.init,Z,Z,LCMM=LCmm.estim,eps=1e-4,emmax=200,dataset=teeth2,rescale=TRUE)

SICmm.estim1$b # incidence
LCmm.estim$b

SICmm.estim1$beta #latency
LCmm.estim$beta

# Estimation + bootstrap
SICmm.estim2 <- SICMM(Time=Time,Status=censor,Z,Z,LCMM=LCmm.estim,eps=1e-4,emmax=200,dataset=teeth2,rescale=TRUE,bootstrap=T,nboot=20)


SICmm.estim2$b
SICmm.estim2$beta
