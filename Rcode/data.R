library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(survival)
library(survminer)
library(smcure)
setwd("D:/2018+/2020spring/高级应用统计/proposal")
teeth <- read.csv("teethdata/Teeth_ILDoData.csv")

teeth2 <- filter(teeth, tooth==2) ## select data with tooth=2
  
# dim(teeth2)
# str(teeth2)
# head(teeth2)
# names(teeth2)
teeth2 = rename(teeth2,  censor = event..1...fail..0...cens., ## rename
                Time = time..years., Plaque=Plaque., Diabetes = Diabetes..Y.N.,
                Tobacco = Tobacco.Use)

# teeth2 = select(teeth2, c(id, censor,Time,Plaque,Pdmean,CALmean,Age,Gender))
teeth2 = select(teeth2, -c(id, tooth, Mobility,Missing., Implant, Molar.Tooth., D.F.sites))

str(teeth2)
head(teeth2)
# censor      Time BOP.   Plaque   Pdmean  CALmean    Crown Filled.    Decayed. Age Gender    Diabetes           Tobacco
#      0 1.2000000    0 16.66667 2.166667 2.166667 No Crown  Filled     Decayed  33   Male No Diabetes Never Had Tobacco
#      0 0.3726027    0  0.00000 2.333333 2.333333    Crown  Filled Not Decayed  56   Male No Diabetes Never Had Tobacco
#      0 4.8136986    0 50.00000 2.333333 2.333333 No Crown  Filled Not Decayed  64 Female No Diabetes Never Had Tobacco
#      0 1.1287671   50  0.00000 3.666667 3.666667 No Crown  Filled Not Decayed  64   Male No Diabetes       Had Tobacco
#      0 4.7369863    0 33.33333 2.333333 2.333333    Crown  Filled Not Decayed  67   Male No Diabetes Never Had Tobacco
#      0 0.1917808    0  0.00000 3.166667 4.666667 No Crown  Filled Not Decayed  57   Male No Diabetes       Had Tobacco

a <- ggplot(data=teeth2, aes(x=Age,y=Time))
a + geom_point(aes(color = factor(censor)), size=1.5) + 
  scale_color_manual(values=c("#6495ED","#FFA500")) ## censor rate

rate = teeth2$censor
rate = 1 - sum(rate)/length(rate)

## survival curve
Sur = Surv(time = teeth2$Time, event = teeth2$censor)
sfit <- survfit(Surv(Time, censor)~ Crown, data=teeth2)
ggsurvplot(sfit)


## segment in Time
qq <- quantile(teeth2$Time, seq(0, 1, 0.25), na.rm = TRUE)
Time.seg = mutate(teeth2, T.quint = cut(Time,qq)) %>% group_by(T.quint)
Time.seg = na.omit(Time.seg)
summarise(Time.seg, Pd.mean = mean(Pdmean), CA.mean = mean(CALmean), Age.mean = mean(Age))

ggplot(data = Time.seg, aes(x = T.quint, y = CALmean, fill = factor(censor))) + geom_boxplot()
ggplot(data = Time.seg, aes(x = T.quint, y = Pdmean, fill = T.quint)) + geom_boxplot()
ggplot(data = Time.seg, aes(x = T.quint, y = Age, fill = T.quint)) + geom_boxplot()
ggplot(data=Time.seg, aes(x = T.quint, y = Time, fill = Tobacco)) + geom_boxplot()
ggplot(Time.seg, aes(x = T.quint, y = Time, fill = Crown)) + geom_boxplot()
ggplot(Time.seg, aes(x = T.quint, y = Time, fill = Diabetes)) + geom_boxplot()
ggplot(Time.seg, aes(x = T.quint, y = Time, fill = Gender)) + geom_boxplot()

## segment in Age
qq <- quantile(teeth2$Age, seq(0,1,0.2), na.rm = T)
Age.seg <- mutate(teeth2, Age.quint = cut(Age, qq)) %>% 
  group_by(Age.quint)
Age.seg <- na.omit(Age.seg)
Age.seg

p = ggplot(Age.seg, aes(x = Age.quint, fill = factor(censor))) 
p1 = p + geom_boxplot(aes(y = Pdmean)) + scale_fill_discrete(name="censor") + xlab("Age")
p2 = p + geom_boxplot(aes(y = CALmean)) + scale_fill_discrete(name="censor") + xlab("Age")

q = ggplot(Age.seg, aes(x = factor(Gender), fill = factor(censor)))
p3 = q + geom_boxplot(aes(y = Pdmean)) + scale_fill_discrete(name="censor") + xlab("Gender")
p4 = q + geom_boxplot(aes(y = CALmean)) + scale_fill_discrete(name="censor") + xlab("Gender")
grid.arrange(p1,p2,p3,p4,nrow = 2)


ggplot(teeth2, aes(x = Gender, fill = factor(Tobacco))) + geom_bar(position = 'dodge') +
  scale_fill_discrete(name = "Tobacco")

f1 = ggplot(Age.seg, aes(x = Age.quint, fill = Tobacco)) + geom_bar(position = 'dodge')
f2 = ggplot(Age.seg, aes(x = Age.quint, fill = Crown)) + geom_bar(position = 'dodge')
f3 = ggplot(Age.seg, aes(x = Age.quint, fill = Filled.)) + geom_bar(position = 'dodge')

