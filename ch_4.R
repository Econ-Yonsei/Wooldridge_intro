library(wooldridge)
library(stargazer)
library(rsq)
library(car)
library(tidyverse)

#C1
v<-vote1
head(v)

#(i), (ii)->Not for R.

#correlation.matrix

#(iii)
reg1<-lm(voteA~lexpendA+lexpendB+prtystrA, data = v)
summary(reg1)

#(iv)
AB<-v$lexpendB-v$lexpendA
reg1_2<-lm (v$voteA~v$lexpendA+AB+v$prtystrA)
stargazer(reg1_2)

#C2 
l<-lawsch85
#(i)
reg2<-lm(l$lsalary~l$LSAT+l$GPA+l$llibvol+l$lcost+l$rank)
stargazer(reg2)

#(ii)->In the latex(individual test) https://m.blog.naver.com/pmw9440/221822183325
reg2_restricted<-lm(l$lsalary~l$llibvol+l$lcost+l$rank)
rsq(reg2, TRUE)
rsq(reg2_restricted, TRUE)
df1=136-6-1
df2=2
F_statistics_c2<-(rsq(reg2, TRUE)-rsq(reg2_restricted, TRUE))/(1-rsq(reg2, TRUE))*df1/df2
1-pf(F_statistics_c2,df1, df2)<0.05 #TRUE: the null hypothesis is rejected.
linearHypothesis(reg2, c("l$rank=0"))
#(iii)

l_non<-l 
faculty_mean<-mean(l$faculty,na.rm = TRUE)
l_non[is.na(l_non)]<-faculty_mean #결측치 변환
colSums(is.na(l_non)) #결측치 확인
reg2_3<-lm(l_non$lsalary~l_non$LSAT+l_non$GPA+l_non$GPA+l_non$llibvol+l_non$lcost+l_non$rank+l_non$clsize+l_non$faculty)
Z<-summary(reg2_3) #individual test
linearHypothesis(reg2_3, c("l_non$clsize=0","l_non$faculty=0"), test = "F")
cor(l_non$clsize, l_non$faculty) 
vif(reg2_3) #multicollinearity? 
plot(reg2_3$fitted.values,reg2_3$residuals) #residual & fitted value 

#(iv)->In the latex.

#C3 ggiraph, ggiraphExtra->2D에서 3변수 visualization
h<-hprice1
#(i)
A<-h$sqrft-150*h$bdrms # the variable for theta
reg3<-lm(h$lprice~A+h$bdrms)
stargazer(reg3)

#(ii)->In the latex.

#(iii)->In the latex.

#(iv)
confint(reg3, level=0.95)[2,] #confidence level 0.95

#C4
b<-bwght
reg4<-lm(b$bwght~b$cigs+b$parity+b$faminc)
summary(reg4)

#C5
m<-mlb1
#(i)
reg5<-lm(m$lsalary~m$years+m$gamesyr+m$bavg+m$hrunsyr+m$rbisyr) #the original one
summary(reg5)
reg5_rbisyr<-lm(m$lsalary~m$years+m$gamesyr+m$bavg+m$hrunsyr) #If we drop the rbisyr
summary(reg5_rbisyr)
vif(reg5) #the original one 
vif(reg5_rbisyr) #drop the rbisyr
reg5_1<-lm(m$rbisyr~m$years+m$gamesyr+m$bavg+m$hrunsyr) 
rsq(reg5_1)
plot(reg5)

#(ii)
reg5_2<-lm(m$lsalary~m$years+m$gamesyr+m$bavg+m$hrunsyr+m$runsyr+m$fldperc+m$sbasesyr)
stargazer(reg5_2) 
summary(reg5_2) #individual test
plot(reg5_2)

#(iii)
linearHypothesis(reg5_2, c("m$bavg=0","m$fldperc=0","m$sbasesyr=0"))

#C6
w<-wage2

#(i)->In the latex.
#(ii)
reg6<-lm(w$lwage~w$educ+w$exper+w$tenure)
linearHypothesis(reg6, c("w$exper=w$tenure")) #H0 is not rejected

#C7
t<-twoyear

#(i)
summary(t$phsrank)

#(ii)
reg7<-lm(t$lwage~t$jc+t$totcoll+t$exper+t$phsrank)
stargazer(reg7)
plot(reg7)

#(iii)
reg7_un<-lm(t$lwage~t$jc+t$totcoll+t$exper)
summary(reg7)
summary(reg7_un)

#(iv)
reg7_4.17<-lm(t$lwage~t$jc+t$univ+t$exper+t$id)
reg7_4.26<-lm(t$lwage~t$jc+t$totcoll+t$exper+t$id)
summary(reg7_4.17)
summary(reg7_4.26)

#C8
ks<-k401ksubs
#(i)
single<-subset(ks, fsize=='1')
count(single) # num of single-person households

#(ii)
reg8<-lm(single$nettfa~single$inc+single$age)
stargazer(reg8)
summary(reg8)
plot(reg8)

#(iii)->In the latex.

#(iv)
T_stat<-(0.843-1)/0.092 #test statistics
1-pt(T_stat, 2014)<0.01 #TRUE: the null hypothesis is rejected.

#(v)
reg8_5<-lm(single$nettfa~single$inc)
stargazer(reg8_5)
summary(reg8_5)
plot(reg8_5)
cor(single$inc, single$age)
#C9
d<-discrim

#(i)
reg9<-lm(d$lpsoda~d$prpblck+d$lincome+d$prppov)
stargazer(reg9) #H0 is rejected!
plot(reg9)

#(ii)
lincome<-d$lincome
prppov<-d$prppov
lincome_mean<-mean(d$lincome,na.rm = TRUE) #결측치 변환설정
prppov_mean<-mean(d$prppov,na.rm = TRUE)
lincome[is.na(lincome)]<-lincome_mean #결측치 변환
prppov[is.na(prppov)]<-prppov_mean
cor(lincome, prppov) #Correlation coefficient
vif(reg9)

#(iii)
reg9_3<-lm(d$lpsoda~d$prpblck+d$lincome+d$prppov+d$lhseval)
stargazer(reg9)
plot(reg9)

#(iv)
linearHypothesis(reg9_3, c('d$lincome=0','d$prppov=0'))
vif(reg9_3)

#(v)->In the latex.not yet.

#C10
e<-elem94_95
#(i)
reg10<-lm(e$lavgsal~e$bs)
stargazer(reg10)
plot(reg10)
summary(reg10)
T_stat2=(reg10$coefficients[2]-(-1))/0.14965 #H0: the coefficient on bs is -1.
1-pt(T_stat2,1846)< 0.01 #TRUE: the null hypothesis is rejected.

#(ii)
reg10_2<-lm(e$lavgsal~e$bs+e$lenrol+e$lstaff)
stargazer(reg10_2)
plot(reg10_2)

#(iii)
reg10_3<-lm(e$bs~e$lenrol+e$lstaff)
rsq(reg10_3)
summary(reg10_3)
vif(reg10_2)
var(reg10$residuals) # check the variance changed 
var(reg10_2$residuals)

#(iv)->In the latex.
#(v)
reg10_4<-lm(e$lavgsal~e$bs+e$lenrol+e$lstaff+e$lunch)
stargazer(reg10_4)
plot(reg10_4)

#(vi)->In the latex

#C11
htv<-htv
#(i)
abil_sq<-(htv$abil)^2
reg11<-lm(htv$educ~htv$motheduc+htv$fatheduc+htv$abil+abil_sq)
stargazer(reg11)
summary(reg11)

#(ii)
linearHypothesis(reg11, c("htv$motheduc=htv$fatheduc"))

#(iii), tuit17, tuit18
reg11_2<-lm(htv$educ~htv$motheduc+htv$fatheduc+htv$abil+abil_sq+htv$tuit17+htv$tuit18)
linearHypothesis(reg11_2, c("htv$tuit17=0","htv$tuit18=0"))

#(iv)
cor(htv$tuit17,htv$tuit18)
su<-(htv$tuit17+htv$tuit18)/2
reg11_3<-lm(htv$educ~htv$motheduc+htv$fatheduc+htv$abil+abil_sq+su)
stargazer(reg11_3)
plot(reg11_3)

#(v)->In the latex.

#C12
econ<-econmath

#(i)
reg12<-lm(econ$colgpa~econ$hsgpa+econ$actmth+econ$acteng)
stargazer(reg12)
plot(reg12)

#(ii)

#(iii)
linearHypothesis(reg12, c("econ$actmth=econ$acteng"))

#(iv)->In the latex.
