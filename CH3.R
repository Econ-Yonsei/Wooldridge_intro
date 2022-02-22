library(wooldridge)
library(tidyverse)

#C1
#(i) Not for R
#(ii) Not for R
#(iii)
b<-bwght
head(b)

reg1<-lm(bwght~cigs,data = b)
ggplot(b, aes(x=cigs, y=bwght))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='cigs',y='bwght') 
summary(reg1) #The report for results(sample size and R-squared)

#C2 https://stackoverflow.com/questions/47344850/scatterplot3d-regression-plane-with-residuals
#(i)
library(scatterplot3d)
h<-hprice1
head(h)
reg2<-lm(price~sqrft+bdrms, data = h)
summary(reg2)

#(ii)
reg2$coefficients[3] #coef of bdrms

#(iii)->Same as (ii)
reg2$coefficients[3]+reg2$coefficients[2]*140

#(iv)
summary(reg2) #R^2 is about 60%
#(v)
pred_price=reg2$coefficients[1]+reg2$coefficients[2]*2438+reg2$coefficients[3]*4
names(pred_price)<-NULL
pred_price

#(vi)
pred_price #the estimated price, actual price =300
300-pred_price #residual for this house

#C3
c<-ceosal2
head(c)

#(i)
reg3<-lm(lsalary~lsales+lmktval, data = c)
summary(reg3)

#(ii)
reg3_add<-lm(lsalary~lsales+lmktval+profits, data = c)
summary(reg3_add)

#(iii)
reg3_add2<-lm(lsalary~lsales+lmktval+profits+ceoten, data = c)
summary(reg3_add2)

#(iv)
cor(c$lmktval,c$profits)

#C4
a<-attend
head(a)

#(i)
summary(a$atndrte)
summary(a$priGPA)
summary(a$ACT)

#(ii)
reg4<-lm(atndrte~priGPA+ACT, data=a)
summary(reg4)

#(iii) Nothing for R

#(iv)
75.7+17.261*3.65+(-1.717)*20
result<-reg4$coefficients[1]+reg4$coefficients[2]*3.65+reg4$coefficients[3]*20
names(result)<-NULL; result

#(v)

stu_A<-reg4$coefficients[1]+reg4$coefficients[2]*3.1+reg4$coefficients[3]*21; names(stu_A)<-NULL

stu_B<-reg4$coefficients[1]+reg4$coefficients[2]*2.1+reg4$coefficients[3]*26; names(stu_B)<-NULL

stu_B-stu_A

#C5
w<-wage1
head(w)

reg5<-lm(educ~exper+tenure, data = w) #regression model educ on exper and tenure
r1<-reg5$residuals # the residuals in reg5
reg5_2<-lm(w$lwage~r1) #regression model lwage on r1
reg5_3<-lm(lwage~educ+exper+tenure, data = w)
##Check the coefficient on r1 and coefficient on educ in the reg5_3

summary(reg5)
summary(reg5_2)
summary(reg5_3)

#C6
w2<-wage2
head(w2)

#(i)
reg6<-lm(IQ~educ, data = w2)
summary(reg6)
del_1<-reg6$coefficients[2]
names(del_1)<-NULL
del_1

#(ii)
reg6_1<-lm(w2$lwage~w2$educ)
summary(reg6_1)
bet_1<-reg6_1$coefficients[2]
names(bet_1)<-NULL
bet_1

#(iii)
reg6_2<-lm(w2$lwage~w2$educ+w2$IQ)
summary(reg6_2)
bet_1_h<-reg6_2$coefficients[2]
names(bet_1_h)<-NULL
bet_2_h<-reg6_2$coefficients[3]
names(bet_2_h)<-NULL
bet_1_h;bet_2_h

#(iv)
(bet_1_h+bet_2_h*del_1)-bet_1 #approximately '0'

#C7 tab_model()->confidence level 볼 수 있음
m<-meap93
head(m)

#(i)
reg7<-lm(m$math10~m$lexpend+m$lnchprg)
summary(reg7)

#(ii)->Not for R

#(iii)
reg7_1<-lm(m$math10~m$lexpend)
summary(reg7_1)

coef_ex7<-reg7$coefficients[2] #the coeffcient on lexpend in the reg7
names(coef_ex7)<-NULL

coef_ex7_1<-reg7_1$coefficients[2] #the coeffcient on lexpend in the reg7_1
names(coef_ex7_1)<-NULL
coef_ex7;coef_ex7_1

#(iv)
cor(m$lexpend, m$lnchprg) 

#(v)->In the Latex, 11.164
reg7_v<-lm(m$lnchprg~m$lexpend)
6.23+(-0.305)*reg7_v$coefficients[2]

coef_ex7_1-(coef_ex7+reg7$coefficients[3]*cor(m$lexpend,m$lnchprg)*sd(m$lnchprg)/sd(m$lexpend)) #approximately '0'

#C8
d<-discrim
head(d)

#(i)
mean(d$prpblck, na.rm = 'TRUE')
mean(d$income, na.rm = 'TRUE')
sd(d$prpblck, na.rm = 'TRUE')
sd(d$income, na.rm = 'TRUE')

#(ii)
reg8<-lm(d$psoda~d$prpblck+d$income)
summary(reg8)

#(iii)
reg8_1<-lm(d$psoda~d$prpblck)
summary(reg8_1)

#(iv)
reg8_2<-lm(d$lpsoda~d$prpblck+d$lincome)
summary(reg8_2)
reg8_2$coefficients[2]*0.2*100

#(v)
reg8_3<-lm(d$lpsoda~d$prpblck+d$lincome+d$prppov)
summary(reg8_3)
reg8_3$coefficients[2]

#(vi)
prppov_xna<- d %>% filter(!is.na(prppov)) %>% select(prppov)
lincome_xna<- d %>% filter(!is.na(lincome)) %>% select(lincome)
cor(prppov_xna, lincome_xna)

#(vii) Not for R

#C9
ch<-charity 
head(ch)

#(i)
reg9<-lm(ch$gift~ch$mailsyear+ch$giftlast+ch$propresp)
summary(reg9)

reg9_s<-lm(ch$gift~ch$mailsyear)
summary(reg9_s)

#(ii), (iii) Not for R

#(iv)
reg9_av<-lm(ch$gift~ch$mailsyear+ch$giftlast+ch$propresp+ch$avggift)
summary(reg9_av)

#(v)-> Not for R

#C10, 
library(haven) #import the dta file
htv<-read_dta("C:/Users/USER/Desktop/Textbooks/STATA/data/HTV.DTA")
head(htv)

#(i)
htv %>% select(educ) %>% summary
num_edu12<-htv %>% select(educ) %>% filter(educ=='12') %>% count %>% as.numeric
num_edu12/1230*100 #percentage of men completed twelfth grade but no higher grade

htv %>% select(c(educ, fatheduc, motheduc)) %>% summary

#(ii)
reg10_2<-lm(htv$educ~htv$motheduc+htv$fatheduc)
summary(reg10_2)

#(iii)
reg10_3<-lm(htv$educ~htv$motheduc+htv$fatheduc+htv$abil)
summary(reg10_3)

#(iv)
abil_sq<-(htv$abil)^2
reg10_4<-lm(htv$educ~htv$motheduc+htv$fatheduc+htv$abil+abil_sq)
summary(reg10_4)

D<-D(expression(educ=8.240+0.190*motheduc+0.109*fatheduc+0.401*abil+0.0051*abil^2), 'abil') # differentiate the equation with respect to 'abil'
polyroot(c(0.401, 0.0051*2)) #find the solution

#(v) Not for R

#(vi) fitted()->함수 사용 가능
abil<-sort(htv$abil)
educ=8.240+0.190*12.18+0.109*12.45+0.401*abil+0.0051*abil^2
plot(educ~abil, type='l')

#C11
me<-meapsingle

#(i)
reg11<-lm(me$math4~me$pctsgle)
summary(reg11)


#(ii)
reg11_2<-lm(me$math4~me$pctsgle+me$lmedinc+me$free)
summary(reg11_2)

#(iii)
cor(me$lmedinc, me$free) 

#(iv)->Not for R

#(v)
reg11_pctsgle<-lm(me$pctsgle~me$lmedinc+me$free) #make the regression
reg11_lmedinc<-lm(me$lmedinc~me$pctsgle+me$free)
reg11_free<-lm(me$free~me$pctsgle+me$lmedinc)

summary(reg11_pctsgle) #check the R^2
summary(reg11_lmedinc)
summary(reg11_free)

R_squared_pctsgle<-0.8258
R_squared_lemdinc<-0.7572
R_squared_free<-0.6863
R_squared<-c(R_squared_pctsgle, R_squared_lemdinc, R_squared_free)

#VIF function
VIF<-function(x){
  1/(1-x)
}
VIF(R_squared) #VIF of pctsgle, lemdinc , and free

#C12
e<-econmath
head(e)
#(i)
max(e$score) #no one
mean(e$score)
mean(e$actmth, na.rm = 'TRUE') #actmth
sd(e$actmth, na.rm = 'TRUE')
mean(e$acteng, na.rm = 'TRUE') #acteng
sd(e$acteng, na.rm = 'TRUE')

#(ii)
reg12<-lm(e$score~e$colgpa+e$actmth+e$acteng)
summary(reg12)


#(iii)
actmth_xna<- e %>% filter(!is.na(actmth)) %>% select(actmth)
acteng_xna<- e %>% filter(!is.na(acteng)) %>% select(acteng)
cor(actmth_xna,acteng_xna)

#(iv) Not for R
