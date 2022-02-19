library(wooldridge)
library(stargazer)
library(tidyverse)
#C1
k<-k401k #Define the data set
  
#(i)
parti_rate<-mean(k$prate) #the average of participation rate
parti_rate
match_rate<-mean(k$mrate) #the average of match rate
match_rate

#(ii)
# reg1<-lm(k$prate ~ k$mrate) #regression model
# plot(k$mrate, k$prate, xlab='mrate', ylab='prate')
# abline(reg1, col='red') #plot
# summary(reg1) #sample size=1534 R-squared=0.0747

#(ii)
reg1<-lm(prate~mrate, data = k) #Regression prate on mrate
ggplot(k, aes(x=mrate, y=prate))+geom_point(color='darkgreen')+geom_smooth(method = 'lm')+labs(x='mrate',y='prate') #A scatterplot and SRF for mrate & prate
summary(reg1) #The report for results(sample size and R-squared)

#(iii)->We can answer the question without using R.

#(iv)

83.0755+5.8611*(3.5) #It isn't reasonable because of the characteristics of data 'prate'.

#(v)->Answered in latex
SSE_p<- sum(predict(reg1)-mean(k$prate)) # SSE for prate

#C2
c<-ceosal2 #Define the data set

#(i)
mean(c$salary) #the average of salary
mean(c$ceoten) #the average of tenure

#(ii)
first_t<-subset(c, ceoten=='0')
length(first_t) #num of first conten
max(c$ceoten) #max of conten

#(ii)
c %>% filter(ceoten=='0') %>% length # # of CEOS in the first years
max(c$ceoten) #the longest tenure

##differentiate SRF with respect to ceoten->percentage of increase.

#(iii)
reg2<-lm(log(salary)~ceoten, data = c)
ggplot(c, aes(x=ceoten, y=log(salary)))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='ceoten',y='log(salary)') #A scatterplot and SRF for ceoten & log(salary)
summary(reg2) #The report for results(sample size and R-squared)

##the (approximate) predicted percentage increase in salary given one more year as a CEO
reg2$coefficients[2]*100

#C3
s<-sleep75
#(i)
reg3<-lm(sleep~totwrk, data=s)
ggplot(s, aes(x=totwrk, y=sleep))+geom_point(color='darkblue')+geom_smooth(method = 'lm')+labs(x='totwrk',y='sleep') #A scatterplot and SRF for totwrk & sleep
summary(reg3) #The report for results(sample size and R-squared)

#(ii)
reg3$coefficients[2]*2

#C4
w<-wage2 #regressand=wage, regressor=IQ

#(i)
mean(w$wage)
mean(w$IQ)
sd(w$IQ)

#(ii)
reg4<-lm(wage~IQ, data=w)
ggplot(w, aes(x=IQ, y=wage))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='IQ',y='wage') #A scatterplot and SRF for IQ & lwage
summary(reg4) #The report for results(sample size and R-squared)

##if IQ increases about 15, then wage changes....
reg4$coefficients[2]*15

## the variation in wage explained by IQ is...

SSE_w<- sum(predict(reg4)-mean(w$wage)) # SSE for wage(You can also check the R-squared)
SSE_w

#(iii)
reg4_1<-lm(lwage~IQ, data=w)
ggplot(w, aes(x=IQ, y=lwage))+geom_point(color='green')+geom_smooth(method = 'lm')+labs(x='IQ',y='log(wage)') #A scatterplot and SRF for IQ & log(wage)
summary(reg4_1) #The report for results(sample size and R-squared)

##the approximate percentage increase in predicted wage if IQ increases by 15 points
reg4_1$coefficients[2]*100*15 

#C5
r<-rdchem
#(i)-> There is nothing to do with using R.
#(ii)
reg5<-lm(lrd~lsales, data=r)
ggplot(r, aes(x=lsales, y=lrd))+geom_point(color='brown')+geom_smooth(method = 'lm')+labs(x='log(sales)',y='log(R&D)') #A scatterplot and SRF for log(sales) & log(rd)
summary(reg5) #The report for results(sample size and R-squared)

#C6
m<-meap93

#(i)->There is nothing to do with using R.

#(ii)->There is nothing to do with using R.

#(iii)
reg6<-lm(math10~lexpend, data = m)
ggplot(m, aes(x=lexpend, y=math10))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='log(expend)',y='math10') #A scatterplot and SRF for log(expend) & math10
summary(reg6) #The report for results(sample size and R-squared)

#(iv)->There is nothing to do with using R.

#(v)
max(predict(reg6)) #max of math10 in data set. 

#C7
ch<-charity

#(i)
mean(ch$gift)
ch %>% filter(gift=='0') %>% count->num_nogift
num_nogift/count(ch) * 100 ###Percentage of no gift

#(ii)
summary(ch$mailsyear) #find the mean, max and min

#(iii)
reg7<-lm(gift~mailsyear, data=ch)
ggplot(ch, aes(x=mailsyear, y=gift))+geom_point(color='green')+geom_smooth(method = 'lm')+labs(x='mailsyear',y='gift') #A scatterplot and SRF for mailsyear & gift
summary(reg7) #The report for results(sample size and R-squared)

#(iv)->There is nothing to do with using R.

#(v)
predict(reg7) %>% min #the smallest predicted charitable contribution in the sample


#C8

#(i)
x<-runif(500, min = 0, max = 10) #Uniform distribution(0,10), n=500
head(x)
mean(x)
sd(x)


#(ii)
u<-rnorm(500,0,36) #Normal distribution(0,36), n=500
mean(u) #Why isn't it zero??
sd(u)

#(iii)
y=1+2*x+u
reg8<-lm(y~x)
dat<-data.frame(x,y)
head(dat)

ggplot(dat, aes(x=x, y=y))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='x',y='y') #A scatterplot and SRF for x & y
summary(reg8) #The report for results(sample size and R-squared)

#(iv)
sum(reg8$residuals)
sum(reg8$residuals*x)

#(v)
sum(u)
sum(u*x)

#(vi)->Repeat (i)~(iii) with new random variables

#C9
county<-countymurders %>% filter(year=='1996')
head(county)

#(i)
county %>% filter(murders=='0') %>% count # # of zero murders
county %>% filter(murders!='0') %>% count # # of nonzero murders
county %>% select(execs) %>% max # the largest # of executions 

#(ii)
reg9<-lm(murders~execs, data = county)
ggplot(county, aes(x=execs, y=murders))+geom_point(color='darkgreen')+geom_smooth(method = 'lm')+labs(x='execs',y='murders') #A scatterplot and SRF for execs & murders
summary(reg9) #The report for results(sample size and R-squared)

reg9<-lm(county_1996$murders~county_1996$execs)
plot(county_1996$execs, county_1996$murders, xlab='execs',ylab='murders')
abline(reg9, col='red') #plot
plot(reg9) #check the residuals
stargazer(reg9) #check the observations and R^2

#(iii)->There is nothing to do with using R.

#(iv)->There is nothing to do with using R.

#(v)->There is nothing to do with using R.

#C10
cath<-catholic

#(i)
length(cath$id) #num of students
mean(cath$math12) #the average of math12
sd(cath$math12) #sd of math12

#(ii)
reg10<-lm(math12~read12, data = catholic)
ggplot(cath, aes(x=read12, y=math12))+geom_point(color='red')+geom_smooth(method = 'lm')+labs(x='read12',y='math12') #A scatterplot and SRF for read12 & math12
summary(reg10) #The report for results(sample size and R-squared)

#(iii), (iv), (v)->There is nothing to do with using R. 
