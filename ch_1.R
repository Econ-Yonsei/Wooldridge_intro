library(wooldridge)
library(stargazer)
## C1
w<-wage1
View(w)

#(i)
s<-summary(w$educ)

s[4] #average of educ
s[1] #min of educ
s[6] #max of educ

#(ii)
average_wage_1976<-mean(wage1$wage)
average_wage_1976

#(iii)

# 1. 월별 데이터 추출

CPI<-read.csv('C:/Users/USER/Desktop/Textbooks/Econometrics/wooldridge/CPI_US/data/cpi.csv') #Source:https://datahub.io/core/cpi-us#resource-cpi-us_zip

# 2. 패키지 설치 / #지우고 코드 실행

#install.packages(tidyr)
library(tidyr)

View(CPI) # 데이터 확인 (월별로 분리됨)

#Separate Date(Year, Month, Date)

cpi<-separate(CPI, "Date", c("Year", "Month", "Day"), sep = "-") 

#각 연도별 월 데이터 평균->연도별 데이터로 전환

cpi<-aggregate(cpi$Index, list(cpi$Year), FUN=mean)

#Graph
library(ggplot2)
names(cpi)=c('Year', 'Index') # change the names of columns in cpi

ggplot(cpi, aes(x=Year, y=Index)) + geom_point(color="green", size=4) + geom_line(aes(x=Year, y=Index, group = 1),color="green", size=2) + ggtitle("CPI for US in 1976~2013 / Source:U.S. Bureau of Labor Statistics")

cpi[38,2] #cpi in 2013
cpi[1,2] #cpi in 1976
#(iv)
average_wage_2013=(cpi[38,2]/cpi[1,2])*average_wage_1976 #cpi[38,2]=cpi in 2013, cpi[1,2]=cpi in 1976
average_wage_2013

#(v)
library(dplyr)
num_of_women=sum(w$female)
num_of_women #num of women

num_of_all=count(w, 'wage')
num_of_all=as.numeric(num_of_all[1,2])
num_of_men=num_of_all-num_of_women #num of men
num_of_men

## C2
b<-bwght

#(i)
num_of_men_2=sum(b$male)
num_of_men_2 #num of men

num_of_all_2=count(b, 'faminc')
num_of_all_2=as.numeric(num_of_all_2[1,2])
num_of_women_2=num_of_all_2-num_of_men_2 #num of women
num_of_women_2

female_cigs<-subset(b, male=='0', select = cigs)
female_cigs<-subset(female_cigs, cigs!='0')
count(female_cigs) #num of smoke dur preg

#(ii)
average_num_cigs<-mean(b$cigs) #average num of smoking per day
average_num_cigs
boxplot(b$cigs, xlab='Num of outliers: 212', ylab='frequency of cigs' ) #boxplot of cigs
a<-which(b$cigs > fivenum(b$cigs)[3] + 1.5*IQR(b$cigs)) #outliers
length(a) #num of outlier 

#(iii)
female_cigs<-subset(b, male=='0', select = cigs)
average_female_num_cigs<-mean(female_cigs$cigs) #mean female cigs
average_female_num_cigs
boxplot(female_cigs$cigs, xlab='Num of outliers: 112', ylab='frequency of cigs' ) #boxplot of cigs
z<-which(female_cigs$cigs > fivenum(female_cigs$cigs)[3] + 1.5*IQR(female_cigs$cigs)) #outliers
length(z) #num of outlier 

#(iv)
average_fatheduc<-mean(b$fatheduc, na.rm = TRUE) #mean of fatheduc, remove NA
average_fatheduc #mean of fatheduc
fatheduc_no_NA<-na.omit(b$fatheduc) #delete the NAs
length(fatheduc_no_NA) #num of data without NA

#(v)
average_faminc<-mean(b$faminc) #the average of family income
average_faminc
sd_faminc<-sd(b$faminc) #sd of family income
sd_faminc

#C3
m<-meap01
#(i)
s2<-summary(m$math4) #summary table
s2
s2[1] #the smallest value
s2[6] #the largest value

boxplot(m$math4)

#(ii)
perfect_math4<-subset(m, math4=='100')
count(perfect_math4) #perfect_pass_num
perfect<-as.numeric(count(perfect_math4))
perfect
total<-as.numeric(count(m))
total
perfect/total #percentage of perfect one

#(iii)
half_math4<-subset(m, math4=='50')
count(half_math4) #half_pass_num
half<-as.numeric(count(half_math4))
half

#(iv)
mean_math4<-mean(m$math4) #the average of math4
mean_read4<-mean(m$read4) #the average of read4
mean_math4
mean_read4 

#(v)
corr_m_r<-cor(m$math4,m$read4)
corr_m_r # correlation of math4 & read4
plot(m$math4,m$read4, xlab='math4', ylab='read4')

#(vi)
mean_exppp<-mean(m$exppp) #the average of exppp
mean_exppp
sd_exppp<-sd(m$exppp) #the sd of exppp
sd_exppp
hist(m$exppp, xlab='exppp',main = '') #histogram of exppp

#(vii)
100*(log(6000)-log(5000)) #difference of percentages

#C4
j<-jtrain2

#(i)
num_of_trained=sum(j$train)
num_of_trained #num of trained men
total_j<-as.numeric(count(j))
total_j #total men
num_of_trained/total_j #fraction

#(ii)
trained<-subset(j, train=='1')
untrained<-subset(j, train=='0')
trained_mean_earn78<-mean(trained$re78) #trained_mean_earn78
trained_mean_earn78 
untrained_mean_earn78<-mean(untrained$re78) #untrained_mean_earn78
untrained_mean_earn78
trained_mean_earn78-untrained_mean_earn78

#(iii)
trained_unem78<-subset(trained, unem78=='1') #trained but unemployed 
untrained_unem78<-subset(untrained, unem78=='1') #untrained and unemployed
count(trained_unem78) 
count(untrained_unem78)
num_of_trained
total_j-num_of_trained

#(iv)->Answered in latex

#C5
f<-fertil2
#(i)
summary(f$children)
summary(f$children)[1] #min
summary(f$children)[6] #max
summary(f$children)[4] #mean

#(ii)
total_f<-as.numeric(count(f)) #num of total
elect_w<-subset(f, electric=='1') #subset of having electrics
elect_f<-as.numeric(count(elect_w)) #num of having electrics
(elect_f/total_f) #percentage

#(iii)
elect_w_mean_child<-mean(elect_w$children) #mean of child in having electircs
unelect_w<-subset(f, electric=='0')
unelect_w_mean_child<-mean(unelect_w$children) #mean of child in not having electircs
elect_w_mean_child
unelect_w_mean_child 

#(iv)->Answered in latex.

#C6
c<-countymurders

#(i)
id<-subset(c, year=='1996')
length(id$countyid) #num of counties in 1996
id_zero<-subset(id, murder=='0')
num_id_zero<-as.numeric(count(id_zero)) #num_of counties having zero murders
num_id_zero
num_id<-as.numeric(count(id)) #num of total counties
num_id
id_nonexec<-subset(id, execs=='0')
num_id_nonexec<-as.numeric(count(id_nonexec))
num_id_nonexec 
100*num_id_nonexec/num_id #percentage of nonexecution

#(ii)
max(id$murders) #max of murders
summary(id$execs)
summary(id$execs)[6] #max of executions
summary(id$execs)[4] #mean of executions

#(iii)
cor(id$murders, id$execs) #correlation between two variables
plot(id$murders, id$execs, xlab='murders', ylab='execs')

#(iv)->Answered in latex.

#C7
al<-alcohol

#(i)
abused<-subset(al, abuse=='1') 
100*974/9822 #percentage of the abused
employed<-subset(al, status=='3')
100*8822/9822 #the employment rate

#(ii)
abused_employed<-subset(abused, status=='3')
100*850/974 #the employment rate in the abused

#(iii)
notabused<-subset(al, abuse=='0')
100*8848/9822 #the employment rate in the nonabused

#(iv)->Answered in latex. 