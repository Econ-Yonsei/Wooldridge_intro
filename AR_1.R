library(forecast)

set.seed(123456) #난수 생성

#AR(1), coefficient=0.5

AR1<-arima.sim(list(ar=c(0.5)), n=200)
ts.plot(AR1, main='AR(1)')
par(mfrow=c(1,2))
acf(AR1, main="ACF of AR(1)")
pacf(AR1, main='PACF of AR(1)')

#AR(1), coefficient=0.9
par(mfrow=c(1,1))
AR1<-arima.sim(list(ar=c(0.9)), n=200) #정상시계열은 이론적으로 맞으나 시도표로 확인하면 직관전으로 납득하기 어려움
ts.plot(AR1, main='AR(1)')
par(mfrow=c(1,2))
acf(AR1, main="ACF of AR(1)") #ACF가 지수적으로 감소하지 않음->정상시계열이 아님 (즉, 0.9는 랜덤워크에 가까움)
pacf(AR1, main='PACF of AR(1)')

#MA(1)
MA1<-arima.sim(list(ma=0.7), n=200)
ts.plot(MA1, main='MA(1)')
par(mfrow=c(1,2))
acf(MA1, main='ACF of MA(1)')
pacf(MA1, main='PACF of MA(1)')

#ARMA(1,1)
x<-arima.sim(list(ar=0.1,ma=-0.6),n=200)
ts.plot(x, main='x')
par(mfrow=c(1,2))
acf(x, main='ACF')
pacf(x, main='PACF')
