# install.packages("lmtest")
# install.packages("sandwich")
# install.packages("tidyverse")

library(lmtest)
library(sandwich)
library(tidyverse)

# 키 몸무게 데이터 읽기
h_w <- read.csv("weight-height.csv")

# 키와 몸무게 단위를 cm와 kg으로 변환
h_w$w_kg <- with(h_w, Weight/2.205)
h_w$h_cm <- with(h_w, Height*2.54)
# plot 그리기
plot(h_w$h_cm, h_w$w_kg)


#회귀분석

reg <- lm(w_kg ~ h_cm, data = h_w)
summary(reg)

#유의하지 않는 변수 만들기
h_w$z <- rnorm(nrow(h_w), mean = mean(h_w$w_kg), sd = 10)

plot(h_w$z, h_w$w_kg, col="black", pch=16, xlim=c(0,150), ylim=c(0,150))

reg1 <- lm(w_kg ~ z, data=h_w)
summary(reg1)




reg2 <- lm(h_cm ~ w_kg, data=h_w)
summary(reg2)

h_w$e <- rnorm(nrow(h_w), mean=0, sd=150)
h_w$y <- -337.7 * h_w$h_cm +  h_w$h_cm* h_w$h_cm + h_w$e
reg3 <- lm(y ~ h_cm , data=h_w)
summary(reg3)

plot(h_w$h_cm, h_w$y)

h_w$h_cmsq <- h_w$h_cm^2
reg4 <- lm(y ~ h_cmsq , data=h_w)
summary(reg4)

h_w$exer <- 5 + 0.3 * h_w$w_kg + h_w$z
h_w$exer <- ifelse(h_w$exer<0, 0, h_w$exer)

reg5 <- lm(w_kg ~ h_cm + exer, data = h_w)
summary(reg5)

#회귀분석

reg <- lm(w_kg ~ h_cm, data = h_w)
summary(reg)

#강건표준오차
library(lmtest)
library(sandwich)
coeftest(reg, vcov = vcovHC(reg, type = 'HC0'))


#남녀 더미변수 만들기

h_w$male <- ifelse(h_w$Gender=="Male", 1, 0)
# 더미변수를 포함한 회귀분석
reg5 <- lm(w_kg ~ h_cm + male, data = h_w)
summary(reg5)

plot(h_w$h_cm, h_w$w_kg)
plot(h_w$h_cm, h_w$w_kg, col=ifelse(h_w$male==1,"red","black"))
# 지역변수 가짜로 만들기
h_w <- h_w %>% 
  mutate(city=sample(rep(c("seoul", "busan", "daegu", "gwangju"), length=n())))

# 지역이 네개이므로 더미변수 세개 만들기
# 제외된 그룹이 레퍼런스

h_w$busan <- ifelse(h_w$city=="busan", 1, 0)
h_w$daegu <- ifelse(h_w$city=="daegu", 1, 0)
h_w$gwangju <- ifelse(h_w$city=="gwangju", 1, 0)

# 더미변수를 포함한 회귀분석
reg8 <- lm(w_kg ~ h_cm + male + busan + daegu + gwangju, data = h_w)
summary(reg8)

#factor 함수를 쓰면 한번에 할 수 있음
#이때 레퍼런스 그룹은 자동으로 생성

reg9 <- lm(w_kg ~ h_cm + male + factor(city), data = h_w)
summary(reg9)

