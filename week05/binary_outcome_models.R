# install.packages("tidyverse")
# install.packages("haven")
# install.packages("mfx")
# install.packages("moonBook")

library(tidyverse)
library(haven)
library(mfx)
library(moonBook)

# sas데이터 읽기 (haven 패키지 필요)
data <- read_sas("hn22_all.sas7bdat")

# 허리둘레(결측치는 NA로 나타남)
summary(data$HE_wc)

# 성별
table(data$sex)

# 수축기혈압
summary(data$HE_sbp)

# 이완기혈압
summary(data$HE_dbp)

# 공복혈당당
summary(data$HE_glu)

# HDL 콜레스테롤
summary(data$HE_HDL_st2)

# 중성지방
summary(data$HE_TG)

# 대사증후군 진단기준 중 3개 이상 충족 시 대사증후군(mbs)
data$case1 <- ifelse((data$sex == 1 & data$HE_wc >= 90) | 
                       (data$sex == 2 & data$HE_wc >= 85), 1, 0)

data$case2 <- ifelse((data$HE_sbp >= 130 | data$HE_dbp >= 85), 1, 0)

data$case3 <- ifelse((data$HE_glu >= 100), 1, 0)

data$case4 <- ifelse((data$sex == 1 & data$HE_HDL_st2 < 40) | 
                       (data$sex == 2 & data$HE_HDL_st2 < 50), 1, 0)

data$case5 <- ifelse((data$HE_TG >= 150), 1, 0)

data$mbs <- ifelse((data$case1 + data$case2 + data$case3 + 
                     data$case4 + data$case5) >= 3, 1, 0)

summary(data$mbs)

# 운동시간 계산
table(data$BE3_75)
table(data$BE3_76)
table(data$BE3_77)
table(data$BE3_78)

table(data$BE3_85)
table(data$BE3_86)
table(data$BE3_87)
table(data$BE3_88)

# 주간운동시간(분) 도출
data$BE3_76 <- ifelse(data$BE3_76 %in% c(8, 9), NA, data$BE3_76)
data$BE3_77 <- ifelse(data$BE3_77 %in% c(88, 99), NA, data$BE3_77)
data$BE3_78 <- ifelse(data$BE3_78 %in% c(88, 99), NA, data$BE3_78)
data$BE3_86 <- ifelse(data$BE3_86 %in% c(8, 9), NA, data$BE3_86)
data$BE3_87 <- ifelse(data$BE3_87 %in% c(88, 99), NA, data$BE3_87)
data$BE3_88 <- ifelse(data$BE3_88 %in% c(88, 99), NA, data$BE3_88)

# 주간 고강도 운동 시간(분) 계산
data$high_min <- ifelse(data$BE3_75 == 1,
                       data$BE3_76 * ((data$BE3_77 * 60) + data$BE3_78),
                       ifelse(data$BE3_75 == 2,
                              0,
                              NA))

# 주간 중강도 운동 시간(분) 계산
data$mid_min <- ifelse(data$BE3_85 == 1,
                       data$BE3_86 * ((data$BE3_87 * 60) + data$BE3_88),
                       ifelse(data$BE3_85 == 2,
                              0,
                              NA))

# 고강도와 중강도 운동 시간 합산
data$exer <- data$high_min + data$mid_min

summary(data$exer)

#
table(data$pa_aerobic)
table(data$region)
table(data$age)
summary(data$ainc)
table(data$edu)

# 선형확률모형 Linear Probability Model

reg <- lm(mbs ~ exer, data=data)
summary(reg)

reg1 <- lm(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region), data=data)
summary(reg1)

reg2 <- lm(mbs ~ pa_aerobic, data=data)
summary(reg2)

reg3 <- lm(mbs ~ pa_aerobic + ainc + age +factor(sex) + factor(edu) + factor(region), data=data)
summary(reg3)

probit1 <- glm(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region), family = binomial(link="probit"), data=data)
summary(probit1)

logit1 <- glm(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region), family = binomial(link="logit"), data=data)
summary(logit1)

probit2 <- glm(mbs ~ pa_aerobic + ainc + age + factor(sex) + factor(edu) + factor(region), family = binomial(link="probit"), data=data)
summary(probit2)

logit2 <- glm(mbs ~ pa_aerobic + ainc + age + factor(sex) + factor(edu) + factor(region), family = binomial(link="logit"), data=data)
summary(logit2)

# marginal effect at mean
probitmfx(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region), data =data)

logitmfx(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region),  data=data)

probitmfx(mbs ~ pa_aerobic + ainc + age + factor(sex) + factor(edu) + factor(region), data =data)

logitmfx(mbs ~ pa_aerobic + ainc + age + factor(sex) + factor(edu) + factor(region),  data=data)

# odds ratio


logitor(mbs ~ exer + ainc + age + factor(sex) + factor(edu) + factor(region),  data=data)


logitor(mbs ~ pa_aerobic + ainc + age + factor(sex) + factor(edu) + factor(region),  data=data)


# odds graph 1

ORplot(logit2)
# or 데이터 프레임으로 저장 
or <- data.frame(exp(cbind(OR = coef(logit2),
                           confint(logit2))))
# row name에 변수이름이 있음
or$var <- rownames(or)

# or 필요한 변수만 저장
or1 <- subset(or, var=="pa_aerobic" | var=="ainc" | var=="age")
colnames(or1) <- c("OR", "CIlow", "CIhigh", "var")


# odds ratio plot 그리기

(p <- ggplot(or1, aes(x = OR, y = var)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = CIhigh, xmin = CIlow), size = .5, height = 
                     .2, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    coord_trans(x = scales:::exp_trans(10)) +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") ) 


exertime <- 0:4000
tempdf <- data.frame(exertime)


# logistic 회귀 계수와 표준오차 가져오기
tempdf$coef <- logit1$coefficients[2]
tempdf$se <- summary(logit1)$coefficients[2,2]
# OR 계산
tempdf$or <- exp(tempdf$coef*tempdf$exertime)
# CI 계산
tempdf$cilow <- exp(tempdf$coef*tempdf$exertime
                    -1.96*(tempdf$se^2*tempdf$exertime^2)^(0.5))
tempdf$cihigh <-exp(tempdf$coef*tempdf$exertime
                    +1.96*(tempdf$se^2*tempdf$exertime^2)^(0.5))
# plot 그리기

plot(x=tempdf$exertime, y=tempdf$or, 
     type="l", pch=19, lwd=3, 
     col="black" , ylim=c(0, 1.1), xlim=c(0,4000),
     xlab="Exercise time", ylab="Odds Ratios")
lines(tempdf$exertime, tempdf$cilow, lwd=1, lty=2, type="l", col="red")
lines(tempdf$exertime, tempdf$cihigh, lwd=1, lty=2, type="l", col="red")
abline(h=1, untf=FALSE, lty=3, lwd=1)

# 고강도 운동과 중강도 운동을 따로?
logit3 <- glm(mbs ~ hexer + mexer + ainc + age + factor(sex) + factor(edu) + factor(region), family = binomial(link="logit"), data=data)
summary(logit3)

exertime <- 0:4000
tempdf <- data.frame(exertime)

# logistic 회귀 계수와 표준오차 가져오기
tempdf$coef <- logit3$coefficients[2]
tempdf$se <- summary(logit3)$coefficients[2,2]
# OR 계산
tempdf$or <- exp(tempdf$coef*tempdf$exertime)
# CI 계산
tempdf$cilow <- exp(tempdf$coef*tempdf$exertime
                    -1.96*(tempdf$se^2*tempdf$exertime^2)^(0.5))
tempdf$cihigh <-exp(tempdf$coef*tempdf$exertime
                    +1.96*(tempdf$se^2*tempdf$exertime^2)^(0.5))
# plot 그리기

plot(x=tempdf$exertime, y=tempdf$or, 
     type="l", pch=19, lwd=3, 
     col="black" , ylim=c(0, 1.1), xlim=c(0,4000),
     xlab="Exercise time", ylab="Odds Ratios")
lines(tempdf$exertime, tempdf$cilow, lwd=1, lty=2, type="l", col="red")
lines(tempdf$exertime, tempdf$cihigh, lwd=1, lty=2, type="l", col="red")
abline(h=1, untf=FALSE, lty=3, lwd=1)






















