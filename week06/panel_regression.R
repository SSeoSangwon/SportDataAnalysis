# install.packages("tidyverse")
# install.packages("plm")
# install.packages("lmtest")
# install.packages("AER")

library(tidyverse)
library(plm)
library(lmtest)
library(AER)


klg <- read.csv("klgfull.csv")
summary(klg)

table(klg$round)

# Final A와 B는 34~38 라운드임
klg$wk <- ifelse(klg$round == "Final A" |
                   klg$round == "Final B" , klg$wk + 33, klg$wk)

# 수원 FC는 수원월드컵경기장을 사용하지 않는데 홈경기가 다 월컵구장으로 나옴
klg$stadium <- ifelse(klg$hometeam == "Suwon FC",
                      "Suwon Civil Stadium", klg$stadium)

# 홈팀 승리확률변수 만들기
klg$hwinprob <- 1 / klg$homeodds

# DV 관중 수, IV 홈 승리확률 회귀분석
reg1 <- lm(attendance ~ hwinprob, data = klg)
summary(reg1)

# 스플릿 라운드에 팬들이 더 많이 올까?
klg$split <- ifelse(klg$wk>33, 1, 0)
reg2 <- lm(attendance ~ hwinprob + split, data = klg)
summary(reg2)

summary(klg)



# year, month, day를 이용해 KLG 데이터에서 날짜 형식으로 변환
klg$date <- as.Date(paste(klg$year, klg$month, klg$day, sep = "-"))

# 날씨 데이터 로드(2023 서울 날씨...)
weather_data <- read.csv("2023weather.csv")

# 날씨 데이터의 'date' 열을 날짜 형식으로 변환
weather_data$date <- as.Date(weather_data$date)

# 날씨 데이터에서 필요한 열만 선택 (date, temp, rain)
weather_data <- weather_data %>%
  select(date, temp, rain)

# 'date' 열을 기준으로 KLG 데이터와 날씨 데이터를 병합 (Left Join)
klg <- merge(klg, weather_data, by = "date", all.x = TRUE)
klg[is.na(klg)] <- 0

# 기온과 강수량 포함해서 분석
reg3 <- lm(attendance ~ hwinprob + split + temp + rain, data = klg)
summary(reg3)

# 승리확률 제곱항 포함해서 분석
klg$hwinprobsq <- klg$hwinprob^2
reg4<-lm(attendance ~ hwinprob + hwinprobsq + split + temp + rain, data = klg)
summary(reg4)

# Log 관중 수
reg5<-lm(log(attendance) ~ hwinprob + hwinprobsq+ split+ temp + rain,
         data=klg)
summary(reg5)

# Log 관중 수
reg6<-lm(log(attendance) ~ hwinprob+ hwinprobsq + temp + rain
         +factor(hometeam)+factor(wk),
         data=klg)
summary(reg6)

# 패널회귀분석
panel<-plm(log(attendance) ~ hwinprob+ hwinprobsq + temp +rain
           +factor(wk),
           data=klg,index="hometeam", model="within")
summary(panel)

# 군집표준오차
panel_cl <- coeftest(panel, vcoc = vcovHC(panel, type = "sss", cluster = "group"))
panel_cl

# 경기장마다 최대 관중 수 가져오기
table(klg$stadium)

klg$capacity <- ifelse(klg$stadium=="Chuncheon Songam Stadium",20000,
                ifelse(klg$stadium=="Gangneung Stadium",21416,
                ifelse(klg$stadium=="Daejeon World Cup Stadium",43535,
                ifelse(klg$stadium=="DGB Daegu Bank Park",12469,
                ifelse(klg$stadium=="Gwangju Football Stadium",10007,
                ifelse(klg$stadium=="Jeju World Cup Stadium",29791,
                ifelse(klg$stadium=="Jeonju World Cup Stadium",34276,
                ifelse(klg$stadium=="Munsu Cup Stadium",37897,
                ifelse(klg$stadium=="Seoul World Cup Stadium",66704,
                ifelse(klg$stadium=="Steelyard Stadium",14268,
                ifelse(klg$stadium=="Sungui Arena Park",18989,
                ifelse(klg$stadium=="Suwon Civil Stadium",11808,
                ifelse(klg$stadium=="Suwon World Cup Stadium",43168,
                NA)))))))))))))
summary(klg$capacity)

# 최대 관중 수와 1000명 정도의 차이여도 만석인 것으로..
klg$att1<-ifelse(klg$capacity-klg$attendance<1000,klg$capacity,
                 klg$attendance)
klg$soldout <-ifelse(klg$capacity==klg$att1,1,0)
summary(klg$soldout)


klg$logcap <- log(klg$capacity)

# Tobit 분석
tobit <- AER::tobit(log(att1) ~ hwinprob + hwinprobsq + temp + rain
                    + factor(wk) + factor(hometeam)
                    , data = klg, right = klg$logcap)

## CSE reg3tobit ##
tobit_cs <- lmtest::coeftest(tobit,vcov. = sandwich::vcovCL,
                             cluster=klg$hometeam,type="HC1")
tobit_cs


