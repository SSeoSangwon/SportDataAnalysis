# install.packages("MatchIt")

library(MatchIt)

data <- read.csv("gamelog_class.csv")

# KBO 야구의 시프트 플레이 금지에 따른 차이 분석
# 좌타자 더미변수 만들기
# grepl 함수 - 조건에 맞는 행은 TRUE로 표시, 조건에 맞지 않는 행은 FALSE로 표시됨
data$leftbat <- ifelse(grepl("좌타", data$pitbat), 1, 0)
summary(data$leftbat)

# 스위치 히터 지우기
# subset 함수 - 원래있던 데이터셋에서 '추출'을 하기 때문에 부분집합이라는 개념
data <- subset(data, !grepl("양타", pitbat))

# 주자가 없는 상황만 분석
# 루 -> 주자 있음
data <- subset(data, !grepl("루", before))

# 출루 성공 1, 아웃 0 더미변수 만들기
# 안타, 루타, 4구, 사구, 홈런 -> 출루
table(data$outcome)
data$suc <- ifelse(grepl("안타", data$outcome)
                   | grepl("루타", data$outcome)
                   | grepl("4구", data$outcome)
                   | grepl("사구", data$outcome)
                   | grepl("홈런", data$outcome), 1, 0)

summary(data$suc)


# 처치군 왼손타자 / 통제군 오른손타자
# 처치 전 2023년 / 처치 후 2024년
# 인터랙션 항 만들기
data$left2024 <- ifelse(data$leftbat == 1 & data$year == 2024, 1, 0)

# 이중차이분석
# 종속변수는 rea와 wpa각각 사용해보자
# rea - Run Expectancy Added(출루예상기여도(?)) / wpa - Win Probability Added(승리확률기여도도)
did1 <- lm(rea ~ left2024 + factor(year) + factor(batcode1), data = data)
summary(did1)

did2 <- lm(wpa ~ left2024 + factor(year) + factor(batcode1), data = data)
summary(did2)

did3 <- lm(suc ~ left2024 + factor(year) + factor(batcode1), data = data)
summary(did3)

data$leftpull <- ifelse(data$leftbat == 1 & data$pull >= 60, 1, 0)
data$leftpull2024 <- ifelse(data$leftpull == 1 & data$year == 2024, 1, 0)

summary(data)

# 당겨치는 비율이 60% 이상인 좌타자와 우타자 제외외
data1 <- subset(data, leftbat == 0 | leftpull == 1)

did4 <- lm(rea ~ left2024 + factor(year) + factor(batcode1), data = data1)
summary(did4)

did5 <- lm(wpa ~ left2024 + factor(year) + factor(batcode1), data = data1)
summary(did5)

did6 <- lm(suc ~ left2024 + factor(year) + factor(batcode1), data = data1)
summary(did6)

# 통계적으로 유의하지 않음 -> why?

# Propensity Score Matching(성향 점수 매칭)

# 당겨치는 비율이 60% 이상인 좌타자와 가장 비슷한 우타자를 찾아보자
# 2024년에 시프트가 폐지되었으므로 2024년 데이터만 사용
data2024 <- subset(data1, year == 2024)

# 플레이 당 기록이 아닌 시즌 기록으로 분석
# unique 함수 - 중복된 항을 하나만 남기고 제거해주는 함수
data2024season <- data.frame(unique(data2024$batcode1))
colnames(data2024season) <- "batcode1"
batstat <- read.csv("batstat2024.csv")

psmbat <- merge(batstat, data2024season)
summary(psmbat)

# NA행 삭제
psmbat1 <- subset(psmbat, !is.na(pull))
match <- matchit(leftbat ~ age + avg + slg + war + pull, data = psmbat1, replace = TRUE, distance = 'logit')
summary(match)
