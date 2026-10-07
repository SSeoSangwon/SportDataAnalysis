# 필요한 패키지 설치
# install.packages("dplyr")
# install.packages("stargazer")

library(dplyr)
library(stargazer)

# 데이터셋 불러오기
mlb_pitch <- read.csv("mlb_pitch_lecture.csv")

# 특정 열 선택
mlb_pitch1 <- select(mlb_pitch, c(Tm, W, L, ERA, WHIP, year))

# 총 경기 수 계산 (경기 수 = 승리 + 패배)
mlb_pitch1$games <- mlb_pitch1$W + mlb_pitch1$L

# 승률 계산 (승률 = 승리 / 총 경기 수)
mlb_pitch1$wpg <- mlb_pitch1$W/mlb_pitch1$games

# 수정된 데이터셋의 요약 통계 출력
summary(mlb_pitch1)

# ifelse를 사용하여 조건에 따라 값 수정
mlb_pitch1$L <- ifelse(mlb_pitch1$Tm == "MIN" & mlb_pitch1$year == 2016, 
                       -mlb_pitch1$L, mlb_pitch1$L)

mlb_pitch1$W <- ifelse(mlb_pitch1$Tm == "NYY" & mlb_pitch1$year == 2018, 
                       100, mlb_pitch1$W)

# 새로운 변수 생성: 팀이 뉴욕에 있으면 nyc = 1, 그렇지 않으면 0
mlb_pitch1$nyc <- ifelse(mlb_pitch1$Tm == "NYY" | mlb_pitch1$Tm == "NYM", 1, 0)

# 총 경기 수와 승률 다시 계산
mlb_pitch1$games <- mlb_pitch1$W + mlb_pitch1$L
mlb_pitch1$wpg <- mlb_pitch1$W/mlb_pitch1$games

# 수정된 데이터셋의 요약 통계 다시 출력
summary(mlb_pitch1)

# 요약 통계 결과를 HTML 파일로 출력
stargazer(mlb_pitch1,                           # 출력할 데이터셋
          title = "Descriptive Statistics",     # 표 제목
          type = "html",                        # 파일 형식 (HTML)
          out = "Descriptive Statistics.html")  # 출력 파일 이름


# 예시 데이터셋 생성
id <- c(1:10)
sex <- rep(c("Female", "Male"), each = 5)
학번 <- c(20, 22, 21, 20, 18, 19, 22, 20, 20, 19)
age <- c(22, 20, 20, 23, 24, 24, 21, 22, 23, 23)
height <- c(165, 158, 161, 190, 178, 172, 175, 182, 168, 162)
weight <- c(52, 48, 59, 85, 83, 72, 73, 82, 64, 50)

df <- data.frame(id, sex, 학번, age, height, weight, stringsAsFactors = FALSE)

df

# df 수정: id 값을 10 증가
df1 <- df
df1$id <- df1$id + 10

# rbind를 사용하여 df와 df1 결합
dffull <- rbind(df, df1)

# 두 데이터 프레임의 열 수와 열 이름이 같아야 함

# 출신 지역 데이터셋 생성
home <- data.frame(id = c(1:10), home = c("서울","인천","경기","부산","광주",
                                          "대전","서울","대구","강원","제주"))

# df와 home 데이터셋을 id를 기준으로 병합
dfhome <- merge(df, home, index = "id")
dfhome


# 각 출신 지역별 인구 데이터 생성
pop <- data.frame(home = c("서울","인천","경기","부산","광주","대전","대구",
                           "강원","제주"), pop = c(1000, 500, 700, 300, 
                                               150, 100, 120, 80, 30))

# dfhome과 pop 데이터셋을 home을 기준으로 병합
dfpop <- merge(dfhome, pop, index = "home")
dfpop


# 일부 출신 지역에 인구 데이터가 없는 경우 처리
pop1 <- data.frame(home = c("서울","인천","경기","부산","광주","대전",
                            "대구","강원"), pop = c(1000, 500, 700, 300, 150, 
                                                100, 120, 80))

# all.x = TRUE를 사용하여 dfhome의 모든 레코드를 유지하는 left join 수행
dfpop1 <- merge(dfhome, pop1, index = "home")
dfpop1
dfpop1 <- merge(dfhome, pop1, index = "home", all.x = TRUE)
dfpop1

# 하나의 출신 지역에 서로 다른 인구 값을 가진 경우 예시
pop2 <- data.frame(home = c("서울","서울","인천","경기","부산","광주","대전",
                            "대구","강원","제주"), pop = c(1000, 800, 500, 700,
                                                     300, 150, 100, 120, 80, 30))

dfpop2 <- merge(dfhome, pop2, index = "home")
dfpop2

# mlb_pitch1 데이터셋을 CSV 파일로 저장
write.csv(mlb_pitch1, "mlb_pitch1.csv",row.names = FALSE)

# ------------------------------------------------------------------------------

# 1. 타자 데이터 불러오기
batter <- read.csv("mlb_batter.csv")

# 2. 결측치 또는 이상치 확인
summary(batter)  # 결측치 또는 이상치가 존재하는지 확인

# 결측치 확인 방법
colSums(is.na(batter))

# 3-1. 각 팀의 시즌별 승률 계산 및 추가
batter$games <- batter$W + batter$L  # 경기 수 추가
batter$win_rate <- batter$W / batter$games  # 승률 추가

# 3-2. 승률에서 이상치가 있는지 확인
summary(batter$win_rate)  # 승률에서 이상치가 존재하는지 확인

# 4-1. 각 팀의 단타 수 계산
batter$X1B <- batter$H - (batter$X2B + batter$X3B + batter$HR)

# 4-2. 단타 수에서 이상치가 있는지 확인
summary(batter$X1B)

# 5. 기술 통계 분석 결과 외부 파일로 저장
stargazer(batter, type = "text", out = "dcp_batter.txt")

# 6. 타자 데이터와 투수 데이터를 병합
batter_selected <- select(batter, Tm, year, H, X1B, X2B, X3B, HR)
merged_data <- merge(mlb_pitch1, batter_selected, by = c("Tm", "year"))

# 병합된 데이터 확인
head(merged_data)
