# install.packages("tidyverse")
# install.packages("plm")
# install.packages("lmtest")
# install.packages("AER")
# install.packages("haven")
# install.packages("ivreg")

library(tidyverse)
library(plm)
library(lmtest)
library(AER)
library(haven)
library(ivreg)

#-------------------------------------------------------------------------------
# 5주차 내용

data <- read_sas("hn22_all.sas7bdat")

#허리둘레
summary(data$HE_wc)
# 결측지는 NA로 나타남

#성별
table(data$sex)
#결측치 없음

# 수축기혈압
summary(data$HE_sbp)
# 결측지는 NA로 나타남

# 이완기혈압
summary(data$HE_dbp)
# 결측지는 NA로 나타남

# 공복혈당
summary(data$HE_glu)
# 결측지는 NA로 나타남

#HDL 콜레스테롤
summary(data$HE_HDL_st2)
# 결측지는 NA로 나타남

#중성지방
summary(data$HE_TG)
# 결측지는 NA로 나타남

# 대사증후군 더미변수 만들기
data$obese <- ifelse((data$sex==1 & data$HE_wc>=90)|(data$sex==2 & data$HE_wc>=85), 1, 0)
data$hbp <- ifelse(data$HE_sbp>=130 | data$HE_dbp>=85,1,0)
data$hglu <- ifelse(data$HE_glu>=100,1,0)
data$lhdl <- ifelse((data$sex==1 & data$HE_HDL_st2<40)|(data$sex==2 & data$HE_HDL_st2<50),1,0)
data$htg <- ifelse(data$HE_TG>=150,1,0)
data$count <- data$obese + data$hbp + data$hglu + data$lhdl + data$htg

table(data$count)
summary(data$count)
data$mbs <- ifelse(data$count>=3,1,0)
summary(data$mbs)



# 신체활동 여부, 일수, 시간, 분 (여가):
table(data$BE3_75)
table(data$BE3_76)
table(data$BE3_77)
table(data$BE3_78)

table(data$BE3_85)
table(data$BE3_86)
table(data$BE3_87)
table(data$BE3_88)

# 주간운동시간(분) 도출
data$hexer <- ifelse(data$BE3_75==1 & data$BE3_77!=99, data$BE3_76 * (data$BE3_77*60+data$BE3_78), ifelse(data$BE3_75==2,0, NA))
summary(data$hexer)
data$mexer <- ifelse(data$BE3_85==1 & data$BE3_87!=99, data$BE3_86 * (data$BE3_87*60+data$BE3_88), ifelse(data$BE3_85==2,0, NA))
summary(data$mexer)
data$exer <- data$hexer + data$mexer
#유산소 신체활동 실천율 : pa_aerobic
table(data$pa_aerobic)

#응답자 거주지역 (시도): region
table(data$region)
#응답자 만나이 : age
table(data$age)
#응답자 월평균 가구총소득:  ainc 
summary(data$ainc)
#응답자 학력: edu
table(data$edu)

#-------------------------------------------------------------------------------

unemp <- read.csv("실업자_및_실업률_성_시도_연령별__20241018163558.csv"
                  , encoding = "UTF-8", fileEncoding = "CP949")

# data <- read_sas("hn22_all.sas7bdat")

# 첫 번째 행을 컬럼명으로 설정
colnames(unemp) <- unemp[1, ]
unemp <- unemp[-1, ]  # 첫 번째 행을 삭제하여 데이터 정리

region_mapping <- c("서울특별시" = 1, "부산광역시" = 2, "대구광역시" = 3, "인천광역시" = 4, "광주광역시" = 5, 
                    "대전광역시" = 6, "울산광역시" = 7, "세종특별자치시" = 8, "경기도" = 9, "강원도" = 10, 
                    "충청북도" = 11, "충청남도" = 12, "전라북도" = 13, "전라남도" = 14, "경상북도" = 15, 
                    "경상남도" = 16, "제주특별자치도" = 17)


unemp$region <- region_mapping[unemp$행정구역별]


data1 <- merge(data, unemp, by = "region")

# unemp 데이터의 컬럼명 확인 (실제 컬럼명을 찾아줌)
colnames(unemp)

# 수정된 unemp 변수 설정
data1$unemp <- ifelse(data1$age > 19 & data1$age < 30, data1$`실업률_20~29세 (%)`,
                      ifelse(data1$age > 29 & data1$age < 40, data1$`실업률_30~39세 (%)`,
                             ifelse(data1$age > 39 & data1$age < 50, data1$`실업률_40~49세 (%)`,
                                    ifelse(data1$age > 49 & data1$age < 60, data1$`실업률_50~59세 (%)`,
                                           ifelse(data1$age > 59, data1$`실업률_60세이상 (%)`, NA)))))

# unemp 변수가 숫자형으로 변환 가능한지 확인
data1$unemp <- as.numeric(data1$unemp)

# 변환 후 요약 통계 확인
summary(data1$unemp)

# 회귀분석 실행
reg1 <- lm(exer ~ ainc + age + factor(sex) + factor(edu) + factor(region), data = data1)
summary(reg1)

# 1st stage regression
prereg <- lm(ainc ~ unemp + age + factor(sex) + factor(edu) + factor(region)
             , data=data1)
summary(prereg)


data1$predinc <- ifelse(!is.na(data1$ainc) & !is.na(data1$unemp)
                        , prereg$fitted.values, NA)
# 2nd stage regression
postreg <- lm(exer ~ predinc + age + factor(sex) + factor(edu) + factor(region)
              , data=data1)
summary(postreg)

# use ivreg()
ivreg <- ivreg(exer ~ ainc + age + factor(sex) + factor(edu) + factor(region)
               | unemp + age + factor(sex) + factor(edu) + factor(region)
               , data = data1)
summary(ivreg)

