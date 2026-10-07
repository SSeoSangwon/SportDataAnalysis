# install.packages("XML")
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")
# install.packages("tidyverse")
# install.packages("stringi")
# install.packages("dplyr")
# install.packages("jsonlite")
# install.packages("xml2")

library(XML)
library(RSelenium)
library(rvest)
library(httr) 
library(stringr)
library(tidyverse)
library(stringi)
library(dplyr)
library(jsonlite)
library(xml2)

# # 기본 설정
# ch=wdman::chrome(port=4593L)
# # 크롬 드라이버 버전 확인
# binman::list_versions("chromedriver")
# # 크롬 드라이버 버전 설정
# remDr=remoteDriver(port=4593L, browserName='chrome', version="128.0.6534.0")
# # R에서 제어하는 크롬 창 열기
# remDr$open()
# # 네이버 방문
# remDr$navigate("https://naver.com")
# 
# # 뉴스스탠드에서 스포츠 클릭
# remDr$findElement(using = "xpath", value = '//*[@id="newsstand"]/div[1]/div/ul/li[3]/a')$clickElement()
# # 성균관대학교 검색
# remDr$findElement(using = "xpath", value = '//*[@id="query"]')$clickElement()
# remDr$findElement(using = "xpath", value = '//*[@id="query"]')$sendKeysToElement(list("성균관대학교", key="enter"))
# 
# # 페이지에서 표 가져오기
# 
# # 베팅 익스플로러 방문
# remDr$navigate("https://www.betexplorer.com/football/south-korea/k-league-1-2023/results/")
# 
# # 테이블 가져오기
# 
# oddstable <- read_html(remDr$getPageSource()[[1]])
# doc <- htmlParse(oddstable)
# table <- readHTMLTable(doc)
# # 몇 개의 표가 존재하는지 확인
# length(table)
# # 첫 번째 테이블이 필요한 테이블
# table1 <- readHTMLTable(doc, which=1, header=F)
# table2 <- data.frame(table1)
# 
# # 3시즌 동안의 K리그 베팅 데이터 수집 (2021-2023)
# year <- 2021:2023
# 
# url1 <- "https://www.betexplorer.com/football/south-korea/k-league-1-"
# url2 <- "/results/"
# 
# # map 함수 사용하여 반복문 작성
# paste0(url1,"2021",url2)
# 
# klgodd <- map(year, function(.x){
#   url <- paste0(url1,.x,url2) # URL 합치는 명령어
#   remDr$navigate(url)
#   # 로딩 대기 (3초)
#   Sys.sleep(3)
#   # 'Main' 클릭
#   remDr$findElement(using = "xpath", value = '//*[@id="sm-0-0"]/div/ul/li[1]/a')$clickElement()
#   # 로딩 대기 (3초)
#   Sys.sleep(3)
#   # 테이블 가져오기
#   
#   oddstable <- read_html(remDr$getPageSource()[[1]])
#   doc <- htmlParse(oddstable)
#   table <- readHTMLTable(doc)
#   # 몇 개의 표가 있는지 확인
#   length(table)
#   # 첫 번째 테이블이 필요한 테이블
#   table1 <- readHTMLTable(doc, which=1, header=F)
#   table2 <- data.frame(table1)
#   # 반복 중 확인용 날짜 프린트
#   print(.x)
#   # 반복 작업을 위해 do.call 사용
#   do.call(rbind, table2 )
# }  )
# 
# # map 함수 결과는 리스트 형태이므로 후처리가 필요
# # 데이터 프레임으로 변환
# klgodd <- data.frame(klgodd)
# # 행과 열이 반대로 되어 있으므로 행렬 변환
# klgodd <- as.data.frame(t(klgodd))
# # 행 이름에 불필요한 정보가 있으므로 삭제
# rownames(klgodd) <- NULL
# 
# klgodd1 <- subset(klgodd, !is.na(V6))
# # 대구 - 수원 블루윙즈 (예시 경기)
# 
# klgodd1$hometeam <- str_extract(klgodd1$V1, ".*(?=\\s\\-)")
# klgodd1$awayteam <- str_extract(klgodd1$V1, "(?<=\\-\\s).*")
# 
# klgodd2 <- subset(klgodd1, select = -V1)
# colnames(klgodd2)
# colnames(klgodd2) <- c("score", "homeodds", "drawodds", "awayodds", "date", "hometeam", "awayteam")
# # 특정 열 이름만 바꾸고 싶다면...
# # 'score'를 'result'로 변경, 'score'는 첫 번째 열
# colnames(klgodd2)[1] <- "result"
# # 열 순서 변경 시 문제 발생 가능, 다시 'score'로 변경
# colnames(klgodd2)[colnames(klgodd2) == "result"] <- "score"
# 
# # 방문한 페이지
# remDr$navigate("https://ticket.interpark.com/Community/Play/Talk/CommunityView.asp?bbsno=44&pageno=6&stext=&groupcode=24005863&sflag=&sort=&cate=01007&no=357629&groupno=357629&seq=0&isBest=&bestOnoff=")
# # 두 번째 방법으로 텍스트 가져오기
# page <- read_html(remDr$getPageSource()[[1]])
# 
# text <- page %>%
#   xml2::xml_find_all('/html/body/table/tbody/tr[6]/td') %>%
#   rvest::html_text()
# 
# # iframe 설정
# remDr$switchToFrame("ContentFr")
# page <- read_html(remDr$getPageSource()[[1]])
# 
# # 후기 가져오기
# text <- page %>%
#   xml2::xml_find_all('/html/body/table/tbody/tr[6]/td') %>%
#   rvest::html_text()

# ------------------------------------------------------------------------------

# **1. 대학입시 경쟁률 가져오기**

# 기본 설정
ch=wdman::chrome(port=4593L) 
# 크롬 드라이버 버전 확인
binman::list_versions("chromedriver")
# 크롬 드라이버 버전 설정
remDr=remoteDriver(port=4593L, browserName='chrome', version="128.0.6534.0") 
# R에서 제어하는 크롬 창 열기
remDr$open() 

# 1. 성균관대학교 입시 경쟁률 가져오기
remDr$navigate("https://naver.com")
remDr$findElement(using = "xpath", value = '//*[@id="query"]')$sendKeysToElement(list("성균관대학교 경쟁률", key = "enter"))
Sys.sleep(3)

# 페이지 소스 가져오기
page <- read_html(remDr$getPageSource()[[1]])

# 경쟁률 데이터 추출 (예: xpath 사용)
competition_rate_skku <- page %>%
  xml_find_all('//*[@id="main_pack"]/div[3]/div[2]/div/div/div[2]/div[1]/div/div/div/ul/li/div/div[1]') %>%
  html_text()

# 2. 서울대학교 입시 경쟁률 가져오기
remDr$navigate("https://naver.com")
remDr$findElement(using = "xpath", value = '//*[@id="query"]')$sendKeysToElement(list("서울대학교 경쟁률", key = "enter"))
Sys.sleep(3)

# 페이지 소스 가져오기
page <- read_html(remDr$getPageSource()[[1]])

# 경쟁률 데이터 추출
competition_rate_snu <- page %>%
  xml_find_all('//*[@id="main_pack"]/div[3]/div[2]/div/div/div[2]/div[1]/div/div/div/ul/li/div/div[1]') %>%
  html_text()

# 3. 연세대학교 입시 경쟁률 가져오기
remDr$navigate("https://naver.com")
remDr$findElement(using = "xpath", value = '//*[@id="query"]')$sendKeysToElement(list("연세대학교 경쟁률", key = "enter"))
Sys.sleep(3)

# 페이지 소스 가져오기
page <- read_html(remDr$getPageSource()[[1]])

# 경쟁률 데이터 추출
competition_rate_yonsei <- page %>%
  xml_find_all('//*[@id="main_pack"]/div[3]/div[2]/div/div/div[2]/div[1]/div/div/div/ul/li/div/div[1]') %>%
  html_text()

print(competition_rate_skku)
print(competition_rate_snu)
print(competition_rate_yonsei)

# 종료
remDr$close()
ch$stop()

# 2. klgodd2 데이터에서 스코어를 홈팀 스코어, 원정팀 스코어로 나누기

# 기본 설정
ch=wdman::chrome(port=4596L) 
# 크롬 드라이버 버전 확인
binman::list_versions("chromedriver")
# 크롬 드라이버 버전 설정
remDr=remoteDriver(port=4596L, browserName='chrome', version="128.0.6534.0") 
# R에서 제어하는 크롬 창 열기
remDr$open() 

# 베팅 익스플로러 방문
remDr$navigate("https://www.betexplorer.com/football/south-korea/k-league-1-2023/results/")

# 테이블 가져오기

oddstable <- read_html(remDr$getPageSource()[[1]])
doc <- htmlParse(oddstable)
table <- readHTMLTable(doc)
# 몇 개의 표가 존재하는지 확인
length(table)
# 첫 번째 테이블이 필요한 테이블
table1 <- readHTMLTable(doc, which=1, header=F)
table2 <- data.frame(table1)

# 3시즌 동안의 K리그 베팅 데이터 수집 (2021-2023)
year <- 2021:2023

url1 <- "https://www.betexplorer.com/football/south-korea/k-league-1-"
url2 <- "/results/"

# map 함수 사용하여 반복문 작성
paste0(url1,"2021",url2)

klgodd <- map(year, function(.x){
  url <- paste0(url1,.x,url2) # URL 합치는 명령어
  remDr$navigate(url)
  # 로딩 대기 (3초)
  Sys.sleep(3)
  # 'Main' 클릭
  remDr$findElement(using = "xpath", value = '//*[@id="sm-0-0"]/div/ul/li[1]/a')$clickElement()
  # 로딩 대기 (3초)
  Sys.sleep(3)
  # 테이블 가져오기
  
  oddstable <- read_html(remDr$getPageSource()[[1]])
  doc <- htmlParse(oddstable)
  table <- readHTMLTable(doc)
  # 몇 개의 표가 있는지 확인
  length(table)
  # 첫 번째 테이블이 필요한 테이블
  table1 <- readHTMLTable(doc, which=1, header=F)
  table2 <- data.frame(table1)
  # 반복 중 확인용 날짜 프린트
  print(.x)
  # 반복 작업을 위해 do.call 사용
  do.call(rbind, table2 )
}  )

# map 함수 결과는 리스트 형태이므로 후처리가 필요
# 데이터 프레임으로 변환
klgodd <- data.frame(klgodd)
# 행과 열이 반대로 되어 있으므로 행렬 변환
klgodd <- as.data.frame(t(klgodd))
# 행 이름에 불필요한 정보가 있으므로 삭제
rownames(klgodd) <- NULL

klgodd1 <- subset(klgodd, !is.na(V6))
# 대구 - 수원 블루윙즈 (예시 경기)

klgodd1$hometeam <- str_extract(klgodd1$V1, ".*(?=\\s\\-)")
klgodd1$awayteam <- str_extract(klgodd1$V1, "(?<=\\-\\s).*")

klgodd2 <- subset(klgodd1, select = -V1)
colnames(klgodd2)
colnames(klgodd2) <- c("score", "homeodds", "drawodds", "awayodds", "date", "hometeam", "awayteam")
# 특정 열 이름만 바꾸고 싶다면...
# 'score'를 'result'로 변경, 'score'는 첫 번째 열
colnames(klgodd2)[1] <- "result"
# 열 순서 변경 시 문제 발생 가능, 다시 'score'로 변경
colnames(klgodd2)[colnames(klgodd2) == "result"] <- "score"

# 홈팀 스코어:원정팀 스코어 구조
klgodd2$score[1]
# : . ? \\. \\? (정규 표현식 처리)
klgodd2$homescore <- str_extract(klgodd2$score, ".*(?=\\:)")
klgodd2$awayscore <- str_extract(klgodd2$score, "(?<=\\:).*")

# 3. klgodd2 데이터에서 날짜를 년, 월, 일로 나누기
klgodd2$date[1]
# 날짜.월.년도 구조
# . 은 정규식에서 모든 것을 뜻하므로 \\.으로 처리해야 함
# 숫자는 \\d로 표현
# 맨 처음은 ^, 맨 마지막은 $

klgodd2$year <- str_extract(klgodd2$date, "\\d\\d\\d\\d$")
klgodd2$day <- str_extract(klgodd2$date, "^\\d\\d")
klgodd2$month <- str_extract(klgodd2$date, "(?<=\\.)\\d\\d(?=\\.)")


# 4. FBREF 사이트에서 K 리그 21-23 시즌 경기 일정 가져오기

# https://fbref.com/en/comps/55/2021/schedule/2021-K-League-1-Scores-and-Fixtures
url1 <- "https://fbref.com/en/comps/55/"
url2 <- "/schedule/"
url3 <- "-K-League-1-Scores-and-Fixtures"

klgsch <- map(year, function(.x){
  url <- paste0(url1,.x,url2,.x, url3) # URL 합치는 명령어
  remDr$navigate(url)
  # 로딩 대기 (3초)
  Sys.sleep(3)
  # 테이블 가져오기
  
  oddstable <- read_html(remDr$getPageSource()[[1]])
  doc <- htmlParse(oddstable)
  table <- readHTMLTable(doc)
  # 몇 개의 표가 있는지 확인
  length(table)
  # 열 번째 테이블이 필요한 테이블
  table1 <- readHTMLTable(doc, which=10, header=F)
  table2 <- data.frame(table1)
  # 반복 중 확인용 날짜 프린트
  print(.x)
  # map 함수는 반복 작업을 수행하려면 다음 코드 필요
  do.call(rbind, table2 )
}  )

# 데이터 프레임으로 변환
klgsch <- data.frame(klgsch)
# 행과 열이 반대로 되어 있으므로 행렬 변환
klgsch <- as.data.frame(t(klgsch))
# 행 이름에 불필요한 정보가 있으므로 삭제
rownames(klgsch) <- NULL

# 빈 행 제거

klgsch1 <- subset(klgsch, V1!="" & V1!="Round")
# 11, 12, 13번째 열 필요 없으므로 제거
klgsch1 <- subset(klgsch1, select = -c(V11,V12, V13))
colnames(klgsch1)
colnames(klgsch1) <- c("round", "wk", "dow", "date", "time", "hometeam", "score","awayteam", "attendance", "stadium" )

# klgodd2와 klgsch1 병합
# 홈팀과 날짜가 같으면 같은 경기
# 홈팀과 날짜 표시 형식이 다른 것이 문제

# 날짜 형식 맞추기
# klgsch1에서는 년도(0000)-월(00)-일(00)의 구조

klgsch1$year <- str_extract(klgsch1$date, "^\\d\\d\\d\\d")
klgsch1$day <- str_extract(klgsch1$date, "\\d\\d$")
klgsch1$month <- str_extract(klgsch1$date, "(?<=\\-)\\d\\d(?=\\-)")


# 팀 이름 확인

table(klgodd2$hometeam)
table(klgsch1$hometeam)

# 대전, 서울, 김천, 인천, 제주, 수원, 울산 팀 이름이 다르게 나옴
# klgodd2 기준으로 klgsch1 수정
# ifelse 사용

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Daejeon Citizen FC", "Daejeon", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Daejeon Citizen FC", "Daejeon", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="FC Seoul", "Seoul", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="FC Seoul", "Seoul", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Sangju Sangmu", "Gimcheon Sangmu", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Sangju Sangmu", "Gimcheon Sangmu", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Incheon United", "Incheon", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Incheon United", "Incheon", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Jeju United FC", "Jeju Utd", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Jeju United FC", "Jeju Utd", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Suwon", "Suwon Bluewings", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Suwon", "Suwon Bluewings", klgsch1$awayteam)

klgsch1$hometeam <- ifelse(klgsch1$hometeam=="Ulsan Hyundai", "Ulsan HD", klgsch1$hometeam)
klgsch1$awayteam <- ifelse(klgsch1$awayteam=="Ulsan Hyundai", "Ulsan HD", klgsch1$awayteam)

# 경기 날짜(년,월,일), 홈팀, 어웨이팀 기준으로 병합
# 그전에 데이터 정제가 필요함

summary(klgodd2)
# klgodd2에서는 homeodds, drawodds, awayodds, homescore, awayscore, year, day, month가 숫자
klgodd2$homeodds <- as.numeric(klgodd2$homeodds)
klgodd2$drawodds <- as.numeric(klgodd2$drawodds)
klgodd2$awayodds <- as.numeric(klgodd2$awayodds)
klgodd2$homescore <- as.numeric(klgodd2$homescore)
klgodd2$awayscore <- as.numeric(klgodd2$awayscore)
klgodd2$year <- as.numeric(klgodd2$year)
klgodd2$day <- as.numeric(klgodd2$day)
klgodd2$month <- as.numeric(klgodd2$month)
klgodd2$awayscore <- ifelse(klgodd2$hometeam=="Gwangju FC" & klgodd2$date=="18.09.2021", 3, klgodd2$awayscore)


summary(klgsch1)
# wk, attendance, year, day, month를 숫자로 변환
klgsch1$wk <- as.numeric(klgsch1$wk)

# attendance는 콤마(,) 때문에 콤마를 먼저 없애야 함
klgsch1$attendance <- gsub(",","",klgsch1$attendance)
klgsch1$attendance <- as.numeric(klgsch1$attendance)


klgsch1$year <- as.numeric(klgsch1$year)
klgsch1$day <- as.numeric(klgsch1$day)
klgsch1$month <- as.numeric(klgsch1$month)

# merge 시 여러 기준으로 병합할 때 by 사용
klgfull <- merge(klgodd2, klgsch1, by = c("year","month","day", "hometeam", "awayteam"))
summary(klgfull)

# klgfull 데이터를 CSV 파일로 저장
write.csv(klgfull, "klgfull.csv", row.names = FALSE)

# 종료
remDr$close()
ch$stop()
