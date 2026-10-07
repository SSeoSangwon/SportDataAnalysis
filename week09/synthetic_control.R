# install.packages("dplyr")
# install.packages("plm")
# install.packages("lmtest")
# install.packages("ParallelTrendsPlot")
# install.packages("Synth")

options(repos=c(
  skranz='https://skranz.r-universe.dev',
  CRAN='https://cloud.r-project.org'))

library(dplyr)
library(plm)
library(lmtest)
library(ParallelTrendsPlot)
library(Synth)

df<-read.csv("olympicapt.csv")

summary(df)

table(df$b)

# 2011년 7월 6일 평창올림픽 개최 발표
# 2011년 6월 pre period
# 2011년 7월 post period
# 강릉 <- treatment city

df$treat<-ifelse(df$b=="강릉시",1,0)
df$post<-ifelse(df$year==2012 |
                  (df$year==2011 & df$month>6),1,0)
df$trtpost<-df$treat* df$post

# 기본 DiD 분석
did<-lm(priceper ~ treat+ post +trtpost,df)
summary(did)

# 패널 분석 방법 활용!
df$yearmonth<-ifelse(df$year==2010,df$month,
                     ifelse(df$year==2011,df$month+12,
                            df$month+24))
didpanel<-plm(priceper~ trtpost +factor(yearmonth),
              index="b",method="within",data=df)
summary(didpanel)

# 군집표준오차 적용
did_cl <- coeftest(didpanel, vcoc = vcovHC(didpanel, type = "sss",
                                           cluster = "group"))
did_cl

# 통제변수 추가
didpanel1 <- plm(priceper ~ trtpost + aptsize
                 + notrans + factor(yearmonth),
                 index = "b", method = "within", data = df)
summary(didpanel1)

did_cl1 <- coeftest(didpanel1,
                    vcoc = vcovHC(didpanel1, type = "sss",
                                  cluster = "group"))
did_cl1

# paralleltrends 검증
pd_data<-parallel.trends.data(
  df,
  timevar="yearmonth",
  yvar="priceper",
  treatdummy="treat",
  expdummy="trtpost",
  treat_exp_dummy=NULL,
  cvars= c("aptsize","notrans"),
  extravars=NULL,
  add.no.control=TRUE,
  constant.type= c("means","zero")[1]
)
parallel.trends.plot(
  pd_data,
  facet.mode=TRUE,
  add.exp.line=c("first","all", "none")[1],
  exp.line.opt=list(color="black")
)

df$city<-as.integer(factor(df$b))
head(df)

dataprep_out <-
  dataprep(
    foo=df,# 데이터 이름
    predictors =c("rgdp","pop", "aptsize","notrans"),# 변수 이름
    predictors.op="mean", # operation to be applied on the predictors
    time.predictors.prior=1:18, # time period for the predictors
    dependent ="priceper",# 종속변수
    unit.variable="city",# 유닛 변수 이름
    time.variable="yearmonth",# 시간 변수 이름
    treatment.identifier=1,# 트리트먼트 유닛(강릉시)
    controls.identifier=unique(df$city)[-1], # 나머지 유닛(donor pool)
    time.optimize.ssr=1:18,# treatment 이전 시기
    unit.names.variable="b", # 유닛 이름 변수
    time.plot=1:36 # 전체 시간
  )

synth_out<-synth(dataprep_out,method="BFGS")

synth.tables<-synth.tab(dataprep.res=dataprep_out,
                        synth.res =synth_out
)
synth.tables$tab.pred

synth.tables$tab.v

synth.tables$tab.w

path.plot(synth.res=synth_out,
          dataprep.res=dataprep_out,
          tr.intake=19,
          Ylab="Price",
          Xlab="Year-month",
          Z.plot=FALSE)

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 19,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in West Germany and its synthetic version")

