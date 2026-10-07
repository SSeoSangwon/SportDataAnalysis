# install.packages("devtools")
# install.packages("mediation")
library(devtools)
# devtools::install_github("cardiomoon/processR")
library(processR)
# install.packages("lavaan")
library(lavaan)

library(mediation)
library(lmtest)
library(sandwich)
library(tidyverse)
library(haven)
library(mfx)
library(survival)
library(moonBook)

# 운동시간이 MBS에 미치는 영향에서 유산소 신체활동 실천율이 조절한다면?

datafin <- read.csv("samplekhn22.csv")

pmacro$no
pmacroModel(1)
statisticalDiagram(1)

summary(datafin$pa_aerobic)

labels <- list(X = "exer", Y = "mbs", W = "pa_aerobic")

pmacroModel(1,labels=labels)
model <- lm(mbs ~ exer + pa_aerobic + exer * pa_aerobic, data = datafin)
summary(model)

# processR
m.summary <- modelsSummary(list(model), labels = labels)
modelsSummaryTable(m.summary)

# 소득이 건강에 미치는 영향에서 운동이 매개한다면?
labels <- list(X = "ainc", Y = "mbs", M = "exer")
meanSummaryTable(labels = labels, data = datafin)

pmacroModel(4,labels=labels)

model = tripleEquation(X = "ainc", M = "exer", Y = "mbs")
cat(model)

semfit = sem(model = model, data = datafin)
summary(semfit)

estimatesTable(semfit)
estimatesTable2(semfit)
statisticalDiagram(4, labels = labels, fit = semfit, whatLabel = "est")

equations = regEquation(X = "ainc", M = "exer", Y = "mbs")
cat(equations)
eq = unlist(strsplit(equations, "\n"))

fit = lapply(1:2, function(i) {
  lm(as.formula(eq[i]), data = datafin)
})

summary(fit[[1]])
summary(fit[[2]])

x = modelsSummary(fit, labels = labels)
modelsSummaryTable(x)

x = modmedSummary(semfit)
modmedSummaryTable(x)
