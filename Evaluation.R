
##Install packages
install.packages("devtools")

##Likert Packages
install_github('likert','jbryer')


##Don't forget to call likert::likert in order to use the likert package correctly
##Verify Scale value
##NPUA missig data and verify courses
##no data from 35
##first variable = gender

##Data Analysis of MathGeAr and MetaMath projects- pre and post test - process
##libraries to use
library(dplyr)
library(xlsx)
library(ggplot2)
library(ggfortify)
library(devtools)
library(psych)
library(gridExtra)
library(readxl)

#ASPU Data - Mathematics Perception

ASPUpeold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQOld")
ASPUpoold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQOld")
ASPUpemo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQModern")
ASPUpomo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQModern")

#ATSU Data - Mathematics Perception

ATSUpemo <- read.xlsx("EvaluationATSU_pre.xlsx", sheetName = "Sheet1")
ATSUpomo <- read.xlsx("EvaluationATSU_pos.xlsx", sheetName = "Sheet1")

#BSU Data - Mathematics Perception
BSUpeold <- read.xlsx("EvaluationBSUPreOld.xlsx", sheetName = "Sheet1")
BSUpoold <- read.xlsx("EvaluationBSUPosOld.xlsx", sheetName = "Sheet1")
BSUpemo <- read.xlsx("EvaluationBSUPreNew.xlsx", sheetName = "Sheet1")
BSUpomo <- read.xlsx("EvaluationBSUPosNew.xlsx", sheetName = "Sheet1")

#GTU Data - Mathematics Perception
GTUpomo <- read.xlsx("EvaluationGTU.xlsx", sheetName = "Sheet1")

#KAI Data - Mathematics Perception 

KAIpeoldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 1)
KAIpooldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 2)
KAIpemofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 4)
KAIpomofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 5)

KAIpeoldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQOld")
KAIpooldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQOld")
KAIpemosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQModern")
KAIpomosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQModern")

#NPUA Data - Mathematics Perception
NPUApeoldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQOld")
NPUApooldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQOld")
NPUApemofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQModern")
NPUApomofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQModern")

NPUApeoldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQOld")
NPUApooldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQOld")
NPUApemosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQModern")
NPUApomosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQModern")

#OMSU DATA - Mathematics Perception
OMSUpeold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQOld")
OMSUpoold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQOld")
OMSUpemo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQModern")
OMSUpomo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQModern")

#TVER DATA - Mathematics Perception
TVERpeoldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQOld")
TVERpooldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQOld")
TVERpemofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQModern")
TVERpomofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQModern")

TVERpeoldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQOld")
TVERpooldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQOld")
TVERpemosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQModern")
TVERpomosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQModern")

#LETI DATA - Mathematics Perception
LETIpeold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQOld")
LETIpoold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQOld")
LETIpemo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQModern")
LETIpomo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQModern")

#NNSU Data - Mathematics Perception
NNSUpeold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQOld")
NNSUpoold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQOld")
NNSUpemo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQModern")
NNSUpomo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQModern")


#----------xx-------------xxx---------------------------xxx------------xx-----------xxx------

#PERFORMANCE DATA
PerData <- read.xlsx("PerformanceTest.xlsx", sheetName = "Sheet1")
PerData$diff <- PerData$postest - PerData$pretest
##we have two columns with NA data that must be deleted
PerData[,8:9] <- NULL
PerData <- na.omit(PerData) ## in case we have NA values on the rows

#GENERAL DATA
prebox <- ggplot(PerData, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(PerData, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(PerData, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

##plot the scatterplot and linear regressions
ggplot(PerData, aes(x = pretest, y = posttest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

## F-test general
var.test(pretest ~ treatment, PerData)

#ANCOVA General
model1Gen <- lm(postest ~ pretest + factor(treatment), data = PerData)
anova(model1Gen)
model2Gen <- lm(postest ~ pretest * factor(treatment), data = PerData)
anova(model2Gen)

## OMSU DATA PERFORMANCE ----------------------------------------------------------------------------------------
DataOMSU <- subset(PerData, university == "OMSU")
describeBy(DataOMSU$diff, DataOMSU$treatment)
save(DataOMSU, file = "DataOMSU.RData")

## PLOT OMSU
prebox <- ggplot(DataOMSU, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataOMSU, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataOMSU, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

## F-test OMSU
var.test(pretest ~ treatment, DataOMSU) #as variancias sao iguais

## t-test means of diff
t.test(diff ~ treatment, var.equal = TRUE, paired = FALSE, DataOMSU) #as medias nao sao diferentes

## plot the linear models
ggplot(DataOMSU, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

model1OMSU <- lm(postest ~ pretest + treatment, DataOMSU)
anova(model1OMSU)
model2OMSU <- lm(postest ~ pretest * factor(treatment), DataOMSU)
anova(model2OMSU)

resultOMSU <- aov(postest ~ pretest * treatment, data = DataOMSU)
print(summary(resultOMSU))

## TSU DATA PERFORMANCE -----------------------------------------------------------------------------------------

DataTSU <- subset(PerData, university == "TSU")

## LETI DATA PERFORMANCE -----------------------------------------------------------------------------------------
DataLETI <- subset(PerData, university == "LETI")
describeBy(DataLETI$diff, DataLETI$treatment)

## PLOT LETI
prebox <- ggplot(DataLETI, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataLETI, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataLETI, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

##F-test LETI
var.test(pretest~treatment, DataLETI) ##as variancias sao iguais

## t-test means of diff
t.test(diff ~ treatment, DataLETI) ##nao existe diferencas nas medias

## plot the linear models
ggplot(DataLETI, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

aov(postest ~ pretest * treatment, data = DataLETI)

model1LETI <- lm(DataLETI$postest ~ DataLETI$pretest + factor(DataLETI$treatment))
anova(model1LETI)
model2LETI <- lm(DataLETI$postest ~ DataLETI$pretest * factor(DataLETI$treatment))
anova(model2LETI)

##KAI DATA PERFORMANCE -------------------------------------------------------------------------------------------------
DataKAI <- subset(PerData, university == "KAI")
describeBy(DataKAI$diff, DataKAI$treatment)

##PLOT KAI
prebox <- ggplot(DataKAI, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataKAI, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataKAI, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

## F-test KAI
var.test(pretest ~ treatment, DataKAI) ## as variancias sao iguais

## t-test means of diff
t.test(diff ~ treatment, var.equal = TRUE, paired = FALSE, DataKAI) ## as medias sao significativamente diferentes

##plot the linear models
ggplot(DataKAI, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

summary(aov(postest ~ pretest * treatment, data = DataKAI))

model1KAI <- lm(postest ~ pretest + factor(treatment), data = DataKAI)
anova(model1KAI)
model2KAI <- lm(postest ~ pretest * factor(treatment), data = DataKAI)
anova(model2KAI)

##UNN DATA PERFORMANCE
DataUNN <- subset(PerData, university == "NNSU")
describeBy(DataUNN$diff, DataUNN$treatment)

##PLOT UNN
prebox <- ggplot(DataUNN, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataUNN, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataUNN, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#F-test UNN
var.test(pretest ~ treatment, DataUNN) #variancas sao diferentes

##welchs test of means
t.test(diff ~ treatment, var.equal = FALSE, paired = FALSE, DataUNN) #medias nao significativas

summary(aov(postest ~ pretest * treatment, DataUNN))

##plot linear models
ggplot(DataUNN, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

model1UNN <- lm(postest ~ pretest + factor(treatment), data = DataUNN)
anova(model1UNN)
model2UNN <- lm(postest ~ pretest * factor(treatment), data = DataUNN)
anova(model2UNN)

##ATSU DATA PERFORMANCE --------------------------------------------------------------------------------------------
DataATSU <- subset(PerData, university == "ATSU")
describeBy(DataATSU$diff, DataATSU$treatment)
prebox <- ggplot(DataATSU, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataATSU, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataATSU, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#plot the scat linear
ggplot(DataATSU, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

##F-test ATSU
var.test(pretest ~ treatment, DataATSU) #variancas sao diferentes

##welchs test
t.test(diff ~ treatment, var.equal = FALSE, paired = FALSE, DataATSU) #rejeita h0

model1ATSU <- lm(postest ~ pretest + factor(treatment), DataATSU)
anova(model1ATSU)
model2ATSU <- lm(postest ~ pretest * factor(treatment), DataATSU)
anova(model2ATSU)

##ASPU DATA PERFORMANCE ---------------------------------------------------------------------------------------------
DataASPU <- subset(PerData, university == "ASPU")
describeBy(DataASPU$diff, DataASPU$treatment)

prebox <- ggplot(DataASPU, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataASPU, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataASPU, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

##plot the scatterplot and linear regressions
ggplot(DataASPU, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

## F-test to verify the variance
var.test(pretest ~ treatment, DataASPU) #variancias iguais

## t-test for the means
t.test(diff ~ treatment, DataASPU)

## linear models for the data
model1ASPU <- lm(postest~ pretest + factor(treatment), data = DataASPU)
anova(model1ASPU)
model2ASPU <- lm(postest ~ pretest * factor(treatment), data = DataASPU)
anova(model2ASPU)

## TVER PERF DATA -------------------------------------------------------------------------------------------------------
DataTVER <- subset(PerData, university == "TVER")
describeBy(DataTVER$diff, DataTVER$treatment)

prebox <- ggplot(DataTVER, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataTVER, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataTVER, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#plot the scatter and linear
ggplot(DataTVER, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

## F-test for variance
var.test(pretest ~ treatment, DataTVER) #variancias iguais

## t-test medias
t.test(diff ~ treatment, var.equal = TRUE, paired = FALSE, DataTVER) 

summary(aov(postest ~ pretest * treatment, DataTVER))

#linear models
model1TVER <- lm(postest ~ pretest + factor(treatment), data = DataTVER)
anova(model1TVER)
model2TVER <- lm(postest ~ pretest * factor(treatment), data = DataTVER)
anova(model2TVER)

##setting labels to the levels
#La = c("StD", "D", "SmD", "SmA", "A", "StA")

##Changing the GTU data to factor
for(col in 8:34){
  DataGTU[,col] <- factor(DataGTU[,col], levels = 1:6, labels = La, ordered = TRUE )
}



