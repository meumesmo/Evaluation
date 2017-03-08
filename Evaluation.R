setwd("~/GitHub/Evaluation/CleanData")

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

##CLEANING DATA

#ASPU Data - Mathematics Perception

ASPUpeold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQOld")
ASPUpeold[,35:42] <- NULL
ASPUpeold$univ <- as.factor("ASPU")
ASPUpoold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQOld")
ASPUpoold[,35:42] <- NULL
ASPUpoold$univ <- as.factor("ASPU")
ASPUpemo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQModern")
ASPUpemo[,35:42] <- NULL
ASPUpemo$univ <- as.factor("ASPU")
ASPUpomo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQModern")
ASPUpomo[,35:42] <- NULL
ASPUpomo$univ <- as.factor("ASPU")

#ATSU Data - Mathematics Perception

ATSUpemo <- read.xlsx("EvaluationATSU_pre.xlsx", sheetName = "Sheet1")
ATSUpemo[,35:42] <- NULL
ATSUpemo$univ <- as.factor("ATSU")
ATSUpomo <- read.xlsx("EvaluationATSU_pos.xlsx", sheetName = "Sheet1")
ATSUpomo[,35:42] <- NULL
ATSUpomo$univ <- as.factor("ATSU")

#BSU Data - Mathematics Perception
BSUpeold <- read.xlsx("EvaluationBSUPreOld.xlsx", sheetName = "Sheet1")
BSUpeold[,35:42] <- NULL
BSUpeold$univ <- as.factor("BSU")
BSUpoold <- read.xlsx("EvaluationBSUPosOld.xlsx", sheetName = "Sheet1")
BSUpoold[,35:42] <- NULL
BSUpoold$univ <- as.factor("BSU")
BSUpemo <- read.xlsx("EvaluationBSUPreNew.xlsx", sheetName = "Sheet1")
BSUpemo[,35:42] <- NULL
BSUpemo$univ <- as.factor("BSU")
BSUpomo <- read.xlsx("EvaluationBSUPosNew.xlsx", sheetName = "Sheet1")
BSUpomo[,35:42] <- NULL
BSUpomo$univ <- as.factor("BSU")

#GTU Data - Mathematics Perception
#Esses dados eu não to ligado pq nao tem informacao de quando eles foram realizados.
GTUpomo <- read.xlsx("EvaluationGTU.xlsx", sheetName = "Sheet1")
GTUpomo[,35:42] <- NULL
GTUpomo$univ <- as.factor("GTU")

#UG Data - Matehamtics Percepetion
UGpeold <- read.xlsx("EvaluationUG.xlsx", sheetName = "PreQOld")
UGpeold[,35:42] <- NULL
UGpeold$univ <- as.factor("UG")
UGpoold <- read.xlsx("EvaluationUG.xlsx", sheetName = "PosQOld")
UGpoold[,35:42] <- NULL
UGpoold$univ<- as.factor("UG")
UGpemo <- read.xlsx("EvaluationUG.xlsx", sheetName = "PreQModern")
UGpemo[,35:42] <- NULL
UGpemo$univ <- as.factor("UG")
UGpomo <- read.xlsx("EvaluationUG.xlsx", sheetName = "PosQModern")
UGpomo[,35:42] <- NULL
UGpomo$univ <- as.factor("UG")


#KAI Data - Mathematics Perception 

KAIpeoldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 1)
KAIpeoldfirst[,35:42] <- NULL
KAIpeoldfirst$univ <- as.factor("KAI")
KAIpooldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 2)
KAIpooldfirst[,35:42] <- NULL
KAIpooldfirst$univ <- as.factor("KAI")
KAIpemofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 4)
KAIpemofirst[,35:42] <- NULL
KAIpemofirst$univ <- as.factor("KAI")
KAIpomofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 5)
KAIpomofirst[,35:42] <- NULL
KAIpomofirst$univ <- as.factor("KAI")

KAIpeoldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQOld")
KAIpeoldsec[,35:42] <- NULL
KAIpeoldsec$univ <- as.factor("KAI")
KAIpooldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQOld")
KAIpooldsec[,35:42] <- NULL
KAIpooldsec$univ <- as.factor("KAI")
KAIpemosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQModern")
KAIpemosec[,35:42] <- NULL
KAIpemosec$univ <- as.factor("KAI")
KAIpomosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQModern")
KAIpomosec[,35:42] <- NULL
KAIpomosec$univ <- as.factor("KAI")

#NPUA Data - Mathematics Perception
NPUApeoldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQOld")
NPUApeoldfirst[,35:45] <- NULL
NPUApeoldfirst$univ <- as.factor("NPUA")
NPUApooldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQOld")
NPUApooldfirst[,35:45] <- NULL
NPUApooldfirst$univ <- as.factor("NPUA")
NPUApemofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQModern")
NPUApemofirst[,35:45] <- NULL
NPUApemofirst$univ <- as.factor("NPUA")
NPUApomofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQModern")
NPUApomofirst[,35:45] <- NULL
NPUApomofirst$univ <- as.factor("NPUA")

NPUApeoldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQOld")
NPUApeoldsec[,35:45] <- NULL
NPUApeoldsec$univ <- as.factor("NPUA")
NPUApooldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQOld")
NPUApooldsec[,35:45] <- NULL
NPUApooldsec$univ <- as.factor("NPUA")
NPUApemosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQModern")
NPUApemosec[,35:45] <- NULL
NPUApemosec$univ <- as.factor("NPUA")
NPUApomosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQModern")
NPUApomosec[,35:45] <- NULL
NPUApomosec$univ <- as.factor("NPUA")

#OMSU DATA - Mathematics Perception
OMSUpeold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQOld")
OMSUpeold[,35:42] <- NULL
OMSUpeold$univ <- as.factor("OMSU")
OMSUpoold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQOld")
OMSUpoold[,35:42] <- NULL
OMSUpoold$univ <- as.factor("OMSU")
OMSUpemo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQModern")
OMSUpemo[,35:42] <- NULL
OMSUpemo$univ <- as.factor("OMSU")
OMSUpomo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQModern")
OMSUpomo[,35:42] <- NULL
OMSUpomo$univ <- as.factor("OMSU")

#TVER DATA - Mathematics Perception
TVERpeoldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQOld")
TVERpeoldfirst[,35:42] <- NULL
TVERpeoldfirst$univ <- as.factor("TVER")
TVERpooldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQOld")
TVERpooldfirst[,35:42] <- NULL
TVERpooldfirst$univ <- as.factor("TVER")
TVERpemofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQModern")
TVERpemofirst[,35:42] <- NULL
TVERpemofirst$univ <- as.factor("TVER")
TVERpomofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQModern")
TVERpomofirst[,35:42] <- NULL
TVERpomofirst$univ <- as.factor("TVER")

TVERpeoldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQOld")
TVERpeoldsec[,35:42] <- NULL
TVERpeoldsec$univ <- as.factor("TVER")
TVERpooldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQOld")
TVERpooldsec[,35:42] <- NULL
TVERpooldsec$univ <- as.factor("TVER")
TVERpemosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQModern")
TVERpemosec[,35:42] <- NULL
TVERpemosec$univ <- as.factor("TVER")
TVERpomosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQModern")
TVERpomosec[,35:42] <- NULL
TVERpomosec$univ <- as.factor("TVER")

#LETI DATA - Mathematics Perception
LETIpeold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQOld")
LETIpeold[,35:42] <- NULL
LETIpeold$univ <- as.factor("LETI")
LETIpoold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQOld")
LETIpoold[,35:42] <- NULL
LETIpoold$univ <- as.factor("LETI")
LETIpemo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQModern")
LETIpemo[,35:42] <- NULL
LETIpemo$univ <- as.factor("LETI")
LETIpomo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQModern")
LETIpomo[,35:42] <- NULL
LETIpomo$univ <- as.factor("LETI")

#NNSU Data - Mathematics Perception
NNSUpeold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQOld")
NNSUpeold[,35:42] <- NULL
NNSUpeold$univ <- as.factor("NNSU")
NNSUpoold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQOld")
NNSUpoold[,35:42] <- NULL
NNSUpoold$univ <- as.factor("NNSU")
NNSUpemo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQModern")
NNSUpemo[,35:42] <- NULL
NNSUpemo$univ <- as.factor("NNSU")
NNSUpomo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQModern")
NNSUpomo[,35:42] <- NULL
NNSUpomo$univ <- as.factor("NNSU")


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

## BTU DATA PERFORMANCE -----------------------------------------

DataBTU <- subset(PerData, university == "NPUA")
describeBy(DataBTU, DataBTU$treatment)
summary(DataBTU)

prebox <- ggplot(DataBTU, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataBTU, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataBTU, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#F-test BTU
var.test(pretest ~ treatment, DataBTU) #as variancias sao iguais

t.test(diff ~ treatment, var.equal = TRUE, paired = FALSE, DataBTU) #as medias nao sao diferentes

ggplot(DataBTU, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

resultBTU <- aov(postest ~ pretest * treatment, data = DataBTU)
print(summary(resultBTU))


##GTU DATA PERFORMANCE---------------------------------------------------------

DataGTU <- subset(PerData, university == "GTU")
describeBy(DataGTU$diff, DataGTU$treatment)
summary(DataGTU)

prebox <- ggplot(DataGTU, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
postbox <- ggplot(DataGTU, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)
meanbox <- ggplot(DataGTU, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#F-test GTU
var.test(pretest ~ treatment, DataGTU) #as variancias nao sao iguais

## t-test means of diff
t.test(diff ~ treatment, var.equal = FALSE, paired = FALSE, DataGTU) #as medias nao sao diferentes

##Linear models
ggplot(DataGTU, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

model1GTU <- lm(postest ~ pretest + treatment, DataGTU)
anova(model1GTU)
model2GTU <- lm(postest ~ pretest * factor(treatment), DataGTU)
anova(model2GTU)

resultGTU <- aov(postest ~ pretest * treatment, data = DataGTU)
print(summary(resultGTU))

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

fitpre <- lm(pretest ~ treatment, data = DataKAI)
fitpos <- lm(postest ~ treatment, data = DataKAI)



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

Questions <- DataMetaMath[,1:27]
Questions.PCA <- prcomp(Questions, center = TRUE, scale = TRUE)
plot(Questions.PCA)

library(ggbiplot)

Questionspemo <- DataRupemo[,1:27]
Questionspemo.PCA <- prcomp(Questionspemo, center = TRUE, scale = TRUE)

gpemo <- ggbiplot(Questionspemo.PCA, obs.scale = 1, var.scale = 1,
                  groups = DataRupemo$univ, ellipse = TRUE)
gpemo <- gpemo + scale_color_discrete(name = '')
gpemo <- gpemo + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
print(gpemo)

gpomo <- ggbiplot(Questions.PCA, obs.scale = 1, var.scale = 1,
              groups = DataMetaMath$university, ellipse = TRUE)
gpomo <- gpomo + scale_color_discrete(name = '')
gpomo <- gpomo + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(gpomo)

Questionspold <- DataRupold[,1:27]
Questionspold.PCA <- prcomp(Questionspold, center = TRUE, scale = TRUE)

gpold <- ggbiplot(Questionspold.PCA, obs.scale = 1, var.scale = 1,
                  groups = DataRupold$univ, ellipse = TRUE)
gpold <- gpold + scale_color_discrete(name = '')
gpold <- gpold + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
print(gpold)


QuestionsPeold <- DataRupre[,1:27]
QuestionsPeold.PCA <- prcomp(QuestionsPeold, center = TRUE, scale = TRUE)
plot(QuestionsPeold.PCA)

gpeold <- ggbiplot(QuestionsPeold.PCA, obs.scale = 1, var.scale = 1,
              groups = DataRupre$univ, ellipse = TRUE)
gpeold <- gpeold + scale_color_discrete(name = '')
gpeold <- gpeold + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(gpeold)

QGeoPemo <- GeoPemo[, 1:27]
QGeoPemo.PCA <- prcomp(QGeoPemo, center = TRUE, scale = TRUE)
plot(QGeoPemo.PCA)

geopemo <- ggbiplot(QGeoPemo.PCA, obs.scale = 1, var.scale = 1, groups = GeoPemo$univ, ellipse = TRUE)
geopemo <- geopemo + scale_color_discrete(name = '')
geopemo <- geopemo + theme(legend.direction = 'horizontal',
                           legend.position = 'top')
print(geopemo)


QGeoPomo <- GeoPomo[, 1:27]
QGeoPomo.PCA <- prcomp(QGeoPomo, center = TRUE, scale = TRUE)
plot(QGeoPomo.PCA)

geopomo <- ggbiplot(QGeoPomo.PCA, obs.scale = 1, var.scale = 1, groups = GeoPomo$univ, ellipse = TRUE)
geopomo <- geopomo + scale_color_discrete(name = '')
geopomo <- geopomo + theme(legend.direction = 'horizontal',
                           legend.position = 'top')
print(geopomo)

QGeArPoold <- GeArpoold[, 1:27]
QGeArPoold.PCA <- prcomp(QGeArPoold, center = TRUE, scale = TRUE)

geoarpoold <- ggbiplot(QGeArPoold.PCA, obs.scale = 1, var.scale = 1,
                       groups = GeArpoold$univ, ellipse = TRUE) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
                                                                                                         legend.position = 'top')
print(geoarpoold)

QGeArPemo <- GeArPemo[,1:27]
QGeArPemo.PCA <- prcomp(QGeArPemo, center = TRUE, scale = TRUE)

geoarpemo <- ggbiplot(QGeArPemo.PCA, obs.scale = 1, var.scale = 1,
                       groups = GeArPemo$univ, ellipse = TRUE) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
                                                                                                          legend.position = 'top')
print(geoarpemo)

QGeArPomo <- GeArPomo[,1:27]
QGeArPomo.PCA <- prcomp(QGeArPomo, center = TRUE, scale = TRUE)
geoarpomo <- ggbiplot(QGeArPomo.PCA, obs.scale = 1, var.scale = 1,
                      groups = GeArPomo$univ, ellipse = TRUE) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
                                                                                                        legend.position = 'top')
print(geoarpomo)

QGeoPoold <- GeoPoold[, 1:27]
QGeoPoold.PCA <- prcomp(QGeoPoold, center = TRUE, scale = TRUE)
plot(QGeoPoold.PCA)

geopoold <- ggbiplot(QGeoPoold.PCA, obs.scale = 1, var.scale = 1,
                     groups = GeoPoold$univ, ellipse = TRUE)
geopoold <- geopoold + scale_color_discrete(name = '')
geopoold <- geopoold + theme(legend.direction = 'horizontal',
                             legend.position = 'top')
print(geopoold)

QGeArPeold <- GeArPeold[,1:27]
QGeArPeold.PCA <- prcomp(QGeArPeold, center = TRUE, scale = TRUE)
plot(QGeArPeold.PCA)

gearpeold <- ggbiplot(QGeArPeold.PCA, obs.scale = 1, var.scale = 1,
                     groups = GeArPeold$univ, ellipse = TRUE)
gearpeold <- gearpeold + scale_color_discrete(name = '')
gearpeold <- gearpeold + theme(legend.direction = 'horizontal',
                             legend.position = 'top')
print(gearpeold)


QGeoPeold <- GeoPeold[,1:27]
QGeoPeold.PCA <- prcomp(QGeoPeold, center = TRUE, scale = TRUE)
plot(QGeoPeold.PCA)

geopeold <- ggbiplot(QGeoPeold.PCA, obs.scale = 1, var.scale = 1,
                   groups = GeoPeold$univ, ellipse = TRUE)
geopeold <- geopeold + scale_color_discrete(name = '')
geopeold <- geopeold + theme(legend.direction = 'horizontal',
                         legend.position = 'top')
print(geopeold)
