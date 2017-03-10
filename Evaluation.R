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
library(plyr)

##CLEANING DATA

#ASPU Data - Mathematics Perception

ASPUpeold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQOld")
ASPUpeold[,35:42] <- NULL
ASPUpeold$univ <- as.factor("ASPU")
ASPUpeold$test <- as.factor("peold")
ASPUpoold <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQOld")
ASPUpoold[,35:42] <- NULL
ASPUpoold$univ <- as.factor("ASPU")
ASPUpoold$test <- as.factor("poold")
ASPUpemo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PreQModern")
ASPUpemo[,35:42] <- NULL
ASPUpemo$univ <- as.factor("ASPU")
ASPUpemo$test <- as.factor("pemo")
ASPUpomo <- read.xlsx("EvaluationDataASPU_Armenia.xlsx", sheetName = "PostQModern")
ASPUpomo[,35:42] <- NULL
ASPUpomo$univ <- as.factor("ASPU")
ASPUpomo$test <- as.factor("pomo")


#ATSU Data - Mathematics Perception

ATSUpemo <- read.xlsx("EvaluationATSU_pre.xlsx", sheetName = "Sheet1")
ATSUpemo[,35:42] <- NULL
ATSUpemo$univ <- as.factor("ATSU")
ATSUpemo$test <- as.factor("pemo")
ATSUpomo <- read.xlsx("EvaluationATSU_pos.xlsx", sheetName = "Sheet1")
ATSUpomo[,35:42] <- NULL
ATSUpomo$univ <- as.factor("ATSU")
ATSUpomo$test <- as.factor("pomo")

#BSU Data - Mathematics Perception
BSUpeold <- read.xlsx("EvaluationBSUPreOld.xlsx", sheetName = "Sheet1")
BSUpeold[,35:42] <- NULL
BSUpeold$univ <- as.factor("BSU")
BSUpeold$test <- as.factor("peold")
BSUpoold <- read.xlsx("EvaluationBSUPosOld.xlsx", sheetName = "Sheet1")
BSUpoold[,35:42] <- NULL
BSUpoold$univ <- as.factor("BSU")
BSUpoold$test <- as.factor("poold")
BSUpemo <- read.xlsx("EvaluationBSUPreNew.xlsx", sheetName = "Sheet1")
BSUpemo[,35:42] <- NULL
BSUpemo$univ <- as.factor("BSU")
BSUpemo$test <- as.factor("pemo")
BSUpomo <- read.xlsx("EvaluationBSUPosNew.xlsx", sheetName = "Sheet1")
BSUpomo[,35:42] <- NULL
BSUpomo$univ <- as.factor("BSU")
BSUpomo$test <- as.factor("pomo")

#GTU Data - Mathematics Perception
#Esses dados eu não to ligado pq nao tem informacao de quando eles foram realizados.
GTUpomo <- read.xlsx("EvaluationGTU.xlsx", sheetName = "Sheet1")
GTUpomo[,35:42] <- NULL
GTUpomo$univ <- as.factor("GTU")
GTUpomo$test <- as.factor("pomo")

#UG Data - Matehamtics Percepetion
UGpeold <- read.xlsx("EvaluationUG.xlsx", sheetName = "PreQOld")
UGpeold[,35:42] <- NULL
UGpeold$univ <- as.factor("UG")
UGpeold$test <- as.factor("peold")
UGpoold <- read.xlsx("EvaluationUG.xlsx", sheetName = "PosQOld")
UGpoold[,35:42] <- NULL
UGpoold$univ<- as.factor("UG")
UGpoold$test <- as.factor("poold")
UGpemo <- read.xlsx("EvaluationUG.xlsx", sheetName = "PreQModern")
UGpemo[,35:42] <- NULL
UGpemo$univ <- as.factor("UG")
UGpemo$test <- as.factor("pemo")
UGpomo <- read.xlsx("EvaluationUG.xlsx", sheetName = "PosQModern")
UGpomo[,35:42] <- NULL
UGpomo$univ <- as.factor("UG")
UGpomo$test <- as.factor("pomo")

#KAI Data - Mathematics Perception 

KAIpeoldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 1)
KAIpeoldfirst[,35:42] <- NULL
KAIpeoldfirst$univ <- as.factor("KAI")
KAIpeoldfirst$test <- as.factor("peold")
KAIpooldfirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 2)
KAIpooldfirst[,35:42] <- NULL
KAIpooldfirst$univ <- as.factor("KAI")
KAIpooldfirst$test <- as.factor("poold")
KAIpemofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 4)
KAIpemofirst[,35:42] <- NULL
KAIpemofirst$univ <- as.factor("KAI")
KAIpemofirst$test <- as.factor("pemo")
KAIpomofirst <- read_excel("EvaluationKAIProb.xlsx", sheet = 5)
KAIpomofirst[,35:42] <- NULL
KAIpomofirst$univ <- as.factor("KAI")
KAIpomofirst$test <- as.factor("pomo")

KAIpeoldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQOld")
KAIpeoldsec[,35:42] <- NULL
KAIpeoldsec$univ <- as.factor("KAI")
KAIpeoldsec$test <- as.factor("peold")
KAIpooldsec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQOld")
KAIpooldsec[,35:42] <- NULL
KAIpooldsec$univ <- as.factor("KAI")
KAIpooldsec$test <- as.factor("poold")
KAIpemosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PreQModern")
KAIpemosec[,35:42] <- NULL
KAIpemosec$univ <- as.factor("KAI")
KAIpemosec$test <- as.factor("pemo")
KAIpomosec <- read.xlsx("EvaluationKAIOp.xlsx", sheetName = "PostQModern")
KAIpomosec[,35:42] <- NULL
KAIpomosec$univ <- as.factor("KAI")
KAIpomosec$test <- as.factor("pomo")

#NPUA Data - Mathematics Perception
NPUApeoldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQOld")
NPUApeoldfirst[,35:45] <- NULL
NPUApeoldfirst$univ <- as.factor("NPUA")
NPUApeoldfirst$test <- as.factor("peold")
NPUApeoldfirst[,7] <- "-"
NPUApooldfirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQOld")
NPUApooldfirst[,35:45] <- NULL
NPUApooldfirst$univ <- as.factor("NPUA")
NPUApooldfirst$test <- as.factor("poold")
NPUApooldfirst[,7] <- "-"
NPUApemofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PreQModern")
NPUApemofirst[,35:45] <- NULL
NPUApemofirst$univ <- as.factor("NPUA")
NPUApemofirst$test <- as.factor("pemo")
NPUApemofirst[,7] <- "-"
NPUApomofirst <- read.xlsx("EvaluationNPUAone.xlsx", sheetName = "PostQModern")
NPUApomofirst[,35:45] <- NULL
NPUApomofirst$univ <- as.factor("NPUA")
NPUApomofirst$test <- as.factor("pomo")
NPUApomofirst[,7] <- "-"

NPUApeoldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQOld")
NPUApeoldsec[,35:45] <- NULL
NPUApeoldsec$univ <- as.factor("NPUA")
NPUApeoldsec$test <- as.factor("peold")
NPUApeoldsec[,7] <- "-"
NPUApooldsec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQOld")
NPUApooldsec[,35:45] <- NULL
NPUApooldsec$univ <- as.factor("NPUA")
NPUApooldsec$test <- as.factor("poold")
NPUApooldsec[,7] <- "-"
NPUApemosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PreQModern")
NPUApemosec[,35:45] <- NULL
NPUApemosec$univ <- as.factor("NPUA")
NPUApemosec$test <- as.factor("pemo")
NPUApemosec[,7] <- "-"
NPUApomosec <- read.xlsx("EvaluationNPUAtwo.xlsx", sheetName = "PostQModern")
NPUApomosec[,35:45] <- NULL
NPUApomosec$univ <- as.factor("NPUA")
NPUApomosec$test <- as.factor("pomo")
NPUApomosec[,7] <- "-"

#OMSU DATA - Mathematics Perception
OMSUpeold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQOld")
OMSUpeold[,35:42] <- NULL
OMSUpeold$univ <- as.factor("OMSU")
OMSUpeold$test <- as.factor("peold")
OMSUpoold <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQOld")
OMSUpoold[,35:42] <- NULL
OMSUpoold$univ <- as.factor("OMSU")
OMSUpoold$test <- as.factor("poold")
OMSUpemo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PreQModern")
OMSUpemo[,35:42] <- NULL
OMSUpemo$univ <- as.factor("OMSU")
OMSUpemo$test <- as.factor("pemo")
OMSUpomo <- read.xlsx("Evaluation Data OMSU.xlsx", sheetName = "PostQModern")
OMSUpomo[,35:42] <- NULL
OMSUpomo$univ <- as.factor("OMSU")
OMSUpomo$test <- as.factor("pomo")

#TVER DATA - Mathematics Perception
TVERpeoldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQOld")
TVERpeoldfirst[,35:42] <- NULL
TVERpeoldfirst$univ <- as.factor("TVER")
TVERpeoldfirst$test <- as.factor("peold")
TVERpooldfirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQOld")
TVERpooldfirst[,35:42] <- NULL
TVERpooldfirst$univ <- as.factor("TVER")
TVERpooldfirst$test <- as.factor("poold")
TVERpemofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PreQModern")
TVERpemofirst[,35:42] <- NULL
TVERpemofirst$univ <- as.factor("TVER")
TVERpemofirst$test <- as.factor("pemo")
TVERpomofirst <- read.xlsx("EvaluationTVERone.xlsx", sheetName = "PostQModern")
TVERpomofirst[,35:42] <- NULL
TVERpomofirst$univ <- as.factor("TVER")
TVERpomofirst$test <- as.factor("pomo")

TVERpeoldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQOld")
TVERpeoldsec[,35:42] <- NULL
TVERpeoldsec$univ <- as.factor("TVER")
TVERpeoldsec$test <- as.factor("peold")
TVERpooldsec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQOld")
TVERpooldsec[,35:42] <- NULL
TVERpooldsec$univ <- as.factor("TVER")
TVERpooldsec$test <- as.factor("poold")
TVERpemosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PreQModern")
TVERpemosec[,35:42] <- NULL
TVERpemosec$univ <- as.factor("TVER")
TVERpemosec$test <- as.factor("pemo")
TVERpomosec <- read.xlsx("EvaluationTVERtwo.xlsx", sheetName = "PostQModern")
TVERpomosec[,35:42] <- NULL
TVERpomosec$univ <- as.factor("TVER")
TVERpomosec$test <- as.factor("pomo")

#LETI DATA - Mathematics Perception
LETIpeold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQOld")
LETIpeold[,35:42] <- NULL
LETIpeold$univ <- as.factor("LETI")
LETIpeold$test <- as.factor("peold")
LETIpoold <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQOld")
LETIpoold[,35:42] <- NULL
LETIpoold$univ <- as.factor("LETI")
LETIpoold$test <- as.factor("poold")
LETIpemo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PreQModern")
LETIpemo[,35:42] <- NULL
LETIpemo$univ <- as.factor("LETI")
LETIpemo$test <- as.factor("pemo")
LETIpomo <- read.xlsx("EvaluationLETI.xlsx", sheetName = "PostQModern")
LETIpomo[,35:42] <- NULL
LETIpomo$univ <- as.factor("LETI")
LETIpomo$test <- as.factor("pomo")

#NNSU Data - Mathematics Perception
NNSUpeold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQOld")
NNSUpeold[,35:42] <- NULL
NNSUpeold$univ <- as.factor("NNSU")
NNSUpeold$test <- as.factor("peold")
NNSUpoold <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQOld")
NNSUpoold[,35:42] <- NULL
NNSUpoold$univ <- as.factor("NNSU")
NNSUpoold$test <- as.factor("poold")
NNSUpemo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PreQModern")
NNSUpemo[,35:42] <- NULL
NNSUpemo$univ <- as.factor("NNSU")
NNSUpemo$test <- as.factor("pemo")
NNSUpomo <- read.xlsx("EvaluationNNSU.xlsx", sheetName = "PostQModern")
NNSUpomo[,35:42] <- NULL
NNSUpomo$univ <- as.factor("NNSU")
NNSUpomo$test <- as.factor("pomo")

#Putting all data together
RussianData <- rbind.fill(LETIpeold, LETIpoold, LETIpemo, LETIpomo, OMSUpeold, OMSUpoold, OMSUpemo, OMSUpomo, TVERpeoldfirst, TVERpeoldsec, TVERpooldfirst, TVERpooldsec, TVERpemofirst, TVERpemosec, TVERpomofirst, TVERpomosec, NNSUpeold, NNSUpoold, NNSUpemo, NNSUpomo, KAIpeoldfirst, KAIpeoldsec, KAIpooldfirst, KAIpooldsec, KAIpemofirst, KAIpemosec, KAIpomofirst, KAIpomosec)
RussianData$`faculty    `<- NULL
RussianData$faculty... <- NULL
RussianData$faculty.... <- NULL
RussianData$`university year` <- NULL

RussianDemo <- RussianData[,1:6]
RussianDemo$test <- RussianData$test
RussianDemo$univ <- RussianData$univ


View(RussianData)

CaucasianData <- rbind.fill(GTUpomo, BSUpeold, BSUpoold, BSUpemo, BSUpomo, UGpeold, UGpoold, UGpemo, UGpomo, ATSUpemo, ATSUpomo, ASPUpeold, ASPUpoold, ASPUpemo, ASPUpomo, NPUApeoldfirst, NPUApeoldsec, NPUApooldfirst, NPUApooldsec, NPUApemofirst, NPUApemosec, NPUApomofirst, NPUApomosec)
CaucasianData$faculty <- NULL
CaucasianData$faculty..<- NULL
CaucasianData$faculty... <- NULL
CaucasianData$faculty.... <- NULL
CaucasianData$X.university.year <- NULL

View(CaucasianData)
