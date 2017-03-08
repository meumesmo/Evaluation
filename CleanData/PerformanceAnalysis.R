##PERFORMANCE ANALYSIS

##libraries
install.packages("psych")
library(psych)
library(ggplot2)
library(gridExtra)


PerData <- read.xlsx("PerformanceTest.xlsx", sheetName = "Sheet1")
PerData$diff <- PerData$postest - PerData$pretest
##we have two columns with NA data that must be deleted
PerData[,8:9] <- NULL
PerData <- na.omit(PerData) ## in case we have NA values on the rows

##General Analysis 
##Options: ASPU, ATSU, BSU, GTU, KAI, LETI, NNSU, NPUA, OMSU, TVER, UG
UnivData <- subset(PerData, university == "NNSU") #put the sigla of the university here for instance GTU
describeBy(UnivData, UnivData$treatment)
summary(UnivData)

#plot the 
prebox <- ggplot(UnivData, aes(x = treatment, y = pretest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Pre Test") + stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) + coord_cartesian(ylim = c(0,100))
postbox <- ggplot(UnivData, aes(x = treatment, y = postest, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores Post Test")+ stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4)+ coord_cartesian(ylim = c(0,100))
meanbox <- ggplot(UnivData, aes(x = treatment, y = diff, fill = treatment)) + geom_boxplot() + scale_x_discrete() + xlab("Treatment Group") + ylab("Scores diff") + stat_summary(fun.y = mean, geom="point",shape=5, size=4)+ coord_cartesian(ylim = c(0,100))
grid.arrange(prebox, postbox, meanbox, ncol = 3)

#F.test 
var.test(pretest ~ treatment, UnivData)

#t.test must specify on the var.equal taking the previous test TRUE of FALSE
t.test(diff ~ treatment, var.equal = FALSE, paired = FALSE, UnivData)

#plot the models
ggplot(UnivData, aes(x = pretest, y = postest, shape = treatment, color = treatment)) + geom_point() + geom_smooth(method = lm, se=FALSE, fullrange = TRUE)

#ancova model
resultUniv <- aov(postest ~ pretest * treatment, data = UnivData)
print(summary(resultUniv))


