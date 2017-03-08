##PERCEPTIONS ANALYSIS

##Install packages
install.packages("devtools")
library(devtools)
##Likert Packages
install_github('likert','jbryer')


library(ggbiplot)

##RUSSIA NUMERIC VALUES: LETI, KAI, OMSU, TVER, NNSU Only questions
Rupeold <- rbind(LETIpeold[,8:35], KAIpeoldfirst[,8:35], KAIpeoldsec[,8:35], OMSUpeold[,8:35], TVERpeoldfirst[,8:35],TVERpeoldsec[,8:35],NNSUpeold[,8:35])
Rupeold <- na.omit(Rupeold)

Rupoold <- rbind(LETIpoold[,8:35], KAIpooldfirst[,8:35], KAIpooldsec[,8:35], OMSUpoold[,8:35], TVERpooldfirst[,8:35], TVERpooldsec[,8:35], NNSUpoold[,8:35])
Rupoold <- na.omit(Rupoold)

Rupemo <- rbind(LETIpemo[,8:35], KAIpemofirst[,8:35], KAIpemosec[,8:35], OMSUpemo[,8:35], TVERpemofirst[,8:35], TVERpemosec[,8:35], NNSUpemo[,8:35])
Rupemo <- na.omit(Rupemo)

Rupomo <- rbind(LETIpomo[,8:35], KAIpomofirst[,8:35], KAIpomosec[,8:35], OMSUpomo[,8:35], TVERpomofirst[,8:35], TVERpomosec[,8:35], NNSUpomo[,8:35])
Rupomo <- na.omit(Rupomo)

##RUSSIA FACTOR VALUES: LETI, KAI, OMSU, TVER, NNSU
Rupeoldfa <- Rupeold
for(col in 1:27){Rupeoldfa[,col] <- factor(Rupeoldfa[,col], levels = 1:6, ordered = TRUE)}

Rupeold1 <- likert::likert(as.data.frame(Rupeoldfa[,1:3]), grouping = Rupeold$univ)
plot(Rupeold1, ordered = FALSE)
plot(Rupeold1, type = 'density', ordered = FALSE)

Rupooldfa <- Rupoold
for(col in 1:27){Rupooldfa[,col] <- factor(Rupooldfa[,col], levels = 1:6, ordered = TRUE)}

Rupoold1 <- likert::likert(as.data.frame(Rupooldfa[,1:3]), grouping = Rupoold$univ)
plot(Rupoold1, ordered = FALSE)
plot(Rupoold1, type = 'density', ordered = FALSE)

Rupemofa <- Rupemo
for(col in 1:27){Rupemofa[,col] <- factor(Rupemofa[,col], levels = 1:6, ordered = TRUE)}

Rupemo1 <- likert::likert(as.data.frame(Rupemofa[,1:3]), grouping = Rupemo$univ)
plot(Rupemo1, ordered = FALSE)
plot(Rupemo1, type = 'density', ordered = FALSE)

Rupomofa <- na.omit(Rupomo)
for(col in 1:27){Rupomofa[,col] <- factor(Rupomofa[,col], levels = 1:6, ordered = TRUE)}

Rupomo1 <- likert::likert(as.data.frame(Rupomofa[,1:3]), grouping = Rupomo$univ)
plot(Rupomo1, ordered = FALSE)
plot(Rupomo1, type = 'density', ordered = FALSE)


##CHANGE THE DATA TO GET THE PCA GRAPHS

Data <- Rupomo  
Questions <- Data[,1:27]
Questions.PCA <- prcomp(Questions, center = TRUE, scale = TRUE)
plot(Questions.PCA)

g <- ggbiplot(Questions.PCA, obs.scale = 1, var.scale = 1, groups = Data$univ, ellipse = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
