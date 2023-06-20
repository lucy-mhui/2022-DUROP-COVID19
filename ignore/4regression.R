library(ggplot2)
library(stringr)
library(scales) 
library(nnet)
library(stats4)
library(bbmle)
nextstrain<-read.csv("nextstrain.csv")
GISAID<-read.csv("GISAID.csv")

# Alpha GISAID plot
plot(as.Date(GISAID$Collection_Date), jitter(GISAID$Alpha, 0.15), pch=19)
model<-glm(GISAID$Alpha~GISAID$Collection_Date, binomial)
summary(model)
xv<-seq(min(GISAID$Collection_Date), max(GISAID$Collection_Date), 0.01)
yv<-predict(model, list(GISAID$Collection_Date=xv), type="response")
lines(xv, yv, col="red")




# proprotion vs sample collection date
# Alpha GISAID
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC=="B.1.1.7")))
colnames(GISAID)[7] <- "Alpha"
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC!="B.1.1.7")))
colnames(GISAID)[8] <- "NotAlpha"

GISAID$month <- strftime(GISAID$Collection_Date, "%m")  

GISAIDa <- aggregate(cbind(Alpha, NotAlpha) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

GISAIDa <- data.frame(GISAIDa, GISAIDa$Alpha/GISAIDa$NotAlpha)
colnames(GISAIDa)[4] <- "AlphaProportion"

plot(GISAIDa$month, GISAIDa$AlphaProportion)

# Alpha next strain
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC=="B.1.1.7")))
colnames(nextstrain)[7] <- "Alpha"
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC!="B.1.1.7")))
colnames(nextstrain)[8] <- "NotAlpha"

nextstrain$month <- strftime(nextstrain$Collection_Date, "%m")  

Nextstraina <- aggregate(cbind(Alpha, NotAlpha) ~ month, data = nextstrain, FUN = sum, na.rm = TRUE)

Nextstraina <- data.frame(Nextstraina, Nextstraina$Alpha/Nextstraina$NotAlpha)
colnames(Nextstraina)[4] <- "AlphaProportion"

plot(Nextstraina$month, Nextstraina$AlphaProportion)

# Beta GISAID
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC=="B.1.351")))
colnames(GISAID)[10] <- "Beta"
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC!="B.1.351")))
colnames(GISAID)[11] <- "NotBeta"

GISAIDb <- aggregate(cbind(Beta, NotBeta) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

GISAIDb <- data.frame(GISAIDb, GISAIDb$Beta/GISAIDb$NotBeta)
colnames(GISAIDb)[4] <- "BetaProportion"

plot(GISAIDb$month, GISAIDb$BetaProportion)

# Beta Nextstrain
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC=="B.1.351")))
colnames(nextstrain)[10] <- "Beta"
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC!="B.1.351")))
colnames(nextstrain)[11] <- "NotBeta"

Nextstrainb <- aggregate(cbind(Beta, NotBeta) ~ month, data = nextstrain, FUN = sum, na.rm = TRUE)

Nextstrainb <- data.frame(Nextstrainb, Nextstrainb$Beta/Nextstrainb$NotBeta)
colnames(Nextstrainb)[4] <- "BetaProportion"

plot(Nextstrainb$month, Nextstrainb$BetaProportion)

# Gamma GISAID
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC=="P.1")))
colnames(GISAID)[12] <- "Gamma"
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC!="P.1")))
colnames(GISAID)[13] <- "NotGamma"

GISAIDg <- aggregate(cbind(Gamma, NotGamma) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

GISAIDg <- data.frame(GISAIDg, GISAIDg$Gamma/GISAIDg$NotGamma)
colnames(GISAIDg)[4] <- "GammaProportion"

plot(GISAIDg$month, GISAIDg$GammaProportion)

# Gamma Nextstrain
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC=="P.1")))
colnames(nextstrain)[12] <- "Gamma"
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC!="P.1")))
colnames(nextstrain)[13] <- "NotGamma"

nextstraing <- aggregate(cbind(Gamma, NotGamma) ~ month, data = nextstrain, FUN = sum, na.rm = TRUE)

nextstraing <- data.frame(nextstraing, nextstraing$Gamma/nextstraing$NotGamma)
colnames(nextstraing)[4] <- "GammaProportion"

plot(nextstraing$month, nextstraing$GammaProportion)

# Delta GISAID
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC=="B.1.617.2")))
colnames(GISAID)[14] <- "Delta"
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC!="B.1.617.2")))
colnames(GISAID)[15] <- "NotDelta"

GISAIDd <- aggregate(cbind(Delta, NotDelta) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

GISAIDd <- data.frame(GISAIDd, GISAIDd$Delta/GISAIDd$NotDelta)
colnames(GISAIDd)[4] <- "DeltaProportion"

plot(GISAIDd$month, GISAIDd$DeltaProportion)

# Delta Nextstrain
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC=="B.1.617.2")))
colnames(nextstrain)[14] <- "Delta"
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC!="B.1.617.2")))
colnames(nextstrain)[15] <- "NotDelta"

Nextstraind <- aggregate(cbind(Delta, NotDelta) ~ month, data = nextstrain, FUN = sum, na.rm = TRUE)

Nextstraind <- data.frame(Nextstraind, Nextstraind$Delta/Nextstraind$NotDelta)
colnames(Nextstraind)[4] <- "DeltaProportion"

plot(Nextstraind$month, Nextstraind$DeltaProportion)

# Omicron GISAID
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC=="BA.1")))
colnames(GISAID)[16] <- "Omicron"
GISAID <- data.frame(cbind(GISAID, as.numeric(GISAID$VOC!="BA.1")))
colnames(GISAID)[17] <- "NotOmicron"

GISAIDo <- aggregate(cbind(Omicron, NotOmicron) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

GISAIDo <- data.frame(GISAIDo, GISAIDo$Omicron/GISAIDo$NotOmicron)
colnames(GISAIDo)[4] <- "OmicronProportion"

plot(GISAIDo$month, GISAIDo$OmicronProportion)

# Omicron Nextstrain
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC=="BA.1")))
colnames(nextstrain)[16] <- "Omicron"
nextstrain <- data.frame(cbind(nextstrain, as.numeric(nextstrain$VOC!="BA.1")))
colnames(nextstrain)[17] <- "NotOmicron"

Nextstraino <- aggregate(cbind(Omicron, NotOmicron) ~ month, data = GISAID, FUN = sum, na.rm = TRUE)

Nextstraino <- data.frame(Nextstraino, Nextstraino$Omicron/Nextstraino$NotOmicron)
colnames(Nextstraino)[4] <- "OmicronProportion"

plot(Nextstraino$month, Nextstraino$OmicronProportion)