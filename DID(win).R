setwd("C:/Users/admin/Google ドライブ/大学院/研究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除
headm <-function(x){x[1:5,1:5]}

install.packages("fitdistrplus")

library(corrr)
library(spdep)
library(sf)
library(maptools)
library(memisc)
library(VIM)
library(mice)
library(BaylorEdPsych)
library(texreg)
library(Matching)
library(fitdistrplus)
################################データクレンジング################################
raw.data <- read.csv("DIDdata.fr.csv", fileEncoding="utf-8") #データ読み込み

data <- raw.data[, !(colnames(raw.data) %in% 
	c("Area.ha.", "Employment.in.2012", "Employment.in.2001", "Population.in.2010", 
	"Population.in.2000", "Road.length.in.2010", "Road.length.in.2000"))]# 不要な行削除

#2010年データセット抽出
data2010 <- cbind(data[,1:4],data[,seq(5,14,2)], data[,seq(14,23,2)], 
	data.frame("Tokyo23wards"=c(rep(1,747),rep(0,2096))),data[,24:25], data.frame(AFT=rep(1,2843)))

colnames(data2010) <- c("Zone", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", "Distance.to.Tokyo.Station", 
	"Distance.to.the.nearest.station", "Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.NRT", "Road.density", "Tokyo23wards","TRE.HND", "TRE.NRT", "AFT") #列名統一

#2000年データセット抽出
data2000 <- cbind(data[,1:4],data[,seq(6,13,2)], data[,seq(13,24,2)], 
	data.frame("Tokyo23wards"=c(rep(1,747),rep(0,2096))), data[,24:25], data.frame(AFT=rep(0,2843)))
colnames(data2000) <- c("Zone", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", "Distance.to.Tokyo.Station", 
	"Distance.to.the.nearest.station", "Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.NRT", "Road.density", "Tokyo23wards", "TRE.HND", "TRE.NRT", "AFT") #列名統一

################距離から推定した旅行時間を用いたアクセシビリティのデータ###############
airport <- read.csv("accessbility_to_airport.csv")
data2010$Accessibility.to.HND <- airport$ACC_to_HND2010
data2010$Accessibility.to.NRT <- airport$ACC_to_NRT2010
data2010$TRE.HND <- airport$TRT_HND
data2010$TRE.NRT <- airport$TRT_NRT

data2000$Accessibility.to.HND <- airport$ACC_to_HND2000
data2000$Accessibility.to.NRT <- airport$ACC_to_NRT2000
data2000$TRE.HND <- airport$TRT_HND
data2000$TRE.NRT <- airport$TRT_NRT
################距離から推定した旅行時間を用いたアクセシビリティのデータ###############
panel <- rbind(data2010, data2000)
panel$AFT <- as.integer(panel$AFT)
panel$Tokyo23wards <- as.integer(panel$Tokyo23wards)
panel$did.HND <- panel$TRE.HND * panel$AFT
panel$did.NRT <- panel$TRE.NRT * panel$AFT

#Ef.D <- read.csv("Efective Density.csv")
#panel$Ef.D <- Ef.D[,2]
UURA <- read.csv("UURA.csv")
panel$UURA <- UURA[,4]
panel$UURA_AFT <- panel$UURA * panel$AFT
UURA_corv <- read.csv("UURA_corv.csv")
panel$UURA_corv <- UURA_corv[,4]

TRE.HND_UURA <- as.matrix(panel$TRE.HND + UURA[,4])
TRE.NRT_UURA <- as.matrix(panel$TRE.NRT + UURA[,4])

UURA$TRE.HND_UURA <- TRE.HND_UURA
UURA$TRE.NRT_UURA <- TRE.NRT_UURA
UURA$num <- c(1:5686)
head(UURA)
UHND <- subset(UURA, TRE.HND_UURA==2)
UNRT <- subset(UURA, TRE.NRT_UURA==2)

UURA$TRE.HND_UURA[UHND$num,] <- 1
UURA$TRE.NRT_UURA[UNRT$num,] <- 1

panel$HNDorUURA <- UURA[,5]
panel$NRTorUURA <- UURA[,6]
panel$HNDandUURA <- panel$TRE.HND*panel$UURA
panel$NRTandUURA <- panel$TRE.NRT*panel$UURA
panel$HNDorUURA_AFT <- panel$HNDorUURA*panel$AFT
panel$NRTorUURA_AFT <- panel$NRTorUURA*panel$AFT
panel$HNDandUURA_AFT <- panel$HNDandUURA*panel$AFT
panel$NRTandUURA_AFT <- panel$NRTandUURA*panel$AFT

TRE_dif <- as.matrix(data2010$TRE.HND - data2010$TRE.NRT)
TRE_int <- data.frame(int=data2010$TRE.HND*data2010$TRE.NRT, num=c(1:2843))
HNDNRT <- subset(TRE_int, int==1)

TRE_dif[HNDNRT$num,] <- 2

TRE_dif <- data.frame(TRE=as.integer(TRE_dif))


write.csv(TRE_dif, file ="TRE_dif.csv")
#####10分閾値つくる#####
acc_HND2010 <- data2010$Accessibility.to.HND
acc_NRT2010 <- data2010$Accessibility.to.NRT
acc_HND2000 <- data2000$Accessibility.to.HND
acc_NRT2000 <- data2000$Accessibility.to.NRT

dif_HND <- as.data.frame(acc_HND2000 - acc_HND2010)
dif_NRT <- as.data.frame(acc_NRT2000 - acc_NRT2010)
names(dif_HND) <- c("dif_HND")
names(dif_NRT) <- c("dif_NRT")
dif_HND$num <- 1:2843
dif_NRT$num <- 1:2843

t_HND <- subset(dif_HND, dif_HND >= 10)
t_NRT <- subset(dif_NRT, dif_NRT >= 10)
HNDtre_10 <- matrix(0, 2843, 1)
NRTtre_10 <- matrix(0, 2843, 1)

HNDtre_10[t_HND$num,] <- 1
HNDtre_10 <- rbind(HNDtre_10, HNDtre_10)
NRTtre_10[t_NRT$num,] <- 1
NRTtre_10 <- rbind(NRTtre_10, NRTtre_10)
#####10分閾値つくる#####
head(panel)
################データの妥当性の確認###############
summary(panel)
sapply(panel, function(y) sum(is.na(y)))
sapply(panel, function(y) sum(is.nan(y)))
sapply(panel, function(y) sum(is.infinite(y)))
################相関行列###############
cormatrix <- correlate(panel[,-1:-3])
write.csv(cormatrix, file="correlation matrix.csv")
################相関行列################
################データの妥当性の確認###############

################欠損値対応###############
########多重代入法#######
panel.imp <- panel[, !(colnames(panel) %in% 
	c("Zone", "Munincipalities", "Town", "Accessibility.to.HND", 
		"Accessibility.to.NRT", "TRE.HND", "TRE.NRT", "did.HND", "did.NRT", 
		"UURA", "UURA_AFT", "HNDorUURA", "NRTorUURA", "HNDandUURA", "NRTandUURA",
		"HNDorUURA_AFT", "NRTorUURA_AFT", "HNDandUURA_AFT", "NRTandUURA_AFT" ))] 
head(panel.imp)
str(panel.imp)
summary(panel.imp)
test.mcar <- LittleMCAR(panel.imp)
test.mcar$missing.patterns
test.mcar$p.value
summary(test.mcar)

imp <- mice(panel.imp, m=20)
fit.ED <- with(data = imp, lm(Employment.density ~ Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station +Road.density + Tokyo23wards 
	+AFT +UURA_corv))
summary(pool(fit.ED))

completed.data <- complete(imp)
head(completed.data)
summary(completed.data)
completed.data[c(179:181,3022:3024,2545,5388),]
summary(panel[,-1:-3])

completed.panel <- panel
completed.panel$Land.Price <- completed.data$Land.Price
completed.panel$Population.density <- completed.data$Population.density
completed.panel$Employment.density <- completed.data$Employment.density
summary(completed.panel)
write.csv(completed.panel, "completed.panel.csv")
panel.HND <- completed.panel[-c(179:181,3022:3024),]
panel.NRT <- completed.panel[-c(2545,5388),]

panel_10 <- completed.panel
panel_10$TRE.HND <- HNDtre_10
panel_10$TRE.NRT <- NRTtre_10
panel_10$did.HND <- HNDtre_10*panel_10$AFT
panel_10$did.NRT <- NRTtre_10*panel_10$AFT
panel.HND_10 <- panel_10[-c(179:181,3022:3024),]
panel.NRT_10 <- panel_10[-c(2545,5388),]

#######相関行列#######
cormatrix.HND <- correlate(panel.HND[,-1:-3])
cormatrix.NRT <- correlate(panel.NRT[,-1:-3])


panelforcor <- completed.panel[, c(-1:-3, -21:-22, -24:-31)]
head(panelforcor)
cormatrix <- correlate(panelforcor)
write.csv(cormatrix, file = "cormatrix.csv")
write.csv(cormatrix.HND, file="correlationHND.csv")
write.csv(cormatrix.NRT, file="correlationNRT.csv")
#######相関行列#######
########多重代入法#######
################欠損値対応###############

completed.panel$HNDtre_10 <- HNDtre_10
completed.panel$NRTtre_10 <- NRTtre_10

################プロペンシティスコアマッチング###############
comp <- completed.panel[complete.cases(completed.panel),]
comp2010 <- comp[1:2839,]
comp2000 <- comp[2840:5678,]
comp1 <- comp2010[,-24:-31]
comp2 <- comp2000[,c(-1:-4, -9, -15:-17,-19:-31)]
head(comp2)
head(comp1)
comp_pscore <- cbind(comp1, comp2)
comp_pscore <- comp_pscore[,-19:-22]
comp_pscore$d_pd <- comp_pscore$Population.density - comp_pscore$Population.density.1
comp_pscore$d_ed <- comp_pscore$Employment.density - comp_pscore$Employment.density.1
comp_pscore$d_lp <- comp_pscore$Land.Price - comp_pscore$Land.Price.1

str(comp_pscore)
logiav.PD.HND <- glm(TRE.HND ~ Area + Mobility + Land.Price + Employment.density 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Accessibility.to.HND + Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)

logiav.ED.HND <- glm(TRE.HND ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station
	+ Accessibility.to.HND + Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)

logiav.LP.HND <- glm(TRE.HND ~ Area + Mobility +Population.density
	+ Employment.density
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)


logiav.PD.NRT <- glm(TRE.NRT ~  Area + Mobility + Land.Price 
	+ Employment.density
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)

logiav.ED.NRT <- glm(TRE.NRT ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Accessibility.to.HND + Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)

logiav.LP.NRT <- glm(TRE.NRT ~ Area + Mobility +Population.density
	+ Employment.density
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ UURA_corv, family=binomial(link = "logit"), data=comp_pscore)

summary(logiav.PD.HND)
summary(logiav.ED.HND)
summary(logiav.LP.HND)
summary(logiav.PD.NRT)
summary(logiav.ED.NRT)
summary(logiav.LP.NRT)

PD.HNDav <- Match(Y=comp_pscore$d_pd, Tr=(comp_pscore$TRE.HND==1), X=logiav.PD.HND$fitted.values)
ED.HNDav <- Match(Y=comp_pscore$d_ed, Tr=(comp_pscore$TRE.HND==1), X=logiav.ED.HND$fitted.values)
LP.HNDav <- Match(Y=comp_pscore$d_lp, Tr=(comp_pscore$TRE.HND==1), X=logiav.LP.HND$fitted.values)
PD.NRTav <- Match(Y=comp_pscore$d_pd, Tr=(comp_pscore$TRE.NRT==1), X=logiav.PD.NRT$fitted.values)
ED.NRTav <- Match(Y=comp_pscore$d_ed, Tr=(comp_pscore$TRE.NRT==1), X=logiav.ED.NRT$fitted.values)
LP.NRTav <- Match(Y=comp_pscore$d_lp, Tr=(comp_pscore$TRE.NRT==1), X=logiav.LP.NRT$fitted.values)

ATE.PD.HNDav <- summary.Match(PD.HNDav)
ATE.ED.HNDav <- summary.Match(ED.HNDav)
ATE.LP.HNDav <- summary.Match(LP.HNDav)
ATE.PD.NRTav <- summary.Match(PD.NRTav)
ATE.ED.NRTav <- summary.Match(ED.NRTav)
ATE.LP.NRTav <- summary.Match(LP.NRTav)
################プロペンシティスコアマッチング###############

################################データクレンジング################################

################################回帰分析################################


################################Spatial################################
################SWM(disntance inverse)#################
Tokyo <- readShapePoly("Tokyo.shp")
plot(Tokyo)
coords <- coordinates(Tokyo)
head(coords)
dmatrix <- spDists(coords, longlat=TRUE)
headm(dmatrix)
d_inverse <- 1/dmatrix
d_inverse[which(is.infinite(d_inverse))] <- 0
headm(d_inverse)
################SWM(disntance inverse)#################
################SWM(Travel time inverse)################
t_inv2010 <- as.matrix(read.csv("SWM_traveltime2010.csv"))
t_inv2000 <- as.matrix(read.csv("SWM_traveltime2000.csv"))

t_inv2010 <- t_inv2010[,-1]
t_inv2000 <- t_inv2000[,-1]
t_inv2010 <- unname(t_inv2010)
t_inv2000 <- unname(t_inv2000)
headm(t_inv2010)

w_HND2010 <- t_inv2010[-(179:181), -(179:181)]
w_HND2000 <- t_inv2000[-(179:181), -(179:181)]
w_NRT2010 <- t_inv2010[-2545, -2545]
w_NRT2000 <- t_inv2000[-2545, -2545]

zm_HND <- matrix(0, nrow = nrow(w_HND2010), ncol = ncol(w_HND2010))
zm_NRT <- matrix(0, nrow = nrow(w_NRT2010), ncol = ncol(w_NRT2010))

m_HND2010 <- cbind(w_HND2010, zm_HND)
m_HND2000 <- cbind(zm_HND, w_HND2000)

m_NRT2010 <- cbind(w_NRT2010, zm_NRT)
m_NRT2000 <- cbind(zm_NRT, w_NRT2000)

w_HND <- rbind(m_HND2010, m_HND2000)
w_NRT <- rbind(m_NRT2010, m_NRT2000)

swm_HND <- mat2listw(w_HND, style="W")
swm_NRT <- mat2listw(w_NRT, style="W")

###############TRE DID################
########OLS,SLM and SEM(mobility)#########
#OLS
PD.HND.OLSm_TRE <- glm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
PD.NRT.OLSm_TRE <- glm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
ED.HND.OLSm_TRE <- glm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
ED.NRT.OLSm_TRE <- glm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
LP.HND.OLSm_TRE <- glm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ UURA_AFT + Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
LP.NRT.OLSm_TRE <- glm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)


PD.HND.OLSm_TRE <- glm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
PD.NRT.OLSm_TRE <- glm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
ED.HND.OLSm_TRE <- glm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
ED.NRT.OLSm_TRE <- glm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
LP.HND.OLSm_TRE <- glm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ UURA_AFT + Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
LP.NRT.OLSm_TRE <- glm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
#SLM
SLM_PD_HNDm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_ED_HNDm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_LP_HNDm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SLM_PD_NRTm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_ED_HNDm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_LP_HNDm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SEM_PD_NRTm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SDM
SDM_PD_HNDm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_ED_HNDm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_LP_HNDm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)

SDM_PD_NRTm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_ED_NRTm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_LP_NRTm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
########OLS,SLM and SEM(mobility)#########
###############TRE DID################

###############TRE DID################
htmlreg(list(PD.HND.OLSm_TRE, SLM_PD_HNDm_TRE,　SEM_PD_HNDm_TRE,　SDM_PD_HNDm_TRE, 
	PD.NRT.OLSm_TRE, SLM_PD_NRTm_TRE, SEM_PD_NRTm_TRE, SDM_PD_NRTm_TRE), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TRE_10) Analyses for Ln(Population density)", 
	caption.above =TRUE, "PD_TRE_10.html")

htmlreg(list(ED.HND.OLSm_TRE, SLM_ED_HNDm_TRE, SEM_ED_HNDm_TRE, SDM_ED_HNDm_TRE, 
	ED.NRT.OLSm_TRE, SLM_ED_NRTm_TRE, SEM_ED_NRTm_TRE, SDM_ED_NRTm_TRE), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TRE_10) Analyses for Ln(Employment density)", 
	caption.above =TRUE, "ED_TRE_10.html")

htmlreg(list(LP.HND.OLSm_TRE, SLM_LP_HNDm_TRE, SEM_LP_HNDm_TRE, SDM_LP_HNDm_TRE, 
	LP.NRT.OLSm_TRE, SLM_LP_NRTm_TRE, SEM_LP_NRTm_TRE, SDM_LP_NRTm_TRE), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TRE_10) Analyses for Ln(Land Price)", 
	caption.above =TRUE, "LP_TRE_10.html")

###############TRE DID################


###############UURA DID################
########OLS,SLM and SEM(mobility)#########
#OLS
PD.HND.OLSm_UURA <- lm(log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND , data=panel.HND)
ED.HND.OLSm_UURA <- lm(log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND , data=panel.HND)
LP.HND.OLSm_UURA <- lm(log(Land.Price) ~ UURA + AFT + UURA_AFT  
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND , data=panel.HND)

PD.NRT.OLSm_UURA <- lm(log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT , data=panel.NRT)
ED.NRT.OLSm_UURA <- lm(log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT , data=panel.NRT)
LP.NRT.OLSm_UURA <- lm(log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT , data=panel.NRT)
#SLM
SLM_PD_HNDm_UURA <- lagsarlm(formula = log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)
SLM_ED_HNDm_UURA <- lagsarlm(formula = log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)
SLM_LP_HNDm_UURA <- lagsarlm(formula = log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)

SLM_PD_NRTm_UURA <- lagsarlm(formula = log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm_UURA <- lagsarlm(formula = log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm_UURA <- lagsarlm(formula = log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDm_UURA <- errorsarlm(formula = log(Population.density) ~  UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)
SEM_ED_HNDm_UURA <- errorsarlm(formula = log(Employment.density) ~  UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)
SEM_LP_HNDm_UURA <- errorsarlm(formula = log(Land.Price) ~  UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND)

SEM_PD_NRTm_UURA <- errorsarlm(formula = log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm_UURA <- errorsarlm(formula = log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm_UURA <- errorsarlm(formula = log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT)

#SDM
SDM_PD_HNDm_UURA <- lagsarlm(formula = log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND, Durbin = TRUE)
SDM_ED_HNDm_UURA <- lagsarlm(formula = log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND, Durbin = TRUE)
SDM_LP_HNDm_UURA <- lagsarlm(formula = log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.HND, data = panel.HND, listw = swm_HND, Durbin = TRUE)

SDM_PD_NRTm_UURA <- lagsarlm(formula = log(Population.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_ED_NRTm_UURA <- lagsarlm(formula = log(Employment.density) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_LP_NRTm_UURA <- lagsarlm(formula = log(Land.Price) ~ UURA + AFT + UURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility + Accessibility.to.NRT, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
########OLS,SLM and SEM(mobility)#########
###############UURA DID################

###############TRE or UURA DID################
########SLM and SEM(mobility)#########
#OLS
PD.HND.OLSm_TREorUURA <- lm(log(Population.density) ~ HNDorUURA + AFT + HNDorUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
PD.NRT.OLSm_TREorUURA <- lm(log(Population.density) ~ NRTorUURA + AFT + NRTorUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
ED.HND.OLSm_TREorUURA <- lm(log(Employment.density) ~ HNDorUURA + AFT + HNDorUURA_AFT  
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
ED.NRT.OLSm_TREorUURA <- lm(log(Employment.density) ~ NRTorUURA + AFT + NRTorUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
LP.HND.OLSm_TREorUURA <- lm(log(Land.Price) ~ HNDorUURA + AFT + HNDorUURA_AFT  
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
LP.NRT.OLSm_TREorUURA <- lm(log(Land.Price) ~ NRTorUURA + AFT + NRTorUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
#SLM
SLM_PD_HNDm_TREorUURA <- lagsarlm(formula = log(Population.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_ED_HNDm_TREorUURA <- lagsarlm(formula = log(Employment.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_LP_HNDm_TREorUURA <- lagsarlm(formula = log(Land.Price) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SLM_PD_NRTm_TREorUURA <- lagsarlm(formula = log(Population.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm_TREorUURA <- lagsarlm(formula = log(Employment.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm_TREorUURA <- lagsarlm(formula = log(Land.Price) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDm_TREorUURA <- errorsarlm(formula = log(Population.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_ED_HNDm_TREorUURA <- errorsarlm(formula = log(Employment.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_LP_HNDm_TREorUURA <- errorsarlm(formula = log(Land.Price) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SEM_PD_NRTm_TREorUURA <- errorsarlm(formula = log(Population.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm_TREorUURA <- errorsarlm(formula = log(Employment.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm_TREorUURA <- errorsarlm(formula = log(Land.Price) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)

#SDM
SDM_PD_HNDm_TREorUURA <- lagsarlm(formula = log(Population.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_ED_HNDm_TREorUURA <- lagsarlm(formula = log(Employment.density) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_LP_HNDm_TREorUURA <- lagsarlm(formula = log(Land.Price) ~ HNDorUURA + AFT 
	+ HNDorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)

SDM_PD_NRTm_TREorUURA <- lagsarlm(formula = log(Population.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_ED_NRTm_TREorUURA <- lagsarlm(formula = log(Employment.density) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_LP_NRTm_TREorUURA <- lagsarlm(formula = log(Land.Price) ~ NRTorUURA + AFT 
	+ NRTorUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
########OLS, SLM and SEM(mobility)#########
###############TRE or UURA DID################

###############TRE&UURA DID################
########OLS,SLM and SEM(mobility)#########
#OLS
PD.HND.OLSm_TREandUURA <- lm(log(Population.density) ~ HNDandUURA + AFT + HNDandUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
PD.NRT.OLSm_TREandUURA <- lm(log(Population.density) ~ NRTandUURA + AFT + NRTandUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
ED.HND.OLSm_TREandUURA <- lm(log(Employment.density) ~ HNDandUURA + AFT + HNDandUURA_AFT  
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
ED.NRT.OLSm_TREandUURA <- lm(log(Employment.density) ~ NRTandUURA + AFT + NRTandUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
LP.HND.OLSm_TREandUURA <- lm(log(Land.Price) ~ HNDandUURA + AFT + HNDandUURA_AFT  
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
LP.NRT.OLSm_TREandUURA <- lm(log(Land.Price) ~ NRTandUURA + AFT + NRTandUURA_AFT 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
#SLM
SLM_PD_HNDm_TREandUURA <- lagsarlm(formula = log(Population.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_ED_HNDm_TREandUURA <- lagsarlm(formula = log(Employment.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_LP_HNDm_TREandUURA <- lagsarlm(formula = log(Land.Price) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SLM_PD_NRTm_TREandUURA <- lagsarlm(formula = log(Population.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm_TREandUURA <- lagsarlm(formula = log(Employment.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm_TREandUURA <- lagsarlm(formula = log(Land.Price) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDm_TREandUURA <- errorsarlm(formula = log(Population.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_ED_HNDm_TREandUURA <- errorsarlm(formula = log(Employment.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_LP_HNDm_TREandUURA <- errorsarlm(formula = log(Land.Price) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SEM_PD_NRTm_TREandUURA <- errorsarlm(formula = log(Population.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm_TREandUURA <- errorsarlm(formula = log(Employment.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm_TREandUURA <- errorsarlm(formula = log(Land.Price) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SDM
SDM_PD_HNDm_TREandUURA <- lagsarlm(formula = log(Population.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_ED_HNDm_TREandUURA <- lagsarlm(formula = log(Employment.density) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)
SDM_LP_HNDm_TREandUURA <- lagsarlm(formula = log(Land.Price) ~ HNDandUURA + AFT 
	+ HNDandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND, Durbin = TRUE)

SDM_PD_NRTm_TREandUURA <- lagsarlm(formula = log(Population.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_ED_NRTm_TREandUURA <- lagsarlm(formula = log(Employment.density) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
SDM_LP_NRTm_TREandUURA <- lagsarlm(formula = log(Land.Price) ~ NRTandUURA + AFT 
	+ NRTandUURA_AFT  + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT, Durbin = TRUE)
########OLS,SLM and SEM(mobility)#########
###############TRE&UURA DID################

###############UURA DID################
htmlreg(list(PD.HND.OLSm_UURA, SLM_PD_HNDm_UURA, SEM_PD_HNDm_UURA, SDM_PD_HNDm_UURA, 
	PD.NRT.OLSm_UURA, SLM_PD_NRTm_UURA, SEM_PD_NRTm_UURA, SDM_PD_NRTm_UURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(UURA) Analyses for Ln(Population density)", 
	caption.above =TRUE, "PD_UURA.html")

htmlreg(list(ED.HND.OLSm_UURA, SLM_ED_HNDm_UURA, SEM_ED_HNDm_UURA, SDM_ED_HNDm_UURA, 
	ED.NRT.OLSm_UURA, SLM_ED_NRTm_UURA, SEM_ED_NRTm_UURA, SDM_ED_NRTm_UURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(UURA) Analyses for Ln(Employment density)", 
	caption.above =TRUE, "ED_UURA.html")

htmlreg(list(LP.HND.OLSm_UURA, SLM_LP_HNDm_UURA, SEM_LP_HNDm_UURA, SDM_LP_HNDm_UURA, 
	LP.NRT.OLSm_UURA, SLM_LP_NRTm_UURA, SEM_LP_NRTm_UURA, SDM_LP_NRTm_UURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(UURA) Analyses for Ln(Land Price)", 
	caption.above =TRUE, "LP_UURA.html")
###############UURA DID################
###############TREorUURA DID################
htmlreg(list(PD.HND.OLSm_TREorUURA, SLM_PD_HNDm_TREorUURA, SEM_PD_HNDm_TREorUURA, SDM_PD_HNDm_TREorUURA, 
	PD.NRT.OLSm_TREorUURA, SLM_PD_NRTm_TREorUURA, SEM_PD_NRTm_TREorUURA, SDM_PD_NRTm_TREorUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREorUURA) Analyses for Ln(Population density)", 
	caption.above =TRUE, "PD_TREorUURA.html")

htmlreg(list(ED.HND.OLSm_TREorUURA, SLM_ED_HNDm_TREorUURA, SEM_ED_HNDm_TREorUURA, SDM_ED_HNDm_TREorUURA, 
	ED.NRT.OLSm_TREorUURA, SLM_ED_NRTm_TREorUURA, SEM_ED_NRTm_TREorUURA, SDM_ED_NRTm_TREorUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREorUURA) Analyses for Ln(Employment density)", 
	caption.above =TRUE, "ED_TREorUURA.html")

htmlreg(list(LP.HND.OLSm_TREorUURA, SLM_LP_HNDm_TREorUURA, SEM_LP_HNDm_TREorUURA, SDM_LP_HNDm_TREorUURA, 
	LP.NRT.OLSm_TREorUURA, SLM_LP_NRTm_TREorUURA, SEM_LP_NRTm_TREorUURA, SDM_LP_NRTm_TREorUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREorUURA) Analyses for Ln(Land Price)", 
	caption.above =TRUE, "LP_TREorUURA.html")
###############TREorUURA DID################
###############TREandUURA DID################
htmlreg(list(PD.HND.OLSm_TREandUURA, SLM_PD_HNDm_TREandUURA, SEM_PD_HNDm_TREandUURA, SDM_PD_HNDm_TREandUURA, 
	PD.NRT.OLSm_TREandUURA, SLM_PD_NRTm_TREandUURA, SEM_PD_NRTm_TREandUURA, SDM_PD_NRTm_TREandUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREandUURA) Analyses for Ln(Population density)", 
	caption.above =TRUE, "PD_TREandUURA.html")

htmlreg(list(ED.HND.OLSm_TREandUURA, SLM_ED_HNDm_TREandUURA, SEM_ED_HNDm_TREandUURA, SDM_ED_HNDm_TREandUURA, 
	ED.NRT.OLSm_TREandUURA, SLM_ED_NRTm_TREandUURA, SEM_ED_NRTm_TREandUURA, SDM_ED_NRTm_TREandUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREandUURA) Analyses for Ln(Employment density)", 
	caption.above =TRUE, "ED_TREandUURA.html")

htmlreg(list(LP.HND.OLSm_TREandUURA, SLM_LP_HNDm_TREandUURA, SEM_LP_HNDm_TREandUURA, SDM_LP_HNDm_TREandUURA, 
	LP.NRT.OLSm_TREandUURA, SLM_LP_NRTm_TREandUURA, SEM_LP_NRTm_TREandUURA, SDM_LP_NRTm_TREandUURA), 
	custom.model.names=c("OLS", "SLM", "SEM", "SDM", "OLS", "SLM", "SEM", "SDM"), 
	digits = 3, caption = "Estimation Results of DID(TREandUURA) Analyses for Ln(Land Price)", 
	caption.above =TRUE, "LP_TREandUURA.html")

###############TREandUURA DID################

###############閾値10分#############
#OLS
PD.HND.OLSm_TRE <- lm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND_10)
PD.NRT.OLSm_TRE <- lm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT_10)
ED.HND.OLSm_TRE <- lm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND_10)
ED.NRT.OLSm_TRE <- lm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT_10)
LP.HND.OLSm_TRE <- lm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ UURA_AFT + Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND_10)
LP.NRT.OLSm_TRE <- lm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area  + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT_10)
#SLM
SLM_PD_HNDm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)
SLM_ED_HNDm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)
SLM_LP_HNDm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)

SLM_PD_NRTm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
SLM_ED_NRTm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
SLM_LP_NRTm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
#SEM
SEM_PD_HNDm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)
SEM_ED_HNDm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)
SEM_LP_HNDm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND)

SEM_PD_NRTm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
SEM_ED_NRTm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
SEM_LP_NRTm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT)
#SDM
SDM_PD_HNDm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND, Durbin = TRUE)
SDM_ED_HNDm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND, Durbin = TRUE)
SDM_LP_HNDm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND_10, listw=swm_HND, Durbin = TRUE)

SDM_PD_NRTm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT, Durbin = TRUE)
SDM_ED_NRTm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT, Durbin = TRUE)
SDM_LP_NRTm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area  
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT_10, listw=swm_NRT, Durbin = TRUE)
########OLS,SLM and SEM(mobility)#########
###############TRE DID################
###############閾値10分#############
################ノンパラメトリック分析################
#HNEDA
#データからTRTATMENTとCONTOL抽出
TRT.HND2010 <- subset(data2010, TRT.HND ==1)
CTR.HND2010 <- subset(data2010, TRT.HND ==0)
TRT.HND2000 <- subset(data2000, TRT.HND ==1)
CTR.HND2000 <- subset(data2000, TRT.HND ==0)

#Population Densityの平均値
P.TRT.HND2010 <- mean(TRT.HND2010$Population.density, na.rm =TRUE)
P.CTR.HND2010 <- mean(CTR.HND2010$Population.density, na.rm =TRUE)
P.TRT.HND2000 <- mean(TRT.HND2000$Population.density, na.rm =TRUE)
P.CTR.HND2000 <- mean(CTR.HND2000$Population.density, na.rm =TRUE)

#Employment Densityの平均値
E.TRT.HND2010 <- mean(TRT.HND2010$Employment.density, na.rm =TRUE)
E.CTR.HND2010 <- mean(CTR.HND2010$Employment.density, na.rm =TRUE)
E.TRT.HND2000 <- mean(TRT.HND2000$Employment.density, na.rm =TRUE)
E.CTR.HND2000 <- mean(CTR.HND2000$Employment.density, na.rm =TRUE)

#Land Priceの平均値
L.TRT.HND2010 <- mean(TRT.HND2010$Land.Price, na.rm =TRUE)
L.CTR.HND2010 <- mean(CTR.HND2010$Land.Price, na.rm =TRUE)
L.TRT.HND2000 <- mean(TRT.HND2000$Land.Price, na.rm =TRUE)
L.CTR.HND2000 <- mean(CTR.HND2000$Land.Price, na.rm =TRUE)

mean2010HND <- rbind(P.TRT.HND2010,P.CTR.HND2010,E.TRT.HND2010,E.CTR.HND2010,L.TRT.HND2010,L.CTR.HND2010)
mean2000HND <- rbind(P.TRT.HND2000,P.CTR.HND2000,E.TRT.HND2000,E.CTR.HND2000,L.TRT.HND2000,L.CTR.HND2000)
meanHND <-cbind(mean2010HND, mean2000HND)
d.HND <- cbind(meanHND[1:6,1]-meanHND[1:6,2])
ATT.HND <- rbind(d.HND[1,]-d.HND[2,],NA,d.HND[3,]-d.HND[4,],NA,d.HND[5,]-d.HND[6,],NA)

d.P.TRT.HND <- (TRT.HND2010$Population.density - TRT.HND2000$Population.density)
d.P.CTR.HND <- (CTR.HND2010$Population.density - CTR.HND2000$Population.density)
d.E.TRT.HND <- (TRT.HND2010$Employment.density - TRT.HND2000$Employment.density)
d.E.CTR.HND <- (CTR.HND2010$Employment.density - CTR.HND2000$Employment.density)
d.L.TRT.HND <- (TRT.HND2010$Land.Price - TRT.HND2000$Land.Price)
d.L.CTR.HND <- (CTR.HND2010$Land.Price - CTR.HND2000$Land.Price)

mean(d.P.TRT.HND, na.rm =TRUE)
mean(d.P.CTR.HND, na.rm =TRUE)

t.P.HND <- t.test(d.P.TRT.HND,d.P.CTR.HND,var.equal=F)
t.E.HND <- t.test(d.E.TRT.HND,d.E.CTR.HND,var.equal=F)
t.L.HND <- t.test(d.L.TRT.HND, d.L.CTR.HND, var.equal=F)
p.value.P.HND <- t.P.HND$p.value
p.value.E.HND <- t.E.HND$p.value
p.value.L.HND <- t.L.HND$p.value
p.value.HND <- rbind(p.value.P.HND, NA, p.value.E.HND, NA, p.value.L.HND, NA)

n.t <- nrow(TRT.HND2010)
n.c <- nrow(CTR.HND2010)
Zones <- rbind(n.t n.c,n.t, n.c,n.t, n.c)

NPT.HND <- as.data.frame(cbind(mean2010HND, mean2000HND, d.HND, ATT.HND, p.value.HND, Zones))
colnames(NPT.HND) <- c("2010","2000", "Difference", "ATT", "p-value", "Zones")
rownames(NPT.HND) <- c(1:6)

#NRITA
#データからTREATMENTとCONTOL抽出
TRT.NRT2010 <- subset(data2010, TRT.NRT==1)
CTR.NRT2010 <- subset(data2010, TRT.NRT==0)
TRT.NRT2000 <- subset(data2000, TRT.NRT==1)
CTR.NRT2000 <- subset(data2000, TRT.NRT==0)

#Population Densityの平均値
P.TRT.NRT2010 <- mean(TRT.NRT2010$Population.density, na.rm =TRUE)
P.CTR.NRT2010 <- mean(CTR.NRT2010$Population.density, na.rm =TRUE)
P.TRT.NRT2000 <- mean(TRT.NRT2000$Population.density, na.rm =TRUE)
P.CTR.NRT2000 <- mean(CTR.NRT2000$Population.density, na.rm =TRUE)

#Employment Densityの平均値
E.TRT.NRT2010 <- mean(TRT.NRT2010$Employment.density, na.rm =TRUE)
E.CTR.NRT2010 <- mean(CTR.NRT2010$Employment.density, na.rm =TRUE)
E.TRT.NRT2000 <- mean(TRT.NRT2000$Employment.density, na.rm =TRUE)
E.CTR.NRT2000 <- mean(CTR.NRT2000$Employment.density, na.rm =TRUE)

#Land Priceの平均値
L.TRT.NRT2010 <- mean(TRT.NRT2010$Land.Price, na.rm =TRUE)
L.CTR.NRT2010 <- mean(CTR.NRT2010$Land.Price, na.rm =TRUE)
L.TRT.NRT2000 <- mean(TRT.NRT2000$Land.Price, na.rm =TRUE)
L.CTR.NRT2000 <- mean(CTR.NRT2000$Land.Price, na.rm =TRUE)

mean2010NRT <- rbind(P.TRT.NRT2010,P.CTR.NRT2010,E.TRT.NRT2010,E.CTR.NRT2010,L.TRT.NRT2010,L.CTR.NRT2010)
mean2000NRT <- rbind(P.TRT.NRT2000,P.CTR.NRT2000,E.TRT.NRT2000,E.CTR.NRT2000,L.TRT.NRT2000,L.CTR.NRT2000)

meanNRT <- cbind(mean2010NRT, mean2000NRT)
d.NRT <- cbind(meanNRT[1:6,1]-meanNRT[1:6,2])
ATT.NRT <- rbind(d.NRT[1,]-d.NRT[2,],NA,d.NRT[3,]-d.NRT[4,],NA,d.NRT[5,]-d.NRT[6,],NA)

d.P.TRT.NRT <- (TRT.NRT2010$Population.density - TRT.NRT2000$Population.density)
d.P.CTR.NRT <- (CTR.NRT2010$Population.density - CTR.NRT2000$Population.density)
d.E.TRT.NRT <- (TRT.NRT2010$Employment.density - TRT.NRT2000$Employment.density)
d.E.CTR.NRT <- (CTR.NRT2010$Employment.density - CTR.NRT2000$Employment.density)
d.L.TRT.NRT <- (TRT.NRT2010$Land.Price - TRT.NRT2000$Land.Price)
d.L.CTR.NRT <- (CTR.NRT2010$Land.Price - CTR.NRT2000$Land.Price)

t.P.NRT <- t.test(d.P.TRT.NRT,d.P.CTR.NRT,var.equal=F)
t.E.NRT <- t.test(d.E.TRT.NRT,d.E.CTR.NRT,var.equal=F)
t.L.NRT <- t.test(d.L.TRT.NRT, d.L.CTR.NRT, var.equal=F)

p.value.P.NRT <- t.P.NRT$p.value
p.value.E.NRT <- t.E.NRT$p.value
p.value.L.NRT <- t.L.NRT$p.value
p.value.NRT <- rbind(p.value.P.NRT, NA, p.value.E.NRT, NA, p.value.L.NRT, NA)

n.t <- nrow(TRT.NRT2010)
n.c <- nrow(CTR.NRT2010)
Zones <- rbind(n.t n.c,n.t, n.c,n.t, n.c)

NPT.NRT <- as.data.frame(cbind(mean2010NRT, mean2000NRT, d.NRT, ATT.NRT, p.value.NRT, Zones))
colnames(NPT.NRT) <- c("2010","2000", "Difference", "ATT", "p-value", "Zones")
rownames(NPT.NRT) <- c(1:6)

NPT.w <-rbind(NPT.HND, NPT.NRT)

#データフレーム作成
case <- c("HANEDA",NA,NA,NA,NA,NA,"NARITA",NA,NA,NA,NA,NA)
var <- c("Population density", NA, "Employment density", NA, "Land price",
 NA,"Population density", NA, "Employment density", NA, "Land price", NA)
group <- c("Treatment","Control","Treatment","Control","Treatment","Control",
	"Treatmet","Control","Treatment","Control","Treatment","Control")

name <- data.frame(Case=case, Variable=var, Group=group)

NPT <- cbind(name,NPT.w) 

write.csv(NPT, file="Non-Parametric Test.csv", row.names=FALSE)
################ノンパラメトリック分析################