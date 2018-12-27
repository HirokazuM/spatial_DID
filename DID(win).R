setwd("C:/Users/admin/Google ドライブ/大学院/研究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除
headm <-function(x){x[1:5,1:5]}

install.packages("")
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

Ef.D <- read.csv("Efective Density.csv")
panel$Ef.D <- Ef.D[,2]
UURA <- read.csv("UURA.csv")
panel$UURA <- UURA[,4]
panel$UURA_AFT <- panel$UURA * panel$AFT
UURA_corv <- read.csv("UURA_corv.csv")
panel$UURA_corv <- UURA_corv[,4]
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
		"Accessibility.to.NRT", "TRE.HND", "TRE.NRT", "did.HND", "did.NRT"))] 
head(panel.imp)
str(panel.imp)
test.mcar <- LittleMCAR(panel.imp)
test.mcar$missing.patterns
test.mcar$p.value
summary(test.mcar)

imp <- mice(panel.imp, m=20)
fit.ED <- with(data = imp, lm(Employment.density ~ Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station +Road.density + Tokyo23wards 
	+AFT + Ef.D))
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
#######相関行列#######
cormatrix.HND <- correlate(panel.HND[,-1:-3])
cormatrix.NRT <- correlate(panel.NRT[,-1:-3])

write.csv(cormatrix, file="correlationHND.csv")
write.csv(cormatrix, file="correlationNRT.csv")
#######相関行列#######
########多重代入法#######
################欠損値対応###############

################プロペンシティスコアマッチング###############
str(panel.HND)
str(panel.NRT)
summary(panel.HND)
summary(completed.panel)
comp <- completed.panel[complete.cases(completed.panel),]
str(comp)

logiav.PD.HND <- glm(TRE.HND ~ Area + Mobility + Land.Price
	+ Employment.density + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.NRT + AFT + did.NRT + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)

logiav.ED.HND <- glm(TRE.HND ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station
	+ Accessibility.to.HND + Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.NRT + AFT +did.NRT + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)

logiav.LP.HND <- glm(TRE.HND ~ Area + Mobility +Population.density
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.NRT + AFT +did.NRT + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)


logiav.PD.NRT <- glm(TRE.NRT ~  Area + Mobility + Land.Price 
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.HND + AFT +did.HND + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)

logiav.ED.NRT <- glm(TRE.NRT ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.HND + AFT +did.HND + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)

logiav.LP.NRT <- glm(TRE.NRT ~ Area + Mobility +Population.density
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+ TRE.HND + AFT +did.HND + Ef.D + UURA + UURA_AFT, family=binomial(link = "logit"), data=comp)

summary(logiav.PD.HND)
summary(logiav.ED.HND)
summary(logiav.LP.HND)
summary(logiav.PD.NRT)
summary(logiav.ED.NRT)
summary(logiav.LP.NRT)

step(logiav.PD.HND)
step(logiav.ED.HND)
step(logiav.LP.HND)
step(logiav.PD.NRT)
step(logiav.ED.NRT)
step(logiav.LP.NRT)

logi.PD.HND <- glm(formula = TRE.HND ~ Mobility + Employment.density + Distance.to.Tokyo.Station + 
    Disntace.to.the.nearest.JR.station + Accessibility.to.HND + 
    Accessibility.to.NRT + Road.density + Tokyo23wards + TRE.NRT + 
    did.NRT + Ef.D + UURA, family = binomial(link = "logit"), 
    data = comp)

logi.ED.HND <- glm(formula = TRE.HND ~ Mobility + Distance.to.Tokyo.Station + 
    Disntace.to.the.nearest.JR.station + Accessibility.to.HND + 
    Accessibility.to.NRT + Road.density + Tokyo23wards + TRE.NRT + 
    did.NRT + Ef.D + UURA, family = binomial(link = "logit"), 
    data = comp)

logi.LP.HND <- glm(formula = TRE.HND ~ Mobility + Employment.density + Distance.to.Tokyo.Station + 
    Disntace.to.the.nearest.JR.station + Accessibility.to.HND + 
    Accessibility.to.NRT + Road.density + Tokyo23wards + TRE.NRT + 
    did.NRT + Ef.D + UURA, family = binomial(link = "logit"), 
    data = comp)


logi.PD.NRT <- glm(formula = TRE.NRT ~ Area + Mobility + Land.Price + Distance.to.Tokyo.Station + 
    Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station + 
    Accessibility.to.HND + Accessibility.to.NRT + Road.density + 
    Tokyo23wards + AFT + did.HND + Ef.D, family = binomial(link = "logit"), 
    data = comp)

logi.ED.NRT <- glm(formula = TRE.NRT ~ Area + Mobility + Land.Price + Population.density + 
    Distance.to.Tokyo.Station + Distance.to.the.nearest.station + 
    Disntace.to.the.nearest.JR.station + Accessibility.to.HND + 
    Accessibility.to.NRT + Road.density + Tokyo23wards + did.HND + 
    Ef.D, family = binomial(link = "logit"), data = comp)

logi.LP.NRT <- glm(formula = TRE.NRT ~ Area + Mobility + Population.density + 
    Employment.density + Distance.to.Tokyo.Station + Distance.to.the.nearest.station + 
    Disntace.to.the.nearest.JR.station + Accessibility.to.HND + 
    Accessibility.to.NRT + Road.density + Tokyo23wards + did.HND + 
    Ef.D, family = binomial(link = "logit"), data = comp)


PD.HNDav <- Match(Y=comp$Population.density, Tr=(comp$TRE.HND==1), X=logiav.PD.HND$fitted)
ED.HNDav <- Match(Y=comp$Employment.density, Tr=(comp$TRE.HND==1), X=logiav.ED.HND$fitted)
LP.HNDav <- Match(Y=comp$Land.Price, Tr=(comp$TRE.HND==1), X=logiav.LP.HND$fitted)
PD.NRTav <- Match(Y=comp$Population.density, Tr=(comp$TRE.NRT==1), X=logiav.PD.NRT$fitted)
ED.NRTav <- Match(Y=comp$Employment.density, Tr=(comp$TRE.NRT==1), X=logiav.ED.NRT$fitted)
LP.NRTav <- Match(Y=comp$Land.Price, Tr=(comp$TRE.NRT==1), X=logiav.LP.NRT$fitted)

PD.HND <- Match(Y=comp$Population.density, Tr=(comp$TRE.HND==1), X=logi.PD.HND$fitted)
ED.HND <- Match(Y=comp$Employment.density, Tr=(comp$TRE.HND==1), X=logi.ED.HND$fitted)
LP.HND <- Match(Y=comp$Land.Price, Tr=(comp$TRE.HND==1), X=logi.LP.HND$fitted)
PD.NRT <- Match(Y=comp$Population.density, Tr=(comp$TRE.NRT==1), X=logi.PD.NRT$fitted)
ED.NRT <- Match(Y=comp$Employment.density, Tr=(comp$TRE.NRT==1), X=logi.ED.NRT$fitted)
LP.NRT <- Match(Y=comp$Land.Price, Tr=(comp$TRE.NRT==1), X=logi.LP.NRT$fitted)

ATE.PD.HNDav <- summary.Match(PD.HNDav)
ATE.ED.HNDav <- summary.Match(ED.HNDav)
ATE.LP.HNDav <- summary.Match(LP.HNDav)
ATE.PD.NRTav <- summary.Match(PD.NRTav)
ATE.ED.NRTav <- summary.Match(ED.NRTav)
ATE.LP.NRTav <- summary.Match(LP.NRTav)

ATE.PD.HND <- summary.Match(PD.HND)
ATE.ED.HND <- summary.Match(ED.HND)
ATE.LP.HND <- summary.Match(LP.HND)
ATE.PD.NRT <- summary.Match(PD.NRT)
ATE.ED.NRT <- summary.Match(ED.NRT)
ATE.LP.NRT <- summary.Match(LP.NRT)
################プロペンシティスコアマッチング###############
################################データクレンジング################################

################################回帰分析################################
################OLS with mobility################
#Population Density
PD.HND.OLSm <- lm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND)

PD.NRT.OLSm <- lm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA
	+ UURA_AFT + Area  + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT)

#Employment Density
ED.HND.OLSm <- lm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND)

ED.NRT.OLSm <- lm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT)

#Land Price
LP.HND.OLSm <- lm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND)

LP.NRT.OLSm <- lm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT)

OLS.HNDm <-mtable("Ln(P.D.)"=PD.HND.OLSm, "Ln(E.D.)"=ED.HND.OLSm, "Ln(L.P.)"=LP.HND.OLSm)
write.mtable(OLS.HNDm,file="OLS.HND(mobility).csv",colsep=",")

OLS.NRTm <-mtable("Ln(P.D.)"=PD.NRT.OLSm, "Ln(E.D.)"=ED.NRT.OLSm, "Ln(L.P.)"=LP.NRT.OLSm)
write.mtable(OLS.NRTm,file="OLS.NRT(mobility).csv",colsep=",")
################OLS with mobility################

################OLS with Ef.D################
#Population Density
PD.HND.OLSe <- lm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND)

PD.NRT.OLSe <- lm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT)

#Employment Density
ED.HND.OLSe <- lm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND)

ED.NRT.OLSe <- lm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT)

#Land Price
LP.HND.OLSe <- lm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND)

LP.NRT.OLSe <- lm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT)

OLS.HNDe <-mtable("Ln(P.D.)"=PD.HND.OLSe, "Ln(E.D.)"=ED.HND.OLSe, "Ln(L.P.)"=LP.HND.OLSe)
write.mtable(OLS.HNDe,file="OLS.HND(Ef.D).csv",colsep=",")

OLS.NRTe <-mtable("Ln(P.D.)"=PD.NRT.OLSe, "Ln(E.D.)"=ED.NRT.OLSe, "Ln(L.P.)"=LP.NRT.OLSe)
write.mtable(OLS.NRTe,file="OLS.NRT(Ef.D).csv",colsep=",")
################OLS with Ef.D################

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
PD.HND.OLSm_TRE <- lm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
PD.NRT.OLSm_TRE <- lm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
ED.HND.OLSm_TRE <- lm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
ED.NRT.OLSm_TRE <- lm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
LP.HND.OLSm_TRE <- lm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.HND)
LP.NRT.OLSm_TRE <- lm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Mobility, data=panel.NRT)
#SLM
SLM_PD_HNDm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_ED_HNDm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_LP_HNDm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SLM_PD_NRTm_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_ED_HNDm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_LP_HNDm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SEM_PD_NRTm_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
########OLS,SLM and SEM(mobility)#########

########OLS,SLM and SEM(Ef.D)#########
PD.HND.OLSe_TRE <- lm(log(Population.density) ~ TRE.HND + AFT + did.HND + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.HND)
PD.NRT.OLSe_TRE <- lm(log(Population.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.NRT)
ED.HND.OLSe_TRE <- lm(log(Employment.density) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.HND)
ED.NRT.OLSe_TRE <- lm(log(Employment.density) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.NRT)
LP.HND.OLSe_TRE <- lm(log(Land.Price) ~ TRE.HND + AFT + did.HND + UURA_corv 
	+ UURA_AFT + Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.HND)
LP.NRT.OLSe_TRE <- lm(log(Land.Price) ~ TRE.NRT + AFT + did.NRT + UURA_corv
	+ Area + Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station + Road.density + Tokyo23wards 
	+ Ef.D, data=panel.NRT)
#SLM
SLM_PD_HNDe_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)
SLM_ED_HNDe_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)
SLM_LP_HNDe_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)

SLM_PD_NRTe_TRE <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTe_TRE <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTe_TRE <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
#SEM
SEM_PD_HNDe_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)
SEM_ED_HNDe_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)
SEM_LP_HNDe_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA_corv + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.HND, listw=swm_HND)

SEM_PD_NRTe_TRE <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTe_TRE <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTe_TRE <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA_corv + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D, data=panel.NRT, listw=swm_NRT)
########OLS,SLM and SEM(Ef.D)#########
###############TRE DID################


###############UURA DID################
########SLM and SEM(mobility)#########

########SLM and SEM(mobility)#########
########SLM and SEM(Ef.D)#########

########SLM and SEM(Ef.D)#########
###############UURA DID################


###############TRE or UURA DID################
########SLM and SEM(mobility)#########

########SLM and SEM(mobility)#########
########SLM and SEM(Ef.D)#########

########SLM and SEM(Ef.D)#########
###############TRE or UURA DID################


###############TRE&UURA DID################
########SLM and SEM(mobility)#########

########SLM and SEM(mobility)#########
########SLM and SEM(Ef.D)#########

########SLM and SEM(Ef.D)#########
###############TRE&UURA DID################


########SLM and SEM(mobility)#########
SLM_PD_HNDm <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_ED_HNDm <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SLM_LP_HNDm <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SLM_PD_NRTm <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTm <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTm <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)

SEM_PD_HNDm <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_ED_HNDm <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)
SEM_LP_HNDm <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.HND, listw=swm_HND)

SEM_PD_NRTm <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTm <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTm <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=panel.NRT, listw=swm_NRT)
###############SLM and SEM(mobility)################
###############SLM and SEM(Ef.D)################
SLM_PD_HNDe <- lagsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)
SLM_ED_HNDe <- lagsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)
SLM_LP_HNDe <- lagsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)

SLM_PD_NRTe <- lagsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)
SLM_ED_NRTe <- lagsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)
SLM_LP_NRTe <- lagsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)

SEM_PD_HNDe <- errorsarlm(formula = log(Population.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)
SEM_ED_HNDe <- errorsarlm(formula = log(Employment.density) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)
SEM_LP_HNDe <- errorsarlm(formula = log(Land.Price) ~ TRE.HND + AFT 
	+ did.HND + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.HND, listw=swm_HND)

SEM_PD_NRTe <- errorsarlm(formula = log(Population.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)
SEM_ED_NRTe <- errorsarlm(formula = log(Employment.density) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)
SEM_LP_NRTe <- errorsarlm(formula = log(Land.Price) ~ TRE.NRT + AFT 
	+ did.NRT + UURA + UURA_AFT + Area + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Ef.D , data=panel.NRT, listw=swm_NRT)

htmlreg(list(PD.HND.OLSm, PD.HND.OLSe, SLM_PD_HNDm, SLM_PD_HNDe, SEM_PD_HNDm,
 SEM_PD_HNDe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Haneda Ln(Population density)", 
 caption.above =TRUE, "HND_PD.html")
htmlreg(list(PD.NRT.OLSm, PD.NRT.OLSe, SLM_PD_NRTm, SLM_PD_NRTe, SEM_PD_NRTm,
 SEM_PD_NRTe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Narita Ln(Population density)", 
 caption.above =TRUE, "NRT_PD.html")

htmlreg(list(ED.HND.OLSm, ED.HND.OLSe, SLM_ED_HNDm, SLM_ED_HNDe, SEM_ED_HNDm,
 SEM_ED_HNDe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Haneda Ln(Employment density)", 
 caption.above =TRUE, "HND_ED.html")
htmlreg(list(ED.NRT.OLSm, ED.NRT.OLSe, SLM_ED_NRTm, SLM_ED_NRTe, SEM_ED_NRTm,
 SEM_ED_NRTe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Narita Ln(Employment density)", 
 caption.above =TRUE, "NRT_ED.html")

htmlreg(list(LP.HND.OLSm, LP.HND.OLSe, SLM_LP_HNDm, SLM_LP_HNDe, SEM_LP_HNDm,
 SEM_LP_HNDe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Haneda Ln(Land Price)", 
 caption.above =TRUE, "HND_LP.html")
htmlreg(list(LP.NRT.OLSm, LP.NRT.OLSe, SLM_LP_NRTm, SLM_LP_NRTe, SEM_LP_NRTm,
 SEM_LP_NRTe), custom.model.names=c("OLS1", "OLS2", "SLM1", "SLM2", "SEM1", "SEM2"), 
 caption = "Estimation Results of DID Analyses for Case Narita Ln(Land Price)", 
 caption.above =TRUE, "NRT_LP.html")
###############SLM and SEM(Ef.D)################
################################回帰分析################################

write.csv(panel, file="panel.csv")

summary(model.E.HND.OLS)
summary(model.E.NRT.OLS)
HND.OLS <- summary(model.P.HND.OLS)
NRT.OLS <- summary(model.P.NRT.OLS)



coe.HND <- HND.OLS$coefficients

class(HND.OLS)
write.csv(HND.OLS, file="HND.OLS")

names(HND.OLS)

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