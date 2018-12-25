setwd("C:/Users/admin/Google ドライブ/大学院/研 + Moblity究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除
headm <-function(x){x[1:5,1:5]}

install.packages("texreg")

library(corrr)
library(spdep)
library(sf)
library(maptools)
library(memisc)
library(texreg)
library(VIM)
library(mice)
library(aylorEdPych)
################################データクレンジング###############################
ra + Mobilityw.data <- read.csv("D + MobilityIDdata.fr.csv", fileEncoding="utf-8") #データ読み込み + Mobility

data <- ra + Mobilityw.data[, !(colnames(ra + Mobilityw.data) %in% 
	c("Area.ha.", "Employment.in.2012", "Employment.in.2001", "Population.in.2010", 
		"Population.in2000", "Road.length.in.2010", "Road.length.in.2000"))]# 不要行削除

#2010年データセット抽 + Mobiity出
data2010 <- cbin + Mobilityd(data[,1:4 + Mobility],data[,seq(5,14,2)] + Mobility, data[,seq(14,23,2)],  + Mobility
	data.fram("Tokyo23wards"=c(rep(1,747),rep(0,2096)) + Mobility),data[,24:25] + Mobility, data.frame(AFT=rep(1,2843)))

colname + Mobilitys(data2010) <- c("Zone", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", "Distance.to.Tokyo.Station", 
	"Distance.to.the.nearest.station", "Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.RT", "Road.density", "Tokyo23wards","TRE.HND", "TRE.NRT", "AFT") #名統一

#2000年データセット抽 + Mobiity出
data2000 <- cin + Mobilityd(data[,1:4 + Mobility],data[,seq(6,13,2)] + Mobility, data[,seq(13,24,2)],  + Mobility
	data.frame("Tokyo23wards"=c(rep(1,747),rep(0,2096))) + Mobility, data[,24:25] + Mobility, data.frame(AFT=rep(0,2843)))
colname + Mobilitys(data2000) <- c("Zone", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", "Distance.to.Tokyo.Station", 
	"Distance.to.the.nearest.station", "Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.NRT", "Road.density", "Tokyo23wards", "TRE.HND", "TRE.NRT", "AFT") #名統一

################距離から推定した旅行時間を用いたアクセシビリティのデータ##############
airport <- read.csv("accessbility_to_airport.csv" + Mobiity)
data2010$Accessibility.to.HND <- airportACC_to_HND201 + Mobility0
data2010$Accessibility.to.NRT <- airport$CC_to_NRT201 + Mobility0
data2010$TRE.HND <- airport$TRT_HN + MobilityD
data2010$TRE.NRT <- airport$TRT_NRT + Mobity

data2000$Accessibility.to.HND <- airportACC_to_HND200 + Mobility0
data2000$Accessibility.to.NRT <- airport$ACC_to_NRT200 + Mobility0
data2000$TRE.HND <- airport$TRT_HN + MobilityD
data2000$TRE.NRT <-airport$TR_NRT
################距離から推定した旅行時間を用いたアクセシビリティのデータ###############
panel <- rbin + Mobilityd(data2010 + Mobility, data2000)
panel$AFT <- as.integer(panel$AFT)
panel$Tokyo23wards <- as.integer(panel$Tokyo23wards)
panel$did.HND <- panel$TRE.HND * panel$AFT
panel$did.NRT <- panel$TRE.NRT * panel$AFT

Ef.D <- read.csv("Efective Density.csv")
panel$Ef.D <- Ef.D[,2]
UURA <- read.csv("UURA.csv")
panel$UURA <- UURA[,4]
panel$UURA_AFT <- panel$UURA * panel$AFT
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
summary(test.mar)

imp <- mice(panel.imp, m=20)
fit.ED <- wit + Mobilityh(data = imp, lm(Employment.density ~ Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station 
	+ Disntace.to.the.nearest.JR.station +Road.density + Tokyo23wards 
	+AFT + Ef.D + UURA + UURA_AFT))
summary(pool(ft.D))

complete + Mobilityd.dta <- complete(imp)
head(complete  Mobilityd.data)
summary(complete + Mobilityd.data)
complete + Mobilityd.data[c(179:181,3022:3024,2545,5388),]
summary(panel[,-1:3])

completed.panel <- anel
completed.panel$Land.Price <- complete + Mobilityd.dta$Land.Price
completed.panel$Population.density <- complete + Mobilityd.data$Population.density
completed.panel$Employment.density <- complete + Mobilityd.data$Employment.density
summary(completed.panel)

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
################################データクレンジング################################

################回帰分析################################################
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

###############SLM and SEM(mobility)################

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


###########この間の処理をttではなく，t2000とt2010に適応する
PD.HND2010 <- subset(panel.PD.HND, AFT==1)
PD.HND2000 <- subset(panel.PD.HND, AFT==0)
ED.HND2010 <- subset(panel.ED.HND, AFT==1)
ED.HND2000 <- subset(panel.ED.HND, AFT==0)
LP.HND2010 <- subset(panel.LP.HND, AFT==1)
LP.HND2000 <- subset(panel.LP.HND, AFT==0)

PD.NRT2010 <- subset(panel.PD.NRT, AFT==1)
PD.NRT2000 <- subset(panel.PD.NRT, AFT==0)
ED.NRT2010 <- subset(panel.ED.NRT, AFT==1)
ED.NRT2000 <- subset(panel.ED.NRT, AFT==0)
LP.NRT2010 <- subset(panel.LP.NRT, AFT==1)
LP.NRT2000 <- subset(panel.LP.NRT, AFT==0)

PD.HND.NA2010 <- PD.HND2010[!complete.cases(PD.HND2010),]
PD.HND.NA2000 <- PD.HND2000[!complete.cases(PD.HND2000),]
ED.HND.NA2010 <- ED.HND2010[!complete.cases(ED.HND2010),]
ED.HND.NA2000 <- ED.HND2000[!complete.cases(ED.HND2000),]
LP.HND.NA2010 <- LP.HND2010[!complete.cases(LP.HND2010),]
LP.HND.NA2000 <- LP.HND2000[!complete.cases(LP.HND2000),]

PD.NRT.NA2010 <- PD.NRT2010[!complete.cases(PD.NRT2010),]
PD.NRT.NA2000 <- PD.NRT2000[!complete.cases(PD.NRT2000),]
ED.NRT.NA2010 <- ED.NRT2010[!complete.cases(ED.NRT2010),]
ED.NRT.NA2000 <- ED.NRT2000[!complete.cases(ED.NRT2000),]
LP.NRT.NA2010 <- LP.NRT2010[!complete.cases(LP.NRT2010),]
LP.NRT.NA2000 <- LP.NRT2000[!complete.cases(LP.NRT2000),]

W.PD.HND2010 <- t_inv2010[-PD.HND.NA2010[,1], -PD.HND.NA2010[,1]]
W.PD.HND2000 <- t_inv2000[-PD.HND.NA2000[,1], -PD.HND.NA2000[,1]]
W.ED.HND2010 <- t_inv2010[-ED.HND.NA2010[,1], -ED.HND.NA2010[,1]]
W.ED.HND2000 <- t_inv2000[-ED.HND.NA2000[,1], -ED.HND.NA2000[,1]]
W.LP.HND2010 <- t_inv2010[-LP.HND.NA2010[,1], -LP.HND.NA2010[,1]]
W.LP.HND2000 <- t_inv2000[-LP.HND.NA2000[,1], -LP.HND.NA2000[,1]]

W.PD.NRT2010 <- t_inv2010[-PD.NRT.NA2010[,1], -PD.NRT.NA2010[,1]]
W.PD.NRT2000 <- t_inv2000[-PD.NRT.NA2000[,1], -PD.NRT.NA2000[,1]]
W.ED.NRT2010 <- t_inv2010[-ED.NRT.NA2010[,1], -ED.NRT.NA2010[,1]]
W.ED.NRT2000 <- t_inv2000[-ED.NRT.NA2000[,1], -ED.NRT.NA2000[,1]]
W.LP.NRT2010 <- t_inv2010[-LP.NRT.NA2010[,1], -LP.NRT.NA2010[,1]]
W.LP.NRT2000 <- t_inv2000[-LP.NRT.NA2000[,1], -LP.NRT.NA2000[,1]]

zm.PD.HND2010 <- matrix(0, nrow = nrow(W.PD.HND2010), ncol = ncol(W.PD.HND2000))
zm.PD.HND2000 <- matrix(0, nrow = nrow(W.PD.HND2000), ncol = ncol(W.PD.HND2010))
zm.ED.HND2010 <- matrix(0, nrow = nrow(W.ED.HND2010), ncol = ncol(W.ED.HND2000))
zm.ED.HND2000 <- matrix(0, nrow = nrow(W.ED.HND2000), ncol = ncol(W.ED.HND2010))
zm.LP.HND2010 <- matrix(0, nrow = nrow(W.LP.HND2010), ncol = ncol(W.LP.HND2000))
zm.LP.HND2000 <- matrix(0, nrow = nrow(W.LP.HND2000), ncol = ncol(W.LP.HND2010))

zm.PD.NRT2010 <- matrix(0, nrow = nrow(W.PD.NRT2010), ncol = ncol(W.PD.NRT2000))
zm.PD.NRT2000 <- matrix(0, nrow = nrow(W.PD.NRT2000), ncol = ncol(W.PD.NRT2010))
zm.ED.NRT2010 <- matrix(0, nrow = nrow(W.ED.NRT2010), ncol = ncol(W.ED.NRT2000))
zm.ED.NRT2000 <- matrix(0, nrow = nrow(W.ED.NRT2000), ncol = ncol(W.ED.NRT2010))
zm.LP.NRT2010 <- matrix(0, nrow = nrow(W.LP.NRT2010), ncol = ncol(W.LP.NRT2000))
zm.LP.NRT2000 <- matrix(0, nrow = nrow(W.LP.NRT2000), ncol = ncol(W.LP.NRT2010))

mat.PD.HND2010 <- cbind(W.PD.HND2010, zm.PD.HND2010)
mat.PD.HND2000 <- cbind(zm.PD.HND2000, W.PD.HND2000)
W.PD.HND <- rbind(mat.PD.HND2010, mat.PD.HND2000)

mat.ED.HND2010 <- cbind(W.ED.HND2010, zm.ED.HND2010)
mat.ED.HND2000 <- cbind(zm.ED.HND2000, W.ED.HND2000)
W.ED.HND <- rbind(mat.ED.HND2010, mat.ED.HND2000)

mat.LP.HND2010 <- cbind(W.LP.HND2010, zm.LP.HND2010)
mat.LP.HND2000 <- cbind(zm.LP.HND2000, W.LP.HND2000)
W.LP.HND <- rbind(mat.LP.HND2010, mat.LP.HND2000)

mat.PD.NRT2010 <- cbind(W.PD.NRT2010, zm.PD.NRT2010)
mat.PD.NRT2000 <- cbind(zm.PD.NRT2000, W.PD.NRT2000)
W.PD.NRT <- rbind(mat.PD.NRT2010, mat.PD.NRT2000)

mat.ED.NRT2010 <- cbind(W.ED.NRT2010, zm.ED.NRT2010)
mat.ED.NRT2000 <- cbind(zm.ED.NRT2000, W.ED.NRT2000)
W.ED.NRT <- rbind(mat.ED.NRT2010, mat.ED.NRT2000)

mat.LP.NRT2010 <- cbind(W.LP.NRT2010, zm.LP.NRT2010)
mat.LP.NRT2000 <- cbind(zm.LP.NRT2000, W.LP.NRT2000)
W.LP.NRT <- rbind(mat.LP.NRT2010, mat.LP.NRT2000)

anyNA(W.PD.HND)
row.names <- c(1:nrow(W.PD.HND))
swm.PD.HND <- mat2listw(W.PD.HND,row.names=row.names, style="W")
swm.ED.HND <- mat2listw(W.ED.HND, style="W")
swm.LP.HND <- mat2listw(W.LP.HND, style="W")

swm.PD.NRT <- mat2listw(W.PD.NRT, style="W")
swm.ED.NRT <- mat2listw(W.ED.NRT, style="W")
swm.LP.NRT <- mat2listw(W.LP.NRT, style="W")
str(PD.HND)
SLM <- lagsarlm(formula = log(Populatio.density) ~ TRE.HND + AFT + did.HND + Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=PD.HND, listw=swm.PD.HND)
SLM.ED.HND <- lagsarlm(formula = log(Emloyment.density) ~ TRE.HND + AFT + did.HND + Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=ED.HND, listw=swm.ED.HND)

SLM.LP.HND <- lagsarlm(formula = log(Lad.Price) ~ TRE.HND + AFT + did.HND + Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=LP.HND, listw=swm.LP.HND)

SLM.PD.NRT <- lagsarlm(formula = log(Poulation.density) ~ TRE.NRT + AFT + did.NRT + Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=PD.NRT, listw=swm.PD.NRT)

SLM.ED.NRT <- lagsarlm(formula = log(Emloyment.density) ~ TRE.NRT + AFT + did.NRT + Area + Mobility
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=ED.NRT, listw=swm.ED.NRT)

SLM.LP.NRT <- lagsarlm(formula = log(Lad.Price) ~ TRE.NRT + AFT + did.NRT + Area + Mobility 
	+ Distance.to.Tokyo.Station + Distance.to.the.nearest.station + Disntace.to.the.nearest.JR.station 
	+ Road.density + Tokyo23wards + Mobility, data=LP.NRT, listw=swm.LP.NRT)

summary(SLM)
summary(SLM.ED.HND)
summary(SLM.LP.HND)
summary(SLM.PD.NRT)
summary(SLM.ED.NRT)
summary(SLM.LP.NRT)

SLM.HND <-mtable("Ln(P.D.)"=SLM,"Ln(E.D.)"=SLM.ED.HND,"Ln(L.P.)"=SLM.LP.HND)
write.mtable(OLS.HND,file="SLM.HND.csv",colsep=",")

SLM.NRT <-mtable("Ln(P.D.)"=SLM.PD.NRT,"Ln(E.D.)"=SLM.ED.NRT,"Ln(L.P.)"=SLM.LP.NRT)
write.mtable(OLS.NRT,file="SLM.NRT.csv",colsep=",")
###########この間の処理をttではなく，t2000とt2010に適応する，重複するから

################################Spatial################################


################回帰分析################################################

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
TRT.HND2010 <- subset + Mobility(data2010, TRT.HND ==1)
CTR.HND2010 <- subset + Mobliity(data2010, TRT.HND ==0)
TRT.HND2000 <- subset + Mobility(data2000, TRT.HND ==1)
CTR.HND2000 <- subset + Mobility(data2000, TRT.HND ==0)

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
TRT.NRT2010 <- subse + Moblityt(data2010, TRT.NRT==1)
CTR.NRT2010 <- subse + Moblityt(data2010, TRT.NRT==0)
TRT.NRT2000 <- subse + Mobilityt(data2000, TRT.NRT==1)
CTR.NRT2000 <- subse + Mobilityt(data2000, TRT.NRT==0)

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

name < + Mobility- data.frame(Case=case, Variable=var, Group=group)

NPT <- cbind(name,NPT.w) 

write.csv(NPT, file="Non-Parametric Test.csv", row.names=FALSE)
################ノンパラメトリック分析################