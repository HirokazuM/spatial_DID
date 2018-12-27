setwd("C:/Users/admin/Google ドライブ/大学院/研究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除

################パッケージインストール################
install.packages("Matching")
library(Matching)
################パッケージインストール################


raw.data <- read.csv("DIDdata.fr.csv", fileEncoding="utf-8") #データ読み込み
head(raw.data)

data <- raw.data[, !(colnames(raw.data) %in% 
	c("Area.ha.", "Employment.in.2012", "Employment.in.2001", 
		"Population.in.2010", "Population.in.2000", "Road.length.in.2010", 
		"Road.length.in.2000"))]# 不要な行削除

#2010年データセット抽出
data2010 <- cbind(data[,1:4],data[,seq(5,14,2)], data[,seq(14,23,2)], 
	data.frame("Tokyo23wards"=c(rep(1,747),rep(0,2096))),data[,24:25], data.frame(AFT=rep(1,2843)))

colnames(data2010) <- c("Zone#", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", 
	"Distance.to.Tokyo.Station", "Distance.to.the.nearest.station", 
	"Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.NRT", "Road.density", "Tokyo23wards","TRT.HND", 
	"TRT.NRT", "AFT") #列名統一

#2000年データセット抽出
data2000 <- cbind(data[,1:4],data[,seq(6,13,2)], data[,seq(13,24,2)], 
	data.frame("Tokyo23wards"=c(rep(1,747),rep(0,2096))), data[,24:25], data.frame(AFT=rep(0,2843)))
colnames(data2000) <- c("Zone#", "Munincipalities", "Town", "Area", "Mobility", 
	"Land.Price", "Population.density", "Employment.density", 
	"Distance.to.Tokyo.Station", "Distance.to.the.nearest.station", 
	"Disntace.to.the.nearest.JR.station", "Accessibility.to.HND", 
	"Accessibility.to.NRT", "Road.density", "Tokyo23wards", "TRT.HND", 
	"TRT.NRT", "AFT") #列名統一


panel <- rbind(data2010, data2000)　#パネルデータの作成
panel$did.HND <- panel$TRT.HND * panel$AFT
panel$did.NRT <- panel$TRT.NRT * panel$AFT

panel <- panel[complete.cases(panel),] #NAを含む列消去
head(panel)

logiav.PD.HND <- glm(TRT.HND ~ Area + Mobility + Land.Price
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.NRT + AFT +did.NRT, family=binomial(link = "logit"), data=panel)

logiav.PD.NRT <- glm(TRT.NRT ~  Area + Mobility + Land.Price 
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.HND + AFT +did.HND, family=binomial(link = "logit"), data=panel)

logiav.ED.HND <- glm(TRT.HND ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.NRT + AFT +did.NRT, family=binomial(link = "logit"), data=panel)

logiav.ED.NRT <- glm(TRT.NRT ~ Area + Mobility + Land.Price +Population.density
	+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.HND + AFT +did.HND, family=binomial(link = "logit"), data=panel)

logiav.LP.HND <- glm(TRT.HND ~ Area + Mobility +Population.density
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.NRT + AFT +did.NRT, family=binomial(link = "logit"), data=panel)

logiav.LP.NRT <- glm(TRT.NRT ~ Area + Mobility +Population.density
	+ Employment.density+ Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards 
	+TRT.HND + AFT +did.HND, family=binomial(link = "logit"), data=panel)

summary(logiav.PD.HND)
summary(logiav.PD.NRT)
summary(logiav.ED.HND)
summary(logiav.ED.NRT)
summary(logiav.LP.HND)
summary(logiav.LP.NRT)

step(logiav.PD.HND)
step(logiav.PD.NRT)
step(logiav.ED.HND)
step(logiav.ED.NRT)
step(logiav.LP.HND)
step(logiav.LP.NRT)

logi.PD.HND <- glm(TRT.HND ~ Area + Mobility + Land.Price + Employment.density 
	+ Distance.to.Tokyo.Station + Accessibility.to.HND + Accessibility.to.NRT 
	+ Road.density + Tokyo23wards + TRT.NRT + did.NRT, 
	family=binomial(link = "logit"), data=panel)

logi.PD.NRT <- glm(TRT.NRT ~  Mobility + Land.Price + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards + TRT.HND + did.HND, 
	family=binomial(link = "logit"), data=panel)

logi.ED.HND <- glm(TRT.HND ~ Area + Mobility + Land.Price 
	+ Distance.to.Tokyo.Station + Accessibility.to.HND + Accessibility.to.NRT 
	+ Road.density + Tokyo23wards + TRT.NRT + did.NRT, 
	family=binomial(link = "logit"), data=panel)

logi.ED.NRT <- glm(TRT.NRT ~ Mobility + Land.Price + Distance.to.Tokyo.Station 
	+ Distance.to.the.nearest.station + Accessibility.to.HND 
	+ Accessibility.to.NRT + Road.density + Tokyo23wards + TRT.HND + did.HND, 
	family=binomial(link = "logit"), data=panel)

logi.LP.HND <- glm(TRT.HND ~ Area + Mobility + Employment.density 
	+ Distance.to.Tokyo.Station + Accessibility.to.HND + Accessibility.to.NRT 
	+ Road.density + Tokyo23wards + TRT.NRT + did.NRT, 
	family=binomial(link = "logit"), data=panel)

logi.LP.NRT <- glm(TRT.NRT ~ Area + Mobility + Employment.density 
	+ Distance.to.Tokyo.Station + Accessibility.to.HND + Accessibility.to.NRT 
	+ Road.density + Tokyo23wards + TRT.HND + did.HND, 
	family=binomial(link = "logit"), data=panel)

PD.HND <- Match(Y=panel$Population.density, Tr=(panel$TRT.HND==1), X=logiav.PD.HND$fitted)
PD.NRT <- Match(Y=panel$Population.density, Tr=(panel$TRT.NRT==1), X=logiav.PD.NRT$fitted)
ED.HND <- Match(Y=panel$Employment.density, Tr=(panel$TRT.HND==1), X=logiav.ED.HND$fitted)
ED.NRT <- Match(Y=panel$Employment.density, Tr=(panel$TRT.NRT==1), X=logiav.ED.NRT$fitted)
LP.HND <- Match(Y=panel$Land.Price, Tr=(panel$TRT.HND==1), X=logiav.LP.HND$fitted)
LP.NRT <- Match(Y=panel$Land.Price, Tr=(panel$TRT.NRT==1), X=logiav.LP.NRT$fitted)

ATE.PD.HND <- summary.Match(PD.HND)
ATE.PD.NRT <- summary.Match(PD.NRT)
ATE.ED.HND <- summary.Match(ED.HND)
ATE.ED.NRT <- summary.Match(ED.NRT)
ATE.LP.HND <- summary.Match(LP.HND)
ATE.LP.NRT <- summary.Match(LP.NRT)