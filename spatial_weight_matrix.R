setwd("C:/Users/admin/Google ドライブ/大学院/研究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除

library(spdep)
library(sp)
library(splus2R)
library(maptools)
library(dplyr)
headm <-function(x){x[1:5,1:5]}

################################################################
raw2010 <- read.csv("Generalized cost2010.csv", fileEncoding="utf-8")
raw2000 <- read.csv("Generalized cost2000.csv", fileEncoding="utf-8")

tt2010 <- subset(raw2010, raw2010$小ゾーンj < 2844 & raw2010$小ゾーンi < 2844)
tt2000 <- subset(raw2000, raw2000$小ゾーンj < 2844 & raw2000$小ゾーンi < 2844)

head(tt2000)

t2010 <- tt2010[,7]
t2000 <- tt2000[,7]
t2010[t2010==0] <- NA
t2000[t2000==0] <- NA

head(t2000)
summary(t2010)
summary(t2000)

################ゾーン間の距離データ###############
Tokyo <- readShapePoly("Tokyo.shp")
plot(Tokyo)
coords <- coordinates(Tokyo)
head(coords)
dmatrix <- spDists(coords, longlat=TRUE)
head(dmatrix)

dim(dmatrix) <- c(8082649, 1)
distance.num <- as.numeric(dmatrix)
head(distance.num)
d_w_0 <- distance.num[distance.num!=0]

distance.data <-as.data.frame(d_w_0)
colnames(distance.data) <- c("distance")
distance.data <- subset(distance.data, distance!=0)
head(distance.data)
################ゾーン間の距離データ###############
################距離から旅行時間を推定###############
dt2010 <- cbind(t2010, distance.data)
dt2000 <- cbind(t2000, distance.data)

head(dt2010)

test <- dt2010[!complete.cases(dt2010),]
head(test)


fit.lm2010 <- lm(t2010 ~ distance, data=dt2010, na.action = na.omit)
fit.lm2000 <- lm(t2000 ~ distance, data=dt2000, na.action = na.omit)

pred2010 <- predict(fit.lm2010, subset(dt2010, is.na(t2010)))
pred2000 <- predict(fit.lm2000, subset(dt2000, is.na(t2000)))
head(pred2010)
summary(t2010)

t2010[is.na(t2010)] <- pred2010
t2000[is.na(t2000)] <- pred2000

summary(t2010)
cor.test(dt2010$t2010, dt2010$distance)
cor.test(dt2000$t2000, dt2010$distance)
str(t2010)
################距離から旅行時間を推定###############
################旅行時間に基づくSWM作成###############
t_inv2010 <- 1/t2010
t_inv2000 <- 1/t2000
for (i in 1:2843){
	t_inv2010 <- append(t_inv2010, 0, after = 2844*(i-1))
}

for (i in 1:2843){
	t_inv2000 <- append(t_inv2000, 0, after = 2844*(i-1))
}

t_inv2010 <- as.matrix(t_inv2010)
t_inv2000 <- as.matrix(t_inv2000)

head(t_inv2000)

class(t_inv2000)
#ベクトルを行列に変換
dim(t_inv2010) <- c(2843, 2843)
dim(t_inv2000) <- c(2843, 2843)
write.csv(t_inv2010, "SWM_traveltime2010.csv")
write.csv(t_inv2000, "SWM_traveltime2000.csv")
################旅行時間に基づくSWM作成###############

################推定した旅行時間データを抽出###############
tt2010$travel_time <- t2010
tt2000$travel_time <- t2000
head(tt2010)
summary(tt2010)
summary(tt2000)
################推定した旅行時間データを抽出###############
HND2010 <- subset(tt2010, 小ゾーンi!="179" & 小ゾーンi!="180" & 小ゾーンi!="181")
HND2000 <- subset(tt2000, 小ゾーンi!="179" & 小ゾーンi!="180" & 小ゾーンi!="181")

HND2010a <- subset(HND2010, 小ゾーンj=="179" |小ゾーンj=="180" | 小ゾーンj=="181")
HND2000a <- subset(HND2000, 小ゾーンj=="179" |小ゾーンj=="180" | 小ゾーンj=="181")

part1HND2010 <- HND2010a[1:534,]
part2HND2010 <- HND2010a[535:8520,]

part1HND2000 <- HND2000a[1:534,]
part2HND2000 <- HND2000a[535:8520,]

part1HND2010av <- as.matrix(part1HND2010[,9]) 
part2HND2010av <- as.matrix(part2HND2010[,9])
part1HND2000av <- as.matrix(part1HND2000[,9]) 
part2HND2000av <- as.matrix(part2HND2000[,9])


dim(part1HND2010av) <- c(3, 178)
dim(part2HND2010av) <- c(3,2662)

dim(part1HND2000av) <- c(3, 178)
dim(part2HND2000av) <- c(3, 2662)

part1HND2010_mean <- as.matrix(apply(part1HND2010av, 2, mean))
part2HND2010_mean <- as.matrix(apply(part2HND2010av, 2, mean))
tail(part2HND2010_mean)
tail(part2HND2000_mean)
part1HND2000_mean <- as.matrix(apply(part1HND2000av, 2, mean))
part2HND2000_mean <- as.matrix(apply(part2HND2000av, 2, mean))

HND2010_mean <- bind_rows(part1HND2010_mean, matrix(1000, 3,1), part2HND2010_mean)

HND2000_mean <- rbind(part1HND2000_mean, matrix(0, 3,1), part2HND2000_mean)



NRT2010 <- subset(tt2010, 小ゾーンj=="2545")
NRT2000 <- subset(tt2000, 小ゾーンj=="2545")
tt_NRT2010 <- as.matrix(NRT2010[,7])
tt_NRT2000 <- as.matrix(NRT2000[,7])

tt_NRT2010 <- append(tt_NRT2010, 1000, after = 2544)
tt_NRT2000 <- append(tt_NRT2000, 0, after = 2544)

TRT_HND <- sapply(HND2010_mean - HND2000_mean, function(x){if (x<0){return(1)} else {return(0)}})
TRT_NRT <- sapply(tt_NRT2010 - tt_NRT2000, function(x){if (x<0){return(1)} else {return(0)}})


head(HND2010_mean)
head(HND2000_mean)


TRT_HND[179:181] <-NA
TRT_NRT[2545] <- NA
HND2010_mean[179:181] <- NA
HND2000_mean[179:181] <- NA
tt_NRT2010[2545] <- NA 
tt_NRT2000[2545] <- NA

ActA <- data.frame(ACC_to_HND2010=HND2010_mean, ACC_to_HND2000=HND2000_mean, 
	ACC_to_NRT2010=tt_NRT2010, ACC_to_NRT2000=tt_NRT2000, 
	dif_HND=(HND2010_mean - HND2000_mean), dif_NRT=(tt_NRT2010 - tt_NRT2000),
	TRT_HND=TRT_HND, TRT_NRT=TRT_NRT)

head(ActA)
summary(ActA)
write.csv(ActA, "accessbility_to_airport.csv")