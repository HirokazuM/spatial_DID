setwd("C:/Users/admin/Google ドライブ/大学院/研究/Data") #ディレクトリ指定
rm(list=ls())# 変数削除
headm <-function(x){x[1:5,1:5]}


#データ読み込み
raw2010 <- read.csv("Generalized cost2010.csv", fileEncoding="utf-8")
raw2000 <- read.csv("Generalized cost2000.csv", fileEncoding="utf-8")


#2844番以降のゾーンを排除
gc2010 <- subset(raw2010, raw2010$小ゾーンj < 2844 & raw2010$小ゾーンi < 2844)
gc2000 <- subset(raw2000, raw2000$小ゾーンj < 2844 & raw2000$小ゾーンi < 2844)

#列名変更
colnames(gc2010) <- c("i", "j", "gc.NE", "gc.E", "t.NE", "t.E", "ACC.NE", "ACC.E")
colnames(gc2000) <- c("i", "j", "gc.NE", "gc.E", "t.NE", "t.E", "ACC.NE", "ACC.E")

#i&J, アクセス時間抽出
data2010 <- gc2010[,c("i", "j", "ACC.NE")]
data2000 <- gc2000[,c("i", "j", "ACC.NE")]

head(data2010)

#アクセシビリティのデータをEmployment Densityと同じ行列の大きさに変換する（ベクトル→行列）
acc2010 <- gc2010$ACC.NE
acc2000 <- gc2000$ACC.NE
#0を含むデータをInfに置き換える
acc2010 <- replace(acc2010, which(acc2010==0), Inf)
acc2000 <- replace(acc2000, which(acc2000==0), Inf)

#0を含んでいないかどうかの確認（2010年のみ）
# head(acc2010)
# acc2010 <- as.data.frame(acc2010)
# str(subset(acc2010, acc2010==0))

#ベクトルを行列に変換
dim(acc2010) <- c(2842, 2843)
dim(acc2000) <- c(2842, 2843)
acc2010[1:5,1:5]
##################################################################
#DIDデータ読み込みEmployment Density抽出
raw.data <- read.csv("DIDdata.fr.csv", fileEncoding="utf-8")
EMP.raw <- raw.data[,(colnames(raw.data)
	%in% c("Employment.in.2012", "Employment.in.2001"))]

#年別にデータ分化
EMP2012 <- EMP.raw[,1]
EMP2001 <- EMP.raw[,2]

head(EMP2012)
zone_id <- c(1:2843)
L2012 <- sapply(zone_id, function(x){EMP2012[-x]})
L2001 <- sapply(zone_id, function(x){EMP2001[-x]})

#Effective Density's raw data caluculation
raw.Ef.D2010 <- L2012/acc2010
raw.Ef.D2000 <- L2001/acc2000
headm(raw.Ef.D2010)

#各ゾーンの総和を計算
Ef.D2010 <- apply(raw.Ef.D2010, 2, sum)
Ef.D2000 <- apply(raw.Ef.D2000, 2, sum)

Ef.D2010 <- as.matrix(Ef.D2010)
Ef.D2000 <- as.matrix(Ef.D2000)

Ef.D <- rbind(Ef.D2010, Ef.D2000)
head(Ef.D)

write.csv(Ef.D, file="Efective Density.csv")

##################################################################
#Employmentの繰り返しの行列作成
ED2012 <- sapply(EMP2012, function(x){rep(x, length=2842)})
ED2001 <- sapply(EMP2001, function(x){rep(x, length=2842)})

#Effective Density's raw data caluculation
raw.Ef.D2010 <- ED2012/acc2010
raw.Ef.D2000 <- ED2001/acc2000
head(raw.Ef.D2010)

#各ゾーンの総和を計算
Ef.D2010 <- apply(raw.Ef.D2010, 2, sum)
Ef.D2000 <- apply(raw.Ef.D2000, 2, sum)

Ef.D2010 <- as.matrix(Ef.D2010)
Ef.D2000 <- as.matrix(Ef.D2000)

Ef.D <- rbind(Ef.D2010, Ef.D2000)

write.csv(Ef.D, file="Efective Density.csv")
head(Ef.D)
str(Ef.D)


