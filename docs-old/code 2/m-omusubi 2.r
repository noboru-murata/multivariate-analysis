### 多次元尺度構成法の例
### - おむすびに関するアンケートデータ

## パッケージの読み込み
require(MASS) 

## データの読み込み("omusubi.csv"を用いる)
raw <- read.csv(file="data/omusubi.csv",row.names=1)   # データの読み込み
scan(file="data/omusubi.txt",what=character(),sep=";") # データの説明の表示

## データの内容を表示
## print(raw)    # 全データの表示
head(raw,n=4) # 最初の4県を表示
tail(raw,n=4) # 最後の4県を表示

## 非計量的多次元尺度構成法: 図(a)
dst <- dist(sqrt(raw)) # Hellinger距離
mds <- isoMDS(dst)     # 2次元のユークリッド座標を構成
## cmdscaleとは異なり，座標はpointsに，評価値はstressに収められている
plot(mds$points,type='n',xlab="axis 1",ylab="axis 2")
text(mds$points,labels=rownames(mds$points),cex=.8)

## Shepard ダイアグラム: 図(b)
shprd <- Shepard(dst, mds$points)
plot(shprd, pch = 20, col="blue",
     xlab="Observed Dissimilarity", ylab="Estimated Distance")
lines(shprd$x, shprd$yf, type = "S", col="red", lwd=2)

## マンハッタン距離による尺度構成: 図(c)
mds <- isoMDS(dst, p=1)
plot(mds$points,type='n',main="p=1",xlab="axis 1",ylab="axis 2")
text(mds$points,labels=rownames(mds$points),cex=.8)      

## 近似的な最大距離による尺度構成: 図(d)
mds <- isoMDS(dst, p=10)
plot(mds$points,type='n',main="p=10",xlab="axis 1",ylab="axis 2")
text(mds$points,labels=rownames(mds$points),cex=.8)      

## 3次元尺度構成: 図(e)
mds <- isoMDS(dst, k=3)
opar <- par(mfrow=c(2,2),mar=c(4,4,1,1))
plot(mds$points[,c(1,2)],type='n',
     xlab="axis 1",ylab="axis 2", xlim=c(-2.5,2.5),asp=1)
text(mds$points[,c(1,2)],labels=rownames(mds$points),cex=.5)      
plot.new()
plot(mds$points[,c(1,3)],type='n',
     xlab="axis 1",ylab="axis 3", xlim=c(-2.5,2.5),asp=1)
text(mds$points[,c(1,3)],labels=rownames(mds$points),cex=.5)      
plot(mds$points[,c(2,3)],type='n',
     xlab="axis 2",ylab="axis 3", xlim=c(-2.5,2.5),asp=1)
text(mds$points[,c(2,3)],labels=rownames(mds$points),cex=.5)      
par(opar)

## 3次元尺度のShepard ダイアグラム: 図(f)
shprd <- Shepard(dst, mds$points)
plot(shprd, pch = 20, col="blue",
     xlab="Observed Dissimilarity", ylab="Estimated Distance")
lines(shprd$x, shprd$yf, type = "S", col="red", lwd=2)
