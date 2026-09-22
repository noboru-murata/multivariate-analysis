### 多次元尺度構成法の例
### - 人工データによる計量的・非計量的方法の比較

## パッケージの読み込み
require(MASS)
require(lattice)

## 3次元データの作成 (格子状に作成してサンプルする)
## 3次元空間内に2次元の曲面を埋め込む
s <- seq(0, 1.5*pi, length=1000)
mydata <- do.call('rbind',
                  lapply(s, function(t){
                      data.frame(
                          X=seq(sin(t), -sin(t), length=100), 
                          Y=seq(cos(t), -cos(t), length=100), 
                          Z=t)}
                      )
                  )
mydata$class <- rep(1:8, each=100*1000/8)
mydata <- mydata[sample(nrow(mydata),400),]

## 人工データの視覚化: 図(a-d)
for (i in seq(20,80,by=20)) { 
    print(cloud(Z ~ X + Y, data=mydata, groups=class,
                screen=list(z=i, x=-80),distance=.6))
}

## ユークリッド距離の計算
dst <- dist(mydata[,1:3])

## 計量的方法: 図(e)
plot(cmdscale(dst),col=rainbow(8)[mydata$class],
     xlab="axis 1", ylab="axis 2",
     main="Torgerson's MDS")
## 曲面のねじれのため一部の構造がつぶれている

## 非計量的方法: 図(f)
plot(isoMDS(dst)$points,col=rainbow(8)[mydata$class],
     xlab="axis 1", ylab="axis 2",
     main="Kruskal's MDS")
## 曲面を平面上に引き伸ばした尺度が構成されている
