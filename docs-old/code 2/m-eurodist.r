### 多次元尺度構成法の例
### - Distances Between European Cities

## データの読み込み ("datasets::eudodist"を用いる)
data(eurodist)    # データセットの読み込み
help(eurodist)    # データセットの詳細を表示
class(eurodist)   # クラスの確認

## 多次元尺度(2次元)の構成
mds <- cmdscale(eurodist,k=2)

## 構成された尺度によるデータの表示: 図(a)
plot(mds,type='n',asp=1,xlab="",ylab="") # 点は打たずに座標軸だけ用意
text(mds,labels=rownames(mds),cex=.8)    # 点の位置に都市名を表示
abline(h=pretty(range(mds[,1]),10),      # ガイドラインを表示
       v=pretty(range(mds[,2]),10), col="lightgray")

## 実際の配置に合わせて南北を逆転: 図(b)
mds2 <- mds * rep(c(1,-1),each=nrow(mds)) 
plot(mds2,type='n',asp=1,xlab="",ylab="")
text(mds2,labels=rownames(mds2),cex=.8)
abline(h=pretty(range(mds2[,1]),10),
       v=pretty(range(mds2[,2]),10), col="lightgray")

## 3次元尺度構成 (第1,2軸): 図(c)
mds3 <- cmdscale(eurodist,k=3)  # 3次元への埋め込み
plot(mds3,type='n',asp=1,xlab="axis 1",ylab="axis 2")
text(mds3,labels=rownames(mds),cex=.8)
abline(h=pretty(range(mds3[,1]),10),
       v=pretty(range(mds3[,2]),10), col="lightgray")

## 3次元尺度構成 (第2,3軸): 図(d)
plot(mds3[,2:3],type='n',asp=1,xlab="axis 2",ylab="axis 3") 
text(mds3[,2:3],labels=rownames(mds),cex=.8)
abline(h=pretty(range(mds3[,2]),10), # 縦横比の関係で第2軸を使用
       v=pretty(range(mds3[,2]),10), col="lightgray")

## 不安定な高次元への埋め込みの例
head(cmdscale(eurodist, k=20),n=3) # 指定した次元までの座標が計算できない
## 高次元への埋め込みの安定化
mds4 <- cmdscale(eurodist, k=20, add=TRUE) # 対角成分に定数を付加
head(mds4$points,n=3) # 推定された座標を表示
