### クラスタ分析の例
### - Violent Crime Rates by US State

## データの読み込み ("datasets::USArrests"を用いる)
data(USArrests) # データの読み込み

## データの内容を確認
help(USArrests)     # 内容の詳細を表示
## print(USArrests)    # 全データの表示
head(USArrests,n=5) # 最初の5個を表示
## tail(USArrests,n=5) # 最後の5個を表示

## 距離の計算
d <- dist(USArrests) # 標準はユークリッド距離
class(d)             # オブジェクトクラスの確認
## print(d)             # 計算した距離行列の表示
## dist class から行列への変換
dmat <- as.matrix(d)
dmat[1:5,1:5] # 一部のみ表示
## 行列から dist class への変換 (下三角行列)
as.dist(dmat[1:5,1:5]) 

## マンハッタン距離の計算
dm <- dist(USArrests,method='manhattan')
as.matrix(dm)[1:5,1:5]

## 正規化(平均0,分散1)したユークリッド距離の計算
ds <- dist(scale(USArrests))
as.matrix(ds)[1:5,1:5]

## 最長距離法による階層的クラスタリング: 図(a)
hc <- hclust(ds) # クラスタリング
par(family="HiraMaruProN-W4") # 日本語フォントの指定
plot(hc,main="最長距離法によるデンドログラム") # デンドログラムの表示
## デンドログラムの表示変更: 図(b)
plot(hc,hang=-1,main="表示の変更")

## ウォード法による階層的クラスタリング: 図(c)
hc <- hclust(ds,method="ward.D2") # method の指定
plot(hc,hang=-1,main="ウォード法によるデンドログラム")
## デンドログラムに基づくグループ分け
rect.hclust(hc,k=10,border='red') # 10個のグループに分ける
(grp <- cutree(hc, k=10))         # 各データの属するグループ

## グループごとのデンドログラム: 図(d)
cl <- NULL # 空の配列を用意する
for(k in 1:10){
    cl <- rbind(cl, colMeans(scale(USArrests)[grp==k,,drop=F]))
}
cl         # 10グループのデータ(平均値)にまとめたもの
plot(hclust(dist(cl),method="ward.D2",
            members=table(grp))) # グループ内の個数を指定
