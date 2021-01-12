### 主成分分析の例
### - Nutritional and Marketing Information on US Cereals

## パッケージの読み込み
require(MASS)  
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み ("MASS::UScereal"を用いる)
data(UScereal) # データの読み込み

## データの内容を確認
help(UScereal)     # 内容の詳細を表示
## print(UScereal)    # 全データの表示
head(UScereal,n=5) # 最初の5個を表示
tail(UScereal,n=5) # 最後の5個を表示

## 解析に使う変数だけ取り出しデータを整理しておく
idx <- which(sapply(UScereal,is.numeric)) # 量的変数のindexを取得
idx <- idx[names(idx)!="shelf"]           # "shelf"を取り除く

## データの散布図: 図(a)
ggscatmat(UScereal, columns=idx, color="mfr", alpha=.8)
## ## ggpairsでの例
## ggpairs(UScereal, columns=idx, lower=list(mapping=aes(colour=mfr)))

## 主成分分析 (データの正規化(scale.=TRUE)による違いを見る)
model1 <- prcomp(~ ., data=dplyr::select(UScereal,idx), scale.=FALSE) 
model2 <- prcomp(~ ., data=dplyr::select(UScereal,idx), scale.=TRUE) 

## 寄与率の違いを表示
summary(model1)
summary(model2)
## 寄与率 (正規化なし): 図(b)
plot(model1, col="lightblue", main="PCA without scaling")
## 寄与率 (正規化あり): 図(c)
plot(model2, col="lightblue", main="PCA with scaling")

## biplotによる表示
## 主成分得点 (正規化なし): 図(d)
autoplot(model1, data=UScereal,
         colour="mfr", shape=19, size="calories", alpha=.2,
         label=TRUE, label.size=4,
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.size=6,
         loadings.label.colour="blue", 
         main="PCA without scaling")
## 主成分得点 (正規化あり): 図(e)
autoplot(model2, data=UScereal,
         colour="mfr", shape=19, size="calories", alpha=.2,
         label=TRUE, label.size=4,
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.size=6,
         loadings.label.colour="blue", 
         main="PCA with scaling")
## 主成分得点 (中心部拡大): 図(f)
autoplot(model2, data=UScereal, colour="mfr", shape=FALSE,
         label=TRUE, label.size=4,
         xlim=c(-.15,.15), ylim=c(-.15,.15),
         main="PCA (zoomed)")
## ## optionの例
## autoplot(model2, data=UScereal,
##          colour="mfr", shape=19, size="calories",
##          frame=TRUE, frame.type="norm", label=FALSE,
##          loadings=TRUE, loadings.colour="blue",
##          loadings.label=TRUE, loadings.label.size=5,
##          loadings.label.colour="blue", 
##          main="PCA with oval frames")
