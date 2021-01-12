### 主成分分析の例
### - Lawyers' Ratings of State Judges in the US Superior Court

## パッケージの読み込み
require(tidyverse) 
require(reshape2) 
require(ggfortify)
require(GGally)

## データの読み込み ("datasets::USJudgeRatings"を用いる)
data(USJudgeRatings) # データの読み込み

## データの内容を確認
help(USJudgeRatings)  # 内容の詳細を表示
## print(USJudgeRatings) # 全データの表示
head(USJudgeRatings)  # 最初の6個を表示
tail(USJudgeRatings)  # 最後の6個を表示

## データの散布図 (一部項目のみ): 図(a)
ggpairs(USJudgeRatings, columns=1:6) +
    ggtitle("Ratings of US Judges")

## 各データの視覚化 (radar chart)
jdgs <- sapply(rownames(USJudgeRatings), # 姓だけ取り出す
               function(x){unlist(strsplit(x,","))[1]})
mydata <- mutate(USJudgeRatings, jdgs) %>% 
    head(12) %>% melt(id.vars="jdgs") # 最初の12名のみ表示
## 各データの表示 (一部データのみ): 図(b)
ggplot(mydata, aes(x=variable, y=value, group=jdgs)) + 
    geom_polygon(fill="lightgreen") + coord_polar() +
    facet_wrap(~jdgs)

## 主成分分析
model <- prcomp(~ ., data=USJudgeRatings)

## 結果の評価
summary(model) # 寄与率の表示
round(model$rotation,3) # 主成分方向の表示(3桁)

## 寄与率: 図(c)
plot(model, col="lightblue",
     main="Proportion of Variance") # 寄与率
## biplotによる表示
## 主成分得点 (第1,2主成分): 図(d)
autoplot(model, data=USJudgeRatings, colour="gray", 
         label=TRUE, label.size=3, label.colour="darkgreen",          
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.size=5,
         loadings.label.colour="blue",
         main="PCA of US Judge Ratings")
## 主成分得点 (第2,3主成分): 図(e)
autoplot(model, x=2, y=3, data=USJudgeRatings, colour="gray", 
         label=TRUE, label.size=3, label.colour="darkgreen",          
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.size=5,
         loadings.label.colour="blue",
         main="PCA of US Judge Ratings")

## 主成分得点の散布図: 図(f)
ggpairs(model$x, columns=1:6) +
    ggtitle("Scatterplot of PCA result")
## (無相関になっていることを確認)
