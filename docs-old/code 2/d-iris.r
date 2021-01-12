### 多値判別分析の例
### - Edgar Anderson's Iris Data

## パッケージの読み込み (lda/qda)
require(MASS)  
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み ("datasets::iris"を用いる)
data(iris) # データセットの読み込み

## データの内容を確認
help(iris)  # 内容の詳細を表示
str(iris)   # データの構造を表示
## print(iris) # 全データの表示
head(iris)  # データの最初を表示
tail(iris)  # データの最後を表示

## データの散布図: 図(a)
ggpairs(iris, columns=1:4, mapping=aes(colour=Species, alpha=.5)) +
    labs(title="Edgar Anderson's Iris Data")

## 3D表示 (Sepal.Widthを除く): 図(b)
require(lattice)
cloud(Sepal.Length ~ Petal.Length * Petal.Width,
      data=iris,　groups=Species, screen=list(z=30, x=-60),
      main="Edgar Anderson's Iris Data")
## cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species,
##       data=iris, screen=list(x=-90, y=70), distance=.4, zoom=.6)

## 特徴量とカテゴリによる線形判別関数の構成
model <- lda(Species ~ ., data=iris)
print(model) # 結果を表示

## 判別得点の散布図: 図(c)
### 参考 https://www.r-bloggers.com/computing-and-visualizing-lda-in-r/
mydata <- data.frame(iris,
                     lda=predict(model, newdata=iris)$x)
prop <- model$svd^2/sum(model$svd^2) # 判別の寄与率の計算
ggplot(mydata) +
    geom_point(aes(lda.LD1,lda.LD2,colour=Species)) +
    labs(x=paste0("LD1 (", round(prop[1]*100,2),"%)"),
         y=paste0("LD1 (", round(prop[2]*100,2),"%)")) +
    theme(legend.position="top")

## 主成分得点の散布図: 図(d)
autoplot(prcomp(~ . -Species, data=iris, scale.=TRUE),
         data=iris, colour="Species") + 
    theme(legend.position="top")

## 訓練データと試験データによる線形判別の評価
## set.seed(1234) # 実験の再現性を求める場合
idx <- sample(1:150,75) # 訓練用データの番号をランダムに選ぶ
with(iris,table(Species[idx])) # 各種が何個ずつ選ばれたか表示

## 線形判別式の作成
model1 <- lda(Species ~ ., data=iris, subset=idx,
               prior=c(1/3,1/3,1/3)) 
## print(model1) # 結果を表示
## ## データの分布をそのまま使う場合はpriorを指定しない
## model1 <- lda(Species ~ ., data=iris, subset=idx) 
## ## モデルの更新を行う場合はupdateを使う (例: Petal.Length を除く)
## model1 <- update(model1, . ~ . - Petal.Length) 

## 線形判別による予測
true <- iris[-idx,5]
predict1 <- predict(model1, newdata=iris[-idx,-5]) 
table(true,predict=predict1$class)  # 真のクラスラベルと予測結果の比較
if(length(true!=predict1$class)>0) {# 誤ったデータがある場合
    which(true!=predict1$class) # 番号を表示
    predict1$posterior[true!=predict1$class,] # 事後確率を表示
}
## 線形判別の事後確率: 図(e)
cloud(setosa ~ versicolor * virginica, 
      data=data.frame(predict1$posterior),
      groups=true)

## 訓練データと試験データによる2次判別の評価
## 2次判別式の作成
model2 <- qda(Species ~ ., data=iris, subset=idx,
               prior=c(1/3,1/3,1/3)) 
## print(model2) # 結果を表示

## 2次判別による予測
predict2 <- predict(model2, newdata=iris[-idx,-5]) 
table(true,predict=predict2$class)  # 真のクラスラベルと予測結果の比較
if(length(true!=predict2$class)>0) {# 誤ったデータがある場合
    which(true!=predict2$class) # 番号を表示
    predict2$posterior[true!=predict2$class,] # 事後確率を表示
}
## 2次判別の事後確率: 図(f)
cloud(setosa ~ versicolor * virginica, # 事後確率を図示
      data=data.frame(predict2$posterior),
      groups=true)
