### 線形回帰分析(重回帰)の例
### - New York Air Quality Measurements

## パッケージの読み込み
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み ("datasets::airquality"を用いる)
data(airquality)   # データの読み込み
help(airquality)   # 内容の詳細を表示
str(airquality)    # データの構造を表示

## データの内容を表示
head(airquality,n=10) # 最初のnデータを表示
tail(airquality,n=10) # 最後のnデータを表示
## print(airquality) では表示が長すぎる

## データのプロット (pairs plot)
mydat <- airquality %>%
    mutate(Level=ifelse(Ozone>60,"high",ifelse(Ozone<20,"low","mid"))) 
mydensity <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
      geom_point(...,colour="gray50",size=1) +
      geom_density_2d(alpha=.8)
}
ggpairs(na.omit(mydat), columns=1:4,
        upper=list(continuous=mydensity),
        lower=list(continuous=wrap("smooth_loess"),
                   mapping=aes(colour=Level))) +
    theme(axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10)) # 文字の大きさを調整

## データの素性を確認
is.data.frame(airquality) # データフレームかどうか確認
names(airquality) # 各列の名称を調べる
dim(airquality)   # データフレームのサイズを調べる

## 回帰分析 (Ozoneを目的変数，Solar.R, Wind, Tempを説明変数)
model <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
summary(model) # 分析のまとめ

## モデルの計算結果を表示する関数群
coef(model)   # または coefficients(model): モデルの係数 
resid(model)  # または residuals(model): 各データの残差
fitted(model) # または fitted.values(model): 各データの予測値
## これ以外にも以下が用意されている
## effects(model) 
## deviance(model)
## df.residual(model)
## anova(model)
## モデルが保持している情報の要素名を調べるには
##  names(model) 
## 各要素の表示を行うには
##  model$coefficients, model$residuals, model$fitted.values
## などとすればよい

## 当て嵌り具合を表示
mydat <- airquality %>%
    mutate(Date=as.Date(paste(Month,Day,"73",sep="/"),"%m/%d/%y"),
           Pred=predict(model, newdata=airquality))
ggplot(mydat, aes(Date)) +
    geom_line(aes(y=Ozone,colour="true"), size=1) +
    geom_line(aes(y=Pred, colour="predict"), size=1) +
    scale_colour_manual(values=c(predict="blue",true="red")) + # lineの色を指定
    theme(legend.position=c(.9,.9))  # 凡例の位置を指定

## 診断プロット (いろいろと問題の多いモデルであることがわかる)
autoplot(model)

## モデルの指定方法 
## 風量(Wind) と 温度(Temp) で回帰
summary(lm(Ozone ~ Wind + Temp, data=airquality))
## 切片を0として Wind と Temp で回帰
summary(lm(Ozone ~ Wind + Temp - 1, data=airquality))
## Wind と Temp の積の効果まで入れて回帰
summary(lm(Ozone ~ Wind * Temp, data=airquality))
## Wind と Temp の積と Wind で回帰
summary(lm(Ozone ~ Wind * Temp - Temp, data=airquality))
## Wind の2次多項式で回帰
summary(lm(Ozone ~ Wind + I(Wind^2), data=airquality))
## Wind の2次直交多項式で回帰
summary(lm(Ozone ~ poly(Wind,2), data=airquality))

## 関数 update によるモデルの更新方法
## Wind のみで回帰
summary(model <- lm(Ozone ~ Wind, data=airquality))
## Solar.R を加える
summary(model <- update(model, . ~ . + Solar.R, data=airquality))
## Solar.R の積の効果を加える
summary(model <- update(model, . ~ . * Solar.R, data=airquality))
## 切片を 0 にする
summary(model <- update(model, . ~ . -1 , data=airquality))

## AICによる最適なモデルの自動探索
## 初期モデルの設定 (ここでは全ての相互作用を含むモデルを用いる)
summary(model <- lm(Ozone ~ Solar.R * Wind * Temp, data=airquality))
## AICの意味で最適なモデルを探索
summary(opt <- step(model))

## 当て嵌り具合を表示
mydat <- airquality %>%
    mutate(Date=as.Date(paste(Month,Day,"73",sep="/"),"%m/%d/%y"),
           Pred=predict(model, newdata=airquality))
ggplot(mydat, aes(Date)) +
    geom_line(aes(y=Ozone,colour="true"), size=1) +
    geom_line(aes(y=Pred, colour="predict"), size=1) +
    scale_colour_manual(values=c(predict="blue",true="red")) + # lineの色を指定
    theme(legend.position=c(.9,.9))  # 凡例の位置を指定

## 診断プロット (前モデルよりは改善されていることがわかる)
autoplot(model) 

## 回帰式による予測
## 8月までのデータで回帰式を推定
summary(model <- lm(formula(opt), # 上で探索されたモデルを利用
            data=airquality, subset=(Month<9)))
## 9月のデータを予測
idx <- with(airquality,Month==9) # 9月のデータのindexを取得
pred <- predict(model,newdata=airquality[idx,], # 9月分を予測
                interval="prediction",level=0.7)
mydat <- data.frame(airquality[idx,], pred) # 9月のデータとその予測
ggplot(mydat, aes(Day)) +
    geom_ribbon(aes(ymin=lwr,ymax=upr), fill="red", alpha=0.2)+
    geom_line(aes(y=Ozone,colour="true"), size=1) +
    geom_line(aes(y=fit, colour="predict"), size=1) +
    theme(legend.position=c(.9,.9)) + # 凡例の位置を指定
    labs(title="Ozone Level Prediction (September; 70% pred. int.)",x="Day",y="Ozone")
