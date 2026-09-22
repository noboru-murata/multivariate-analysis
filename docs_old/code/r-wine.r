### 線形回帰分析の例
### - Ashenfelter's Wine Equation

## パッケージの読み込み
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み("wine.csv"を用いる)
scan(file="data/wine.txt", what=character(), sep=";") # データの説明の表示
wine <- read.csv(file="data/wine.csv", row.names=1)  # データの読み込み

## データの内容を確認
print(wine) # データの表示

## データのプロット
ggpairs(wine,
        lower=list(continuous=wrap("smooth_loess",colour="blue"))) +
    theme(axis.title.x=element_text(size=8),
          axis.title.y=element_text(size=8)) # 文字の大きさを調整

## ボルドーワインの価格(質)を冬の降雨量(WRAIN)，育成期平均気温(DEGREES)，
## 収穫期降雨量(HRAIN)，年数(TIME_SV)で回帰する
model <- lm(LPRICE2 ~ ., data=wine)
summary(model)
round(coef(model), digits=6) # 係数を確認する

## 診断プロット
autoplot(model)
## autoplot(model, which=1) # res vs fit
## autoplot(model, which=2) # qq plot
## autoplot(model, which=4) # Cook's dist
## autoplot(model, which=1:6)

## 回帰による予測結果を比較
year <- rownames(wine)
price <- exp(wine$LPRICE2)
predict <- exp(predict(model, newdata=wine, interval="prediction"))
mydat <- data.frame(year=year, price=price, predict)

## 実測値と予測値の比較
ggplot(mydat, aes(price, fit, label=year)) +
    geom_text(na.rm=TRUE) + # text で表示
    scale_x_log10() + scale_y_log10() + # log-log plot
    labs(title="Bordeaux Wine Price (log)", x="Price", y="Prediction")

## 各年の比較
ggplot(mydat, aes(year, price, group=1)) +
    geom_ribbon(aes(ymin=lwr, ymax=upr),
                fill="red", alpha=.1, na.rm=TRUE) +
    geom_path(aes(y=price, colour="Price"), na.rm=TRUE) + 
    geom_path(aes(y=fit, colour="Predict"), na.rm=TRUE) +
    theme(axis.text.x=element_text(angle = 90, hjust=1, vjust=.5),
          legend.position=c(.9,.9)) + # 文字の向きと凡例の位置を調整
    labs(title="Bordeaux Wine Price", x="Year") 

### 専門家の評価が高かったのは1986年だが，予測では凡庸
### その後1989/1990年の気温(かなり高い)を用いて予測した内容が話題となる
