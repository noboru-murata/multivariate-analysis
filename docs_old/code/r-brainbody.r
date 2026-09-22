### 線形回帰分析(単回帰)の例
### - Brain and Body Weights for 28 Species

## パッケージの読み込み
require(MASS) 
require(tidyverse) 
require(ggfortify)

## データの読み込み ("MASS::Animals"を用いる)
data(Animals)

## データの内容を確認
help(Animals)  # 内容の詳細を表示 
print(Animals) # データの表示

## データのプロット (normal plot)
ggplot(Animals, aes(body, brain, label=rownames(Animals))) +
    geom_point(colour="royalblue") + 
    labs(title="Brain and Body Weights (normal plot)",
         x="body [kg]", y="brain [g]") 

ggplot(Animals, aes(body, brain)) +
    scale_x_log10() + scale_y_log10() +
    geom_point(colour="royalblue") +
    labs(title="Brain and Body Weights (log-log plot)",
         x="body [kg]", y="brain [g]")

## 回帰分析 (単回帰)
model <- lm(log(brain) ~ log(body), data=Animals)
summary(model) # 分析結果のまとめを表示

## 区間推定
yc <- exp(predict(model, newdata=Animals, interval="confidence"))
colnames(yc) <- paste("c",colnames(yc),sep=".")
yp <- exp(predict(model, newdata=Animals, interval="prediction"))
colnames(yp) <- paste("p",colnames(yp),sep=".")
mydat <- cbind(Animals, yc, yp)

## 回帰式および信頼区間・予測区間の表示
ggplot(mydat, aes(body, brain, label=rownames(mydat))) +
    scale_x_log10() + scale_y_log10() + # log-log plot
    geom_line(aes(y=c.fit), color="dodgerblue", lwd=1.2) +
    geom_ribbon(aes(ymin=c.lwr,ymax=c.upr), fill="blue", alpha=0.2)+
    geom_ribbon(aes(ymin=p.lwr,ymax=p.upr), fill="blue", alpha=0.1)+
    geom_text(size=3) + 
    labs(title="Brain and Body Weights", x="body [kg]", y="brain [g]")

## 診断プロット
autoplot(model, colour="royalblue",
         smooth.colour="gray50", smooth.linetype="dashed",
         ad.colour="blue",
         label.size=3, label.n=5, label.colour="red")

## 外れ値を除いた回帰分析
idx <- c(6,16,26) # 外れ値のindex
model <- lm(log(brain) ~ log(body), data=Animals, subset=-idx)
summary(model)

## 区間推定
yc <- exp(predict(model, newdata=Animals, interval="confidence"))
colnames(yc) <- paste("c",colnames(yc),sep=".")
yp <- exp(predict(model, newdata=Animals, interval="prediction"))
colnames(yp) <- paste("p",colnames(yp),sep=".")
mydat <- cbind(Animals, yc, yp)

## 回帰式および信頼区間・予測区間の表示
ggplot(mydat, aes(body, brain, label=rownames(mydat))) +
    scale_x_log10() + scale_y_log10() +
    geom_line(aes(y=c.fit), color="royalblue", lwd=1.2) +
    geom_ribbon(aes(ymin=c.lwr,ymax=c.upr), fill="blue", alpha=0.2)+
    geom_ribbon(aes(ymin=p.lwr,ymax=p.upr), fill="blue", alpha=0.1)+
    geom_text(size=3) + 
    labs(title="Brain and Body Weights", x="body [kg]", y="brain [g]")

## 診断プロット
autoplot(model, colour="royalblue",
         smooth.colour="gray50", smooth.linetype="dashed",
         ad.colour="blue",
         label.size=3, label.n=5, label.colour="red")
