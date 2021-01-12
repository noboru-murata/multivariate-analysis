### 線形判別分析(2値判別)の例
### - Biopsy Data on Breast Cancer Patients

## データの読み込み ("MASS::biopsy"を用いる)
require(MASS) # パッケージの読み込み (lda/biopsy)
require(tidyverse) 
require(ggfortify)
require(GGally)
require(plotROC)

## データの内容を表示
help(biopsy)  # 内容の詳細を表示
## print(biopsy) # データの表示
head(biopsy)  # 最初の6個を表示
tail(biopsy)  # 最後の6個を表示

## データの散布図: 図(a)
mydata <- na.omit(biopsy)[-1] # NA，および患者のIDを除く
ggpairs(mydata, lower=list(mapping=aes(colour=class)))
## ## ggscatmat でも似たことはできるが数値データの散布図のみ
## ggscatmat(mydata, color="class", alpha=.8) # colour ではない

## 主成分分析による2次元表示: 図(b)
autoplot(prcomp(mydata[-10]), data=mydata, colour="class") +
    theme(legend.position=c(.9,.9)) 

## 判別分析 (ランダムに選んだ300個のサンプルで分析)
## set.seed(1234) # 実験の再現性を求める場合
idx <- sample(nrow(mydata),300)
train <- mydata[idx,] # 学習に用いるデータを選択 
model <- lda(class ~ .,data=train) 
print(model) # モデルの概要を表示

## 線形判別関数の値の分布をクラス毎に表示
predict.tr <- predict(model) # 学習データの予測
res.tr <- data.frame(x=as.vector(predict.tr$x), # 学習データの評価用
                    class=train$class,
                    d=c(benign=0,malignant=1)[train$class]) 
## 判別関数の値の分布 (学習データ): 図(c)
ggplot(res.tr, aes(x)) + xlim(-3,8) + # 値の範囲を指定
    geom_histogram(aes(fill=class)) +
    facet_grid(class ~ .) + theme(legend.position="none") 

## AUC (学習データ): 図(d)
gg <- ggplot(res.tr, aes(m=x, d=d)) + geom_roc(colour="red")
gg + annotate("text", x=.9, y=.1,
              label=paste("AUC =", round(calc_auc(gg)$AUC,3)))

## 上記の結果を用いて残りのサンプルを予測
test <- mydata[-idx,] # 予測対象のデータを選択 
predict.te <- predict(model,newdata=test[,-10])
table(true=test$class,est=predict.te$class) # 判別結果を表示

## 線形判別関数の値の分布をクラス毎に表示
res.te <- data.frame(x=as.vector(predict.te$x), # 試験データの評価用
                    class=test$class,
                    d=c(benign=0,malignant=1)[test$class]) 
## 判別関数の値の分布 (試験データ): 図(e)
ggplot(res.te, aes(x)) + xlim(-3,8) + # 値の範囲を指定
    geom_histogram(aes(fill=class)) +
    facet_grid(class ~ .) + theme(legend.position="none") 

## AUC (試験データ): 図(f)
gg <- ggplot(res.te, aes(m=x, d=d)) + geom_roc(colour="red")
gg + annotate("text", x=.9, y=.1,
              label=paste("AUC =", round(calc_auc(gg)$AUC,3)))
