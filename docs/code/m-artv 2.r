## パッケージの読み込み
require(MASS)

## データの読み込み
files <- list.files(path="data")
mydat <- list()
for(fn in files){ # data 内にある全ファイルを読み込む
    mydat[[fn]] <-
        read.csv(file=paste("data",fn,sep="/"),
                 na.string="-")
}
## データは以下のサイトのものを利用した．(2016/12/19)
##   http://artv.info
## 以下から data.zip を download して展開して下さい．
## 

## データの集計のための関数
myfun <- function(df){ # 1行ごとに処理
    tmp <- data.frame(t(apply(df,1,mysubfun)))
    rownames(tmp) <- df[,3]
    return(tmp)
}
mysubfun <- function(x){ 
    tmpa <- which(names(x)=="平均")
    tmpb <- min(which(is.na(x)),tmpa)
    tmpc <- as.numeric(x[c(4:8,tmpb-(1:5))])
    return(c(tmpc,mean(tmpc)))
}
## 長さを揃えるため，初回から5回までと，最終回から5回前までを取得
## 番組によっては重複している回があるが，
## ここでは初回からの変動と，最終回前の変動を比較して
## 距離を定義することとする

## 全データを整理
tmp <- lapply(mydat,myfun)
names(tmp) <- NULL
dat <- do.call(rbind,tmp)

## 距離の計算，平均からの変動をマンハッタン距離で比較
dst <- dist((dat[,-11]-dat[,11]),method="manhattan")

## 別の距離の例
## 前の回との差分(増減)に着目する場合
## dst <- dist(t(apply(dat[,-11],1,diff)),method="manhattan")

## 平均視聴率が高い番組のみ使う場合
## dst <- dist((dat[,-11]-dat[,11])[dat[,11]>8,],method="manhattan")

## 非計量的MDSを利用
mds <- isoMDS(dst)

## 全体図の表示
par(family="HiraMaruProN-W4") 
plot(mds$points,
     xlab="axis 1", ylab="axis 2", type="n",
     main="全体図")
text(mds$points,labels=rownames(dat),
     col=ifelse(dat[,11]>8,"red","blue"))

## 中心部分のみ表示
plot(mds$points, xlim=c(-5,5),ylim=c(-5,5),
     xlab="axis 1", ylab="axis 2", type="n",
     main="中心部")
text(mds$points,labels=rownames(dat),
     col=ifelse(dat[,11]>8,"red","blue"))

