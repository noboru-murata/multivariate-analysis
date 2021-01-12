## パッケージの読み込み
require(MASS)

## データの読み込み
files <- list.files(path="data/artv")
mylist <- list()
for(fn in files){ # data 内にある全ファイルを読み込む
    mylist[[fn]] <-
        read.csv(file=paste("data/artv",fn,sep="/"),
                 na.string="-")
}
## データは以下のサイトのものを利用した．(2019/09/30)
##   http://artv.info

## データの集計のための関数
myfun <- function(df){ # 1行ごとに処理
    tmp <- data.frame(t(apply(df,1,mysubfun)))
    rownames(tmp) <- df[,3]
    return(tmp)
}
mysubfun <- function(x){ 
    tmp <- min(which(names(x)=="平均"),
               which(is.na(x))) # データの末端
    return(as.numeric(x[c(4:6, # 初回から3回
                          floor((4+tmp)/2)+(-1:1), # 真ん中3回
                          tmp-(3:1))])) # 最終回までの3回
}
## 長さを揃えるため，初回から3回，真ん中3回，最終回前の3回を取得
## 番組によっては重複している回があるが，これらを比較して距離を定義する
## 全データを整理
mydata <- lapply(mylist,myfun)
names(mydata) <- NULL # リストの名前を削除
mydata <- do.call(rbind,mydata)

## 距離の計算，平均からの変動をマンハッタン距離で比較
dst <- dist(mydata-rowMeans(mydata),method="manhattan")

## 別の距離の例
## 前の回との差分(増減)に着目する場合
## dst <- dist(t(apply(mydata,1,diff)),method="manhattan")

## 平均視聴率が高い番組のみ使う場合
## dst <- dist((mydata-rowMeans(mydata))[rowMeans(mydata)>8,],
##             method="manhattan")

## 非計量的MDSを利用
mds <- isoMDS(dst)

## 全体図の表示: 図()
par(family="HiraMaruProN-W4") 
plot(mds$points,
     xlab="axis 1", ylab="axis 2", type="n",
     main="全体図")
text(mds$points,labels=rownames(mydata),
     col=ifelse(rowMeans(mydata)>8,"red","blue"))

## 中心部の表示: 図()
plot(mds$points, xlim=c(-5,5),ylim=c(-5,5),
     xlab="axis 1", ylab="axis 2", type="n",
     main="中心部")
text(mds$points,labels=rownames(mydata),
     col=ifelse(rowMeans(mydata)>8,"red","blue"))

