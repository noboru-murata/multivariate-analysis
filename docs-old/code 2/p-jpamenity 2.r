### 主成分分析の例
### - 県別の生活環境に関するデータ

## パッケージの読み込み
require(MASS)  
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み ("jpamenity.csv"を用いる)
raw <- read.csv(file="data/jpamenity.csv") # データの読み込み
scan(file="data/jpamenity.txt",what=character(),sep=";") # 説明の表示

## データの整形
mydata <- raw[-1,-c(1,2)] # 不要な行・列を削除
names(mydata) <-  names(read.csv("data/jpamenityitem.csv")) # 変数名の略記
rownames(mydata) <- raw[-1,1] # 各行の名前を県名
areaname <- c("北海道","東北","関東","中部","近畿","中国","四国","九州")
area <- rep(areaname,c(1,6,7,9,7,5,4,8))

## データの内容を表示
## print(mydata) # 全データの表示
head(mydata)  # 最初の6個を表示
## tail(mydata)  # 最後の6個を表示

## データの散布図 (一部項目のみ): 図(a)
item <- c(1,7,8,18,19,20)
## print(names(mydata)[item])
ggscatmat(data.frame(mydata,area),
          columns=item, color="area", alpha=.5) +
    theme(text=element_text(family="HiraMaruProN-W4"))
## ## ggparis を用いる場合 (legendが付かない)
## ggpairs(data.frame(mydata,area),
##         columns=item, mapping=aes(colour=area)) +
##     theme(text=element_text(family="HiraMaruProN-W4"))

## 主成分分析
model <-princomp(mydata,cor=TRUE)
## model <-prcomp(mydata,scale.=TRUE) # prcompを使う場合

## 分析結果を表示
print(model) 
## 寄与率 (正規化あり): 図(b)
plot(model)

## 主成分得点 (scale=1) [既定値]: 図(c)
autoplot(model, data=mydata, shape=FALSE,
         label=TRUE, label.family="HiraMaruProN-W4", label.size=3, 
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.family="HiraMaruProN-W4",
         loadings.label.size=4, loadings.label.colour="blue",
         main="県別の生活環境") +
    theme(text=element_text(family="HiraMaruProN-W4"))

## 中心部の拡大表示: 図(d)
autoplot(model, data=mydata, shape=FALSE,
         xlim=c(-.3,.3), ylim=c(-.3,.3),
         label=TRUE, label.family="HiraMaruProN-W4", label.size=3, 
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.family="HiraMaruProN-W4",
         loadings.label.size=4, loadings.label.colour="blue",
         main="県別の生活環境 (中心を拡大)") +
    theme(text=element_text(family="HiraMaruProN-W4"))

## 主成分得点 (scale=0): 図(e)
autoplot(model, data=mydata, scale=0, shape=FALSE,
         label=TRUE, label.family="HiraMaruProN-W4", label.size=3, 
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.family="HiraMaruProN-W4",
         loadings.label.size=4, loadings.label.colour="blue",
         main="scale=0での表示") +
    theme(text=element_text(family="HiraMaruProN-W4"))

## 主成分得点 (scale=1/2): 図(f)
autoplot(model, data=mydata, scale=1/2, shape=FALSE,
         label=TRUE, label.family="HiraMaruProN-W4", label.size=3, 
         loadings=TRUE, loadings.colour="blue",
         loadings.label=TRUE, loadings.label.family="HiraMaruProN-W4",
         loadings.label.size=4, loadings.label.colour="blue",
         main="scale=1/2での表示") +
    theme(text=element_text(family="HiraMaruProN-W4"))
