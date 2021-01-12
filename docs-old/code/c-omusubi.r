### クラスタ分析の例
### - おむすびに関するアンケートデータ

## パッケージの読み込み
require(cluster)
require(tidyverse) 
require(reshape2) 
require(ggfortify)
require(GGally)
require(ggdendro)

## データの読み込み("omusubi.csv"を用いる)
raw <- read.csv(file="data/omusubi.csv",row.names=1)   # データの読み込み
scan(file="data/omusubi.txt",what=character(),sep=";") # データの説明の表示

## データの内容を表示
## print(raw)    # 全データの表示
head(raw,n=4) # 最初の4県を表示
tail(raw,n=4) # 最後の4県を表示

## 1つだと処理が難しいので北海道は東北に含める
areaname <- c("tohoku","kanto","chubu", 
              "kinki","chugoku","chikoku","kyushu")
area <- rep(areaname,c(7,7,9,7,5,4,8))
## データの散布図: 図(a)
ggpairs(data.frame(raw,area),
        columns=1:ncol(raw), mapping=aes(colour=area)) +
    labs(title="おむすびの具 県別人気アンケート (2009)") +
    theme(text=element_text(family="HiraMaruProN-W4"))

## 県別の人気比率: 図(b)
mydata <- mutate(raw,perfecture=rownames(raw)) %>%
    melt(id.vars="perfecture")
ggplot(mydata,aes(x=perfecture,y=value,fill=variable)) +
    geom_bar(stat="identity",position="fill") + coord_flip() +
    scale_x_discrete(limits=rev(rownames(raw))) +
    labs(title="おむすびの具 県別人気アンケート (2009)",
         x="県名",y="人気比率") +
    theme(legend.position="top",
          text=element_text(family="HiraMaruProN-W4"))

## 距離計算
dst <- daisy(sqrt(raw)) # Hellinger距離
## その他の距離の計算方法の例
## dst <- dist(raw) # ユークリッド距離
## dst <- daisy(raw) # ユークリッド距離 (cluster package の距離)
## dst <- daisy(raw,stand=TRUE) # ユークリッド距離
## dst <- as.dist(acos(cor(t(raw)))) # 別の距離

## 階層的クラスタリング: 図(c)
hclst <- agnes(dst) # dianaという関数もある
ggdendrogram(as.dendrogram(hclst),
             rotate=TRUE, theme_dendro=FALSE) +
    labs(title="おむすびの具人気アンケートによるクラスタ分析",
         x="県名",y="距離") +
    theme(text=element_text(family="HiraMaruProN-W4"))
## ggdendrogram(as.dendrogram(hclst), rotate=TRUE) # 白地
## バナープロット: 図(d)
plot(hclst, which.plot=1) # banner plot
## plot(hclst, which.plot=2) # dendrogram

## 非階層的クラスタリング (クラスタ数を指定すること)
nhclst <- pam(raw,k=3)
## nhclst <- pam(dst,k=3) # 距離行列を使うこともできる
## nhclst <- clara(rawt,k=3) # claraはデータフレームを使う
## 非階層的クラスタリングによる散布図: 図(e)
autoplot(nhclst, frame=TRUE) 
## plot(nhclst, which.plot=1) # 散布図
## シルエットプロット: 図(f)
plot(nhclst, which.plot=2) # silhouette plot
## ## 他の方法でも同様な表示は可能
## autoplot(pam(raw,3), frame=TRUE, frame.type='norm')
## autoplot(clara(raw,3), frame=TRUE, frame.type="norm")
## autoplot(fanny(raw,3), frame=TRUE, frame.type='norm')
