### 判別分析の例
### - 企業別の利益データ (2007-2012年度)

## データの読み込み ("jpcompany.csv"を用いる)
raw <- read.csv(file="data/jpcompany.csv",row.names=1) # データの読み込み
scan(file="data/jpcompany.txt",what=character(),sep=";") # 説明の表示

## データの内容を表示
## print(raw)    # 全データの表示
head(raw,n=3) # 最初の3個を表示
## tail(raw,n=3) # 最後の3個を表示

## 判別分析のためにデータを整形
## class: 2011年から2012年に掛けて営業利益が増収(pos)/減収(neg)
## 特徴量: 売上高に占める各利益の割合
mydata <- data.frame(class=ifelse(raw[,26]-raw[,22]>0,"pos","neg"))
for (k in 1:5){
    mydata <- data.frame(mydata,raw[,(4*k+1)+(1:3)]/raw[,4*k+1])
}
names(mydata)[2:ncol(mydata)] <- # 特徴量の名前
    paste(rep(c("営業利益率","経常利益率","純利益率"),5),
          rep((1:5)+2006,each=3),sep=".")
rownames(mydata) <- raw[,2]  # 各社の名前
mydata <- na.exclude(mydata) # 欠損(NA)の除去 
head(mydata,n=3) # 最初の3個を表示

## データの一部とラベルの関係を図示
item <- c(1,5,8,11,14)
## print(names(mydata)[item]) # 項目名の表示
## データの散布図: 図(a)
ggpairs(mydata,
        columns=item, mapping=aes(colour=class)) +
    theme(text=element_text(family="HiraMaruProN-W4"))
## ## ggscatmat を用いる場合 (class以外が表示される)
## ggscatmat(mydata,
##           columns=item, color="class", alpha=.5) +
##     theme(text=element_text(family="HiraMaruProN-W4"))

## データ全体で判別式を作成
model <- lda(class ~ ., data=mydata)
table(true=mydata$class,predict=predict(model)$class)

## 線形判別関数の値の分布をクラス毎に表示
predict.tr <- predict(model) 
res.tr <- data.frame(x=as.vector(predict.tr$x), 
                    class=mydata$class,
                    d=c(neg=0,pos=1)[mydata$class]) 
## 判別関数の値の分布: 図(b)
ggplot(res.tr, aes(x)) + 
    geom_histogram(aes(fill=class)) +
    facet_grid(class ~ .) + theme(legend.position="none") 

## 全体を用いると判別があまり良好でないので
## 2011年の経常利益率で層別に調べてみる
## (業種別に調べると良いかもしれないが各業種のデータが少ない)
conditions <- c("経常利益率.2011<0.05",
                "経常利益率.2011>0.05&経常利益率.2011<0.1",
                "経常利益率.2011>0.1&経常利益率.2011<0.2",
                "経常利益率.2011>0.2")
for(i in 1:4){
    subdata <- subset(mydata, subset=eval(parse(text=conditions[i])))
    submodel <- lda(class ~ ., data=subdata)
    tbl <- table(true=subdata$class,predict=predict(submodel)$class)
    print(conditions[i])
    print(tbl)
    ## 線形判別関数の値の分布をクラス毎に表示
    predict.tr <- predict(submodel) 
    res.tr <- data.frame(x=as.vector(predict.tr$x), 
                         class=subdata$class,
                         d=c(neg=0,pos=1)[subdata$class]) 
    ## 層別の判別関数の値の分布: 図(c)-(f)
    gg <- ggplot(res.tr, aes(x)) + 
        geom_histogram(aes(fill=class)) +
        facet_grid(class ~ .) +
        theme(text=element_text(family="HiraMaruProN-W4"),
              legend.position="none") +
        labs(title=conditions[i])
    print(gg)
}
