### car package を用いた回帰分析の診断の例
### 詳しい解説は以下を参照
###  An R Companion to Applied Regression, Second Edition
###     John Fox and Sanford Weisberg
###        Sage Publications, 2011

## パッケージの読み込み
require(car)
require(tidyverse) 
require(ggfortify)
require(GGally)

## データの読み込み ("datasets::mtcars"を用いる)
data(mtcars)  # データの読み込み

## データの内容を確認
help(mtcars)  # 内容の詳細を表示
str(mtcars)   # データの構造を表示
## print(mtcars) # 全データの表示
head(mtcars)  # データの一部を表示

## カテゴリカルな変数を因子に変換してデータを整理
mydata <- mtcars
mydata[,c(2,8:11)] <- lapply(mydata[,c(2,8:11)],
                             factor)
with(mydata,table(cyl)) # clyカテゴリ毎のデータ数を表示
## str(mydata) # データの構造を表示

## 量的変数の散布図: 図(a)
ggscatmat(mydata, columns=c(1,3:7),
          color="cyl", alpha=.8)

## ## その他の散布図の書き方
## ggpairs(mydata) +
##     theme(axis.title.x=element_text(size=10),
##           axis.title.y=element_text(size=10)) 

## 気筒数(cyl)毎の燃費(mpg)と排気量(disp)の関係: 図(b)
coplot(mpg ~ disp | cyl, 
       data=mydata, panel=panel.smooth, rows=1)

### 線形回帰モデルの作成
### モデル例: 量的変数のみを用いたモデル
## 目的変数: 燃費(mpg)
## 説明変数: 排気量(disp)，馬力(hp)，
##         重さ(wt)，ギア比(drat)
## 馬力と重さが有意
## 排気量は不合理な結果で有意性も低い悪いモデル
model <- lm(mpg ~ disp + hp + wt + drat,
            data=mydata)
summary(model)
## 推定された係数と信頼区間: 図(c)
ggcoef(model)

## 診断プロット: 図(d)
autoplot(model)

## car packageによる診断
## *residual plots*
## モデルが正しければ残差は予測値や説明変数とは独立になるはず
## 出力は lack-of-fit test (2乗項を含むかどうかのt検定)
## 各変数と残差の関係: 図(e)
##  (drat 以外は残差に偏りが見られる)
residualPlots(model) 
## ## optionの例 (terms の与え方でいろいろな図が作成できる)
## residualPlots(model, terms= ~ 1)  # 予測値との関係のみ
## residualPlots(model, terms= ~ wt, fitted=FALSE) 
## residualPlots(model, terms= ~ 1 | cyl) # cylごと
## residualPlots(model, terms= ~ . | cyl) 
##  (残差の偏りとcylの間に何らかの関係が見られる)

## *marginal-model plots*
## residualPlotsの変種
## 残差の代わりに目的変数を縦軸に用いて
## 予測値や説明変数での条件付周辺分布を視覚化する
## モデルが正しければデータとモデルの条件付平均は一致するはず
## 各変数と目的変数の関係: 図(f)
##  (hpやwtは2つの平均が明瞭に交差している)
marginalModelPlots(model)
## # optionの例
## marginalModelPlots(model, id.n=3) # 外れ値を3つ表示
## marginalModelPlots(model, sd=TRUE) # 標準偏差を付加

## *added-variable plots*
##  偏回帰 (以下の2つの回帰)
##   y  ~ x2 + ... + xp 
##   x1 ~ x2 + ... + xp
##  の残差の散布図を用いて，説明変数と目的変数の関係を視覚化する
## 偏回帰の残差の関係: 図(g)
##  (hpは反比例(双曲線)の関係が示唆される)
avPlots(model)
## ## optionの例
## avPlots(model, id.n=2) # それぞれの軸で外れ値を2つ

## *QQ plot*
## 正規分布から逸脱する残差を調べて
## モデルに合致しないデータを洗い出す
## 残差の正規性の検討: 図(h)
qqPlot(model)
## ## optionの例
## qqPlot(model, envelope=.99) # 信頼区間を変更
## qqPlot(model, id.n=3) # 外れ値を3つ表示

## 残差
residuals(model) %>% head() # headで最初の部分のみ表示
## 標準化誤差
rstandard(model) %>% head() # standardized residual
rstudent(model) %>% head()  # Studentized residual
## テコ比
hatvalues(model) %>% head()
## Cook's dist
cooks.distance(model) %>% head()
## 最大正規化誤差のt-検定 (Bonferroniの補正)
outlierTest(model)

## *influence index plot*
## 各データの Cook's dist., Studentized residual,
## Bonferroni p-value, hat-value (テコ比)を表示
## 外れ値の診断プロット: 図(i)
influenceIndexPlot(model, id.n=3)

## *influence plot*
## Cook's dist., Studentized residual, hat-valueを表示
## 外れ値の診断プロット: 図(j)
influencePlot(model, id.n=3)

## *component+residual plots*
## 説明変数と残差+説明変数による成分(偏回帰の残差)の関係から
## 非線形性の確認を行う
## 非線形性の検討: 図(k)
crPlots(model)
## ## optionの例
## crPlots(model, order=2) # 2次の関係を仮定
## crPlots(model, order=3) # 3次の関係を仮定

## *spread-level plot*
## 予測値と残差の絶対値の関係から残差の分散の一様性の確認を行う
## 分散の一様性の検討: 図(l)
spreadLevelPlot(model)

## non-constant variance score test
## 残差の分散の一様性の検定
ncvTest(model)

## variance inflation factor
## 多重共線性の確認
vif(model)

### モデル例: 量的変数のみを用いたモデル
## 目的変数: 燃費(mpg)
## 説明変数: AICを用いたstepwise法により選択されたモデル
model <- step(lm(mpg ~ ., data=mydata), trace=0)
summary(model)
## ggcoef(model)

## 各種診断: 図(m)(n)
residualPlots(model) 
## marginalModelPlots(model, id.n=3) 
## avPlots(model, id.n=2) 
qqPlot(model)
## crPlots(model)
## spreadLevelPlot(model)
vif(model)

### モデル例: 変数を変換して作成したモデル
## 目的変数: 燃費の逆数(gpm)
## 説明変数: power-weight ratio(hp/wt)を加えAICで選択
mydata2 <- dplyr::select(mutate(mydata,
                                gpm=1/mpg,pwr=hp/wt),drat:pwr)
rownames(mydata2) <- rownames(mtcars) # 行名を付ける
model <- step(lm(gpm ~ ., data=mydata2), trace=0)
summary(model) 
## ggcoef(model)

## 各種診断: 図(o)(p)
residualPlots(model) 
## marginalModelPlots(model, id.n=3) 
## avPlots(model, id.n=2) 
qqPlot(model)
## crPlots(model)
## spreadLevelPlot(model)
vif(model)

## 主成分分析による変数間の関係の視覚化: 図(q)
autoplot(princomp(~ disp + hp + wt + drat,
                  data=mydata, cor=TRUE),
         data=mydata,
         colour="cyl", shape=FALSE, label=TRUE,
         loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=6)
## ## optionの例
## autoplot(princomp(~ disp + hp + wt + drat,
##          data=mydata, cor=TRUE),
##          data=mydata, label=FALSE,
##          colour="cyl", shape="am", size="carb", 
##          loadings=TRUE, loadings.label=TRUE,
##          loadings.label.size=6)
