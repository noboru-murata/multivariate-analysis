### 第2講 資料

#| fig-cap: "正規分布 (平均0,分散)"
ggplot() + 
  geom_function(fun = \(x) dnorm(x,mean = 0,sd = 1),
                colour = "red", linewidth = 2) +
  xlim(-4, 4) + ylim(0, 0.4) +
  labs(x = "x", y = "確率密度") 

#| fig-cap: "カイ2乗分布 (自由度3)"
ggplot() + 
  geom_function(fun = \(x) dchisq(x, 3, ncp = 0),
                colour = "green", linewidth = 2) +
  xlim(-1, 10) + 
  labs(x = "x", y = "確率密度") 

#| fig-cap: "t分布 (自由度3)"
ggplot() + 
  geom_function(fun = \(x) dt(x, df = 3, ncp = 0),
                colour = "blue", linewidth = 2) +
  xlim(-4, 4) + ylim(0, 0.4) +
  labs(x = "x", y = "確率密度") 

#| fig-cap: "F分布 (自由度3,5)"
ggplot() + 
  geom_function(fun = \(x) df(x, df1 = 3, df2 = 5, ncp = 0),
                colour = "purple", linewidth = 2) +
  xlim(-1, 4) + ylim(0, 0.8) +
  labs(x = "x", y = "確率密度") 

#| fig-cap: "いかさまのないコインの場合"
n <- 20
k <- 0:n
p <- dbinom(k,n,0.5)
plot(k,p,type="h",
     col="blue",lwd=5,
     ylab="probability")

#| fig-cap: "いかさまのないコインの場合"
q <- dbinom(k,n,0.6)
plot(k,q,type="h",
     col="red",lwd=5,
     ylab="probability")

#| fig-cap: "いかさまの有無による違い"
plot(k,p,type="h",
     col="blue",lwd=5,
     ylab="probability")
lines(k+.2,q,type="h",col="red",lwd=5)

#| fig-cap: "いかさまのないコインの場合"
cp <- 1-pbinom(k-1,n,0.5)
plot(k,cp,type="h",
     col="blue",lwd=5,
     ylab="type-I error rate")

#| fig-cap: "いかさまのあるコインの場合"
cq <- 1-pbinom(k-1,n,0.6)
plot(k,cq,type="h",
     col="red",lwd=5,
     ylab="power")

#| fig-cap: "いかさまの有無による違い"
plot(k,cp,type="h",
     col="blue",lwd=5,
     ylab="error rate / power")
lines(k+.2,cq,type="h",
      col="red",lwd=5)

#| fig-cap: "いかさまのあるコインの場合"
cq2 <- 1-pbinom(k-1,n,0.9)
plot(k,cq2,type="h",
     col="orange",lwd=5,
     ylab="power")

#| fig-cap: "いかさまの有無による違い"
plot(k,cp,type="h",
     col="blue",lwd=5,
     ylab="error rate / power")
lines(k+.2,cq2,type="h",
      col="orange",lwd=5)

#| fig-cap: "対立仮説による検出力の違い"
prob <- seq(0,1,by=0.02)
power <- 1-pbinom(14,n,prob)
plot(prob,power,type="l",
     col="gray",lwd=2)

#| fig-cap: "いかさまの有無による違い"
n <- 100
k <- 0:n
cp <- 1-pbinom(k-1,n,0.5)
cq <- 1-pbinom(k-1,n,0.6)
plot(k,cp,type="h",
     col="blue",lwd=3,
     ylab="probability")
lines(k+.4,cq,type="h",
      col="red",lwd=3)

#| fig-cap: "対立仮説による検出力の違い"
prob <- seq(0,1,by=0.02)
power <- 1-pbinom(60,n,prob)
plot(prob,power,type="l",
     col="gray",lwd=2)
