### 第1講 資料

#' 単回帰の例
#' 表の作成
bb_data <-
  Animals |>
  rownames_to_column() |>
  as_tibble() |>
  set_names("種","体重[kg]","脳の重さ[g]")
bb_tbl <-
  bb_data |>
  gt() |>
  tab_header(
    title = "体重と脳の重さ"
  )

bb_tbl

bb_tbl |>
  tab_options(table.font.size = 11,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(50),
              latex.tbl.pos = "h") |>
  as_latex()

bb_p <-
  bb_data |>
  ggplot(aes(`体重[kg]`, `脳の重さ[g]`)) +
  geom_point(colour = alpha("royalblue", 0.75)) + 
  geom_text_repel(aes(label = `種`), # 各点の名前を追加
                  size = 3)
print(bb_p) # グラフを表示

bb_p <-
  bb_p +
  scale_x_log10() + scale_y_log10() # log-log plot を指定
print(bb_p) # グラフを表示

bb_p <-
  bb_p + # 回帰式を追加
  geom_smooth(method = "lm", 
              colour = "dodgerblue",
              fill = "dodgerblue")
print(bb_p) # グラフを表示

#' 重回帰の例
#' 表の作成
bw_data <-
  read_csv(file="data/wine.csv") |>
  set_names("ヴィンテージ","価格(対数比)","冬の雨","平均気温","秋の雨","経過年数")
bw_tbl <-
  bw_data |>
  gt() |>
  tab_header(
    title = "ワイン価格と気候特性"
  )

bw_tbl

bw_tbl |>
  tab_options(table.font.size = 10,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(60),
              latex.tbl.pos = "h") |>
  as_latex()

bw_data |> 
  ggpairs(columns = 2:6,
          upper = list(continuous = wrap("cor", size = 3.5)),
          lower = list(continuous = wrap("smooth_loess", colour = "blue"))) +
  theme(axis.text = element_text(size = 6), # 文字の大きさを調整
        strip.text = element_text(size = 8)) 

bw_fit <- lm(`価格(対数比)` ~ . - `ヴィンテージ`, # VINTを除く全て
             data = bw_data)
#' 推定結果を表にまとめるためのパッケージ gtsummary を利用
bw_fit |>
  tbl_regression(estimate_fun = label_style_sigfig(digits = 4)) |>
  add_glance_source_note(include = c(r.squared,adj.r.squared,statistic,p.value))

bw_fit |>
  tbl_regression(estimate_fun = label_style_sigfig(digits = 4)) |>
  add_glance_source_note(include = c(r.squared,adj.r.squared,statistic,p.value)) |>
  as_gt() |>
  tab_options(table.font.size = 10,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(50),
              latex.tbl.pos = "h") |>
  as_latex() 

bw_fit |>
  augment() |>
  ggplot(aes(x = `価格(対数比)`, y = .fitted, label = `ヴィンテージ`)) +
  geom_text(na.rm = TRUE) + # text で表示
  geom_abline(slope = 1, colour = "darkmagenta") +
  labs(y = "予測値")

#' 主成分分析の例
#' 表の作成
ja_data <- bind_cols(
  read_csv(file = "data/prefecture.csv",
           col_select = c(2,4)) |>
  set_names("県名", "地方名"), # 列の名称を"県名"と"地方名"に変更
  read_csv(file = "data/jpamenity.csv",
           col_select = !1:2) |> slice(-1) |>
  set_names(names(read_csv(file = "data/jpamenityitem.csv")))) # 簡略化した項目名に変更

ja_data |>
  gt() |>
  tab_header(
    title = "生活環境データ"
  )

ja_data |>
  select(1:11) |> # 大きな表なので一部を選択する(列:select，行:slice)
  gt() |>
  tab_header(
    title = "生活環境データ"
  ) |>
  tab_options(table.font.size = 9,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(100),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 6.5
ja_data |>
  ggpairs(columns = 3:10,
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 2.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方名))) +
  theme(axis.text = element_text(size = 5), # 文字の大きさを調整
        strip.text = element_text(size = 6)) 

#| fig-width: 6.5
ja_data |>
  ggpairs(columns = 11:19,
          legend = c(2,1), 
          upper = list(continuous = wrap("cor", size = 2.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方名))) +
  theme(axis.text = element_text(size = 5), # 文字の大きさを調整
        strip.text = element_text(size = 6)) 

#| fig-width: 6.5
ja_data |>
  ggpairs(columns = 20:27,
          legend = c(2,1), 
          upper = list(continuous = wrap("cor", size = 2.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方名))) +
  theme(axis.text = element_text(size = 5), # 文字の大きさを調整
        strip.text = element_text(size = 6)) 

#| fig-width: 6.5
ja_fit <- ja_data |> 
  column_to_rownames(var = "県名") |>
  select(where(is.double)) |>
  prcomp(scale. = TRUE) # 主成分分析の実行
autoplot(ja_fit, # バイプロット
         asp = 1, # 縦横比を設定
         data = ja_data, colour = "地方名", # 地方ごとに色付け
         label = TRUE, # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = jp_font, label.size = 2,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 2.5) 

#' 判別分析の例
#' 表の作成
bio_data <- biopsy |>
  as_tibble() |>
  na.omit() |>   # NA を除く
  select(-ID) |> # IDを除く
  set_names(paste0("検査",1:9),"種別") 
bio_tbl <-
  bio_data |>
  slice(1:15) |>
  gt() |>
  tab_header(
    title = "生体組織採取検査"
  )

bio_tbl

bio_tbl |>
  tab_options(table.font.size = 10,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(60),
              latex.tbl.pos = "h") |>
  as_latex()

bio_data |>
  ggpairs(diag = list(mapping = aes(colour = `種別`)),
          upper = list(continuous = wrap("cor", size = 2)),
          lower = list(mapping = aes(colour = `種別`))) +
  theme(axis.text = element_text(size = 6), # 文字の大きさを調整
        strip.text = element_text(size = 8)) 

autoplot(bio_data |> select(where(is.numeric)) |> prcomp(), # 主成分分析
         data = bio_data,
         colour = "種別") +
  theme(legend.position=c(.85,.88)) 

bio_fit <- lda(`種別` ~ ., data = bio_data)
bio_data |>
  mutate(x = predict(bio_fit)$x) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(fill = `種別`), show.legend = FALSE) +
  facet_grid(vars(`種別`))

#' クラスタ分析の例
#' 表の作成
om_data <- bind_cols( 
  read_csv(file = "data/omusubi.csv"), 
  read_csv(file = "data/prefecture.csv")) |> # 県名・地方名の情報を付加
  select(jp,ume:etc) |>
  set_names(c("県名","梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他"))
om_tbl <-
  om_data |>
  gt() |>
  tab_header(
    title = "好きなおむすびの具"
  )

om_tbl

om_tbl |>
  tab_options(table.font.size = 9,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(60),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 6
om_data |>
  pivot_longer(-県名) |>
  mutate(県名 = fct_rev(as_factor(県名)),
         name = as_factor(name)) |> 
  ggplot(aes(y = 県名, x = value)) +
  geom_bar(aes(fill = name),
           stat = "identity",
           position = position_stack(reverse=TRUE)) +
  labs(x = "人気比率", fill = "具材") +
  theme(axis.text.y = element_text(size = 8))

#| fig-width: 6
om_agnes <- 
  om_data |> 
  column_to_rownames(var = "県名") |> 
  sqrt() |> 
  agnes()
om_agnes |> as.dendrogram() |>
  ggdendrogram(rotate = TRUE, theme_dendro = FALSE) +
  labs(x = "県名", y = "距離") +
  theme(axis.text.y = element_text(size = 8))

#' 時系列解析の例
#' 表の作成
ap_tsbl <-
  AirPassengers |>
  as_tsibble() # 時系列向きの形式に変換
ap_gt <-
  ap_tsbl |> 
  mutate(Year = year(index),
         Month = month(index, label = TRUE)) |>
  as_tibble() |> select(!index) |> # 
  pivot_wider(names_from = Month)
ap_tbl <-
  ap_gt |>
  gt() |>
  tab_header(title = "月ごとの米国航空機旅客数(×1000人)",
             subtitle = "1949-1960")

ap_tbl

ap_tbl |> 
  tab_options(table.font.size = 10,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(80),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 6.5
ap_tsbl |>
  autoplot(value) +
  labs(x = "年", y = "旅客数(×1000人)")

#| fig-width: 6.5
ap_train <- ap_tsbl |> filter_index(~ "1958-12") # 訓練データ
ap_test  <- ap_tsbl |> filter_index("1959-01" ~ .) # 試験データ(2年分)
ap_train |> 
  gg_tsdisplay(difference(log(value)),
               plot_type = "partial") 

#| fig-width: 6.5
ap_fit <-
  ap_train |>
  model( # 名前に特殊な文字を含む場合は``で括る
    `arima(0,1,2)(0,1,1)` = ARIMA(log(value) ~ pdq(0,1,2) + PDQ(0,1,1)),
    `arima d=1 D=1` = ARIMA(log(value) ~ pdq(d = 1) + PDQ(D = 1)),
    `arima auto` = ARIMA(log(value)),
    `ets` = ETS(log(value)),
    )
ap_fit |> select(`arima auto`) |>
  forecast(h = nrow(ap_test)) |>
  autoplot(ap_train) +
  autolayer(ap_test, value, colour = "purple") +
  theme(legend.position=c(.9,.15)) +
  labs(x = "年", y = "旅客数(×1000人)")
