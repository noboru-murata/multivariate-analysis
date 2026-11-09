### 第6講 資料

#' 主成分分析の例
#' 人口関連データの整理
ja_data <-
  bind_cols(
    ## 都道府県名と地方名の取得
    read_csv(file = "data/prefecture.csv",
             col_select = c(2,4)) |>
    set_names("都道府県名", "地方区分"),     # 列の名称を"都道府県名"と"地方区分"に変更
    ## データの取得
    read_csv(file = "data/jpamenity.csv",
             col_select = !1:2) |> # 不要な列を読み込まない
    slice(-1) |>                   # 不要な行を削除
    set_names(names(read_csv(file = "data/jpamenityitem.csv"))) # 簡略化した項目名に変更
  ) |> 
  select(1:10) |> # 人口関連のみ利用
  mutate(地方区分 = as_factor(地方区分)) # 地方区分を出現順に因子化

ja_data |> View() # 左上ペインに表として表示

#| fig-width: 6.2
ja_data |>
  ggpairs(columns = 3:10,  # 都道府県名・地方区分を除く
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 2.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分))) +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 5))

#| fig-width: 7
ja_pca <- ja_data |> 
  select(!1:2) |> # 人口関連データのみ
  prcomp(scale. = TRUE) # 主成分分析の実行
autoplot(ja_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = ja_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE, # ラベルの表示
         label.label = "都道府県名",
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.show.legend = FALSE,
         label.family = jp_font, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 4)

#' データの読み込み
js_data <- read_csv(file = "data/japan_social.csv",
                    col_type = list(地方区分 = col_factor()))
## js_data <-
##   bind_cols(
##     read_csv(file = "data/prefecture.csv",
##              col_select = c(2,4)) |>
##     set_names("県名","地方区分"), # 列の名称を"県名"と"地方区分"に変更
##     read_csv(file = "data/japan_social.csv",
##              col_select = 2:6) |> 
##     set_names("森林面積割合","農業産出額","人口割合","土地生産性","商品販売額")
##   )|> # 簡略化した項目名に変更
##   mutate(地方区分 = as_factor(地方区分)) # 地方区分を出現順に因子化

js_data |> View() # 左上ペインに表として表示

#| fig-width: 6
#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  ggpairs(columns = 3:7, # 都道府県名・地方区分は除く
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分))) +
  theme(strip.text = element_text(size = 9),
        axis.text = element_text(size = 7))

#| fig-width: 6
js_data |> # 箱ひげ図．
  pivot_longer(where(is.double)) |>  # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>  # 出現順にx軸を並べるために因子化する
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), alpha = 0.5,
               show.legend = FALSE) +# 変数ごとに色を変える
  labs(x = NULL, y = NULL)

#| fig-width: 6
js_data |> 
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = name), alpha = 0.4) + # 変数ごとに色を変える
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(.95, .95),
        legend.justification = c("right", "top"))

#' データの標準化
js_data_std <-
  js_data |>
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3)))

js_data_std |> View() # 左上ペインに表として表示

#| fig-width: 6
js_data |> # 箱ひげ図．変数のばらつきをそろえる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), alpha = 0.5,
               show.legend = FALSE) + # 変数ごとに色を変える
  labs(x = NULL, y = NULL)

#| fig-width: 6
js_data |> 
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = name), alpha = 0.4) + # 変数ごとに色を変える
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(.95, .95),
        legend.justification = c("right", "top"))

js_pca <-
  js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = TRUE) # 変数のばらつきを規格化

js_pca |>
  tidy("loadings") |>
  pivot_wider(names_from = "PC", names_prefix = "PC") |>
  View() # 左上ペインに表として表示

#| fig-width: 6
js_pca |>
  tidy("loadings") |>
  mutate(column = as_factor(column)) |>
  ggplot(aes(x = PC, y = value, fill = column)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5,
           position = position_dodge(width = 0.7, reverse = TRUE)) +
  scale_x_reverse() + coord_flip() +
  labs(x = "主成分", y = "負荷量", fill = NULL)

#| fig-width: 7
autoplot(js_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = js_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE,
         label.label = "都道府県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.size = 3,
         label.show.legend = FALSE,
         loadings = FALSE,
         loadings.colour = "orchid", # 負荷の表示
         loadings.label = FALSE,
         loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.size = 4) 

js_pca <- js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = FALSE) # 標準化しない

js_pca |>
  tidy("loadings") |>
  pivot_wider(names_from = "PC", names_prefix = "PC") |>
  View() # 左上ペインに表として表示

#| fig-width: 6
js_pca |>
  tidy("loadings") |>
  mutate(column = as_factor(column)) |>
  ggplot(aes(x = PC, y = value, fill = column)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5,
           position = position_dodge(width = 0.7, reverse = TRUE)) +
  scale_x_reverse() + coord_flip() +
  labs(x = "主成分", y = "負荷量", fill = NULL)

#| fig-width: 7
autoplot(js_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = js_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE,
         label.label = "都道府県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.size = 3,
         label.show.legend = FALSE,
         loadings = FALSE,
         loadings.colour = "orchid", # 負荷の表示
         loadings.label = FALSE,
         loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.size = 4)
