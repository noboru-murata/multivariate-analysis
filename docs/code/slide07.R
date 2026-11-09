### 第7講 資料

#| include: false
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
  theme(strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

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

#| include: false
js_pca <- js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = FALSE) # 標準化しない

#| fig-width: 6
js_pca |>
  tidy("loadings") |>
  mutate(column = as_factor(column)) |>
  ggplot(aes(x = PC, y = value, fill = column)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5,
           position = position_dodge(width = 0.7, reverse = TRUE)) +
  scale_x_reverse() + coord_flip() +
  labs(x = "主成分", y = "負荷量", fill = NULL)

#| include: false
#' 寄与率に関する表を作成
js_pcs_tab <- 
  js_pca |>                              
  tidy("eigenvalues") |> # "d" または "eigenvalues" または "pcs"
  rename(主成分 = PC,
         標準偏差 = std.dev,
         寄与率 = percent,
         累積寄与率 = cumulative)

#' 寄与率に関する表を作成
js_pcs_tab |>                              
  View() # 左上ペインに表として表示

#| fig-width: 7
#' まとめて並べて表示
js_pca |>                              
  tidy("eigenvalues") |>
  pivot_longer(!PC) |>
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = PC, y = value, fill = name)) +
  geom_bar(stat="identity") +
  facet_wrap(~ name,
             scales = "free_y",
             labeller = labeller(name = c(
                                   std.dev = "標準偏差",
                                   percent = "寄与率",
                                   cumulative = "累積寄与率"))) +
  labs(x = "主成分") +
  theme(legend.position = "none") 

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

#| fig-width: 7
autoplot(js_pca, # バイプロット
         x = 3, y = 2,
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

#| include: false
#' データの標準化
js_data_std <-
  js_data |>
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3)))

js_data_std |> View() # 左上ペインに表として表示

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

#| include: false
js_pca <- js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = TRUE) # 標準化する

#| fig-width: 6
js_pca |>
  tidy("loadings") |>
  mutate(column = as_factor(column)) |>
  ggplot(aes(x = PC, y = value, fill = column)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5,
           position = position_dodge(width = 0.7, reverse = TRUE)) +
  scale_x_reverse() + coord_flip() +
  labs(x = "主成分", y = "負荷量", fill = NULL)

#| include: false
#' 寄与率に関する表を作成
js_pcs_tab <- 
  js_pca |>                              
  tidy("eigenvalues") |> # "d" または "eigenvalues" または "pcs"
  rename(主成分 = PC,
         標準偏差 = std.dev,
         寄与率 = percent,
         累積寄与率 = cumulative)

#' 寄与率に関する表を作成
js_pcs_tab |>                              
  View() # 左上ペインに表として表示

#| fig-width: 7
#' まとめて並べて表示
js_pca |>                              
  tidy("eigenvalues") |>
  pivot_longer(!PC) |>
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = PC, y = value, fill = name)) +
  geom_bar(stat="identity") +
  facet_wrap(~ name,
             scales = "free_y",
             labeller = labeller(name = c(
                                   std.dev = "標準偏差",
                                   percent = "寄与率",
                                   cumulative = "累積寄与率"))) +
  labs(x = "主成分") +
  theme(legend.position = "none") 

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

#| fig-width: 7
autoplot(js_pca, # バイプロット
         x = 3, y = 2,
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

#| include: false
js_pca <- js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = TRUE) # 標準化する

js_pca |>
  tidy("loadings") |>
  pivot_wider(names_from = "PC", names_prefix = "PC") |>
  View() # 左上ペインに表として表示

js_pca |>
  tidy("loadings") |>
  mutate(column = as_factor(column)) |>
  ggplot(aes(x = PC, y = value, fill = column)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5,
           position = position_dodge(width = 0.7, reverse = TRUE)) +
  scale_x_reverse() + coord_flip() +
  labs(x = "主成分", y = "負荷量", fill = NULL)

#| include: false
#' 寄与率に関する表を作成
js_pcs_tab <- 
  js_pca |>                              
  tidy("eigenvalues") |> # "d" または "eigenvalues" または "pcs"
  rename(主成分 = PC,
         標準偏差 = std.dev,
         寄与率 = percent,
         累積寄与率 = cumulative)

#' 寄与率に関する表を作成
js_pcs_tab |>                              
  View() # 左上ペインに表として表示

#' まとめて並べて表示
js_pca |>                              
  tidy("eigenvalues") |>
  pivot_longer(!PC) |>
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = PC, y = value, fill = name)) +
  geom_bar(stat="identity") +
  facet_wrap(~ name,
             scales = "free_y",
             labeller = labeller(name = c(
                                   std.dev = "標準偏差",
                                   percent = "寄与率",
                                   cumulative = "累積寄与率"))) +
  labs(x = "主成分") +
  theme(legend.position = "none") 

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
         loadings = TRUE,
         loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE,
         loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.size = 4)

#| fig-width: 7
autoplot(js_pca, # バイプロット
         x = 3, y = 2,
         asp = 1, # 縦横比を設定
         data = js_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE,
         label.label = "都道府県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.size = 3,
         label.show.legend = FALSE,
         loadings = TRUE,
         loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE,
         loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.size = 4)

#| fig-width: 6
autoplot(js_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = js_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE,
         label.label = "都道府県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.size = 3,
         label.show.legend = FALSE,
         loadings = TRUE,
         loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE,
         loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.size = 4) +
  theme(legend.position = "none") +
  xlim(-0.23,0.28) + ylim(-0.13,0.38)
