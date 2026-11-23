### 第8講 資料

#| include: false
#' 判別分析の例
#' 表の作成
bio_data <- MASS::biopsy |>
  as_tibble() |>
  na.omit() # NA を除く

bio_data |> View() # 左上ペインに表として表示

#| fig-width: 5.6
  bio_data |>
    select(!ID) |> # IDを除く
    ggpairs(upper = list(continuous = wrap("cor", size = 2.5)),
            diag = list(continuous = wrap("densityDiag", alpha = 0.7),
                        mapping = aes(colour = class)),
            lower = list(continuous = wrap("points", alpha = .7),
                         mapping = aes(colour = class))) +
theme(strip.text = element_text(size = 8),
      axis.text = element_text(size = 5))

#| fig-width: 6.4
bio_data |>
  select(where(is.numeric)) |>
  prcomp() |> # 主成分分析
  autoplot(data = bio_data,
           asp = 1,
           colour = "class")

#| fig-width: 5.6
bio_lda <- lda(class ~ . -ID, data = bio_data)
bio_data |>
  mutate(x = predict(bio_lda)[["x"]]) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(fill = class), alpha = 0.7,
                 show.legend = FALSE) +
  facet_grid(class ~ .)

#| fig-width: 6
#' 東京の気象データによる判別分析
library(MASS) # パッケージの読み込み
#' データの整理
tw_data <- read_csv("data/tokyo_weather.csv")
tw_subset  <- tw_data |> 
  filter(month %in% c(9,10)) |> # 9,10月のデータ
  select(temp, humid, month) |> # 気温・湿度・月を選択
  mutate(month = as_factor(month)) # 月を因子化
#' 視覚化
tw_subset |> # 気温と湿度の散布図を作成
  ggplot(aes(x = temp, y = humid)) + 
  geom_point(aes(colour = month)) + # 月ごとに点の色を変える
  labs(x = "気温", y = "湿度", 
       colour = "月",
       title = "東京の気候 (9月・10月)")

#| fig-width: 6
#' 線形判別関数の推定
tw_model <- month ~ temp + humid
tw_lda <- lda(formula = tw_model, data = tw_subset)
tw_lda_fitted <- predict(tw_lda) # 判別関数によるクラス分類結果の取得
as_tibble(tw_lda_fitted[["x"]]) |> # リストから判別関数値(配列)を取得
  mutate(month = tw_subset[["month"]]) |> # 月(真値)を追加
  ggplot(aes(x = LD1, fill = month)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  labs(fill = "月") +
  facet_grid(month ~ .) # 月ごとに表示(y方向に並べる)

#| fig-width: 6
#' 判別関数により予測されるラベルの図示
range_x <- range(tw_subset[["temp"]])  # 気温の値域
range_y <- range(tw_subset[["humid"]]) # 湿度の値域
grid_x <- pretty(range_x, 100) # 気温の値域の格子点を作成
grid_y <- pretty(range_y, 100) # 湿度の値域の格子点を作成
grid_xy <- expand.grid(temp = grid_x,
                       humid = grid_y) # 2次元の格子点を作成
tw_lda_grid <- predict(tw_lda, # 格子点上の判別関数値を計算
                       newdata = grid_xy)
as_tibble(grid_xy) |> 
  mutate(predict = tw_lda_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  geom_point(data = tw_subset,
             aes(x = temp, y = humid, colour = month)) +
  labs(x = "気温", y = "湿度",
       fill = "予測", colour = "真値",
       title = "線形判別")


#| fig-width: 6
  #' 2次判別関数を推定
tw_qda <- qda(formula = tw_model, data = tw_subset)
tw_qda_fitted <- predict(tw_qda) # 判別関数によるクラス分類結果の取得
#' 判別関数により予測されるラベルの図示
tw_qda_grid <- predict(tw_qda, # 格子点上の判別関数値を計算
                       newdata = grid_xy)
as_tibble(grid_xy) |> 
  mutate(predict = tw_qda_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  geom_point(data = tw_subset,
             aes(x = temp, y = humid, colour = month)) +
  labs(x = "温度", y = "湿度",
       fill = "予測", colour = "真値",
       title = "2次判別")

#| fig-width: 6
#' 多値判別
tw_subset3  <- tw_data |>
  filter(month %in% c(9,10,11)) |>
  select(temp, humid, month) |>
  mutate(month = as_factor(month))

#' 線形判別関数(3値)を作成
tw_lda3 <- lda(formula = tw_model, data = tw_subset3)
tw_lda3_fitted <- predict(tw_lda3) # 判別関数によるクラス分類結果の取得

#' 推定された線形判別関数(3値)により予測されるラベルの図示
#' 格子点は再計算する必要がある
range_x <- range(tw_subset3[["temp"]])  # 気温の値域
range_y <- range(tw_subset3[["humid"]]) # 湿度の値域
grid_x <- pretty(range_x, 100) # 気温の値域の格子点を作成
grid_y <- pretty(range_y, 100) # 湿度の値域の格子点を作成
grid_xy <- expand.grid(temp = grid_x,
                       humid = grid_y) # 2次元の格子点を作成
tw_lda3_grid <- predict(tw_lda3, # 格子点上の判別関数値を計算
                        newdata = grid_xy)
as_tibble(grid_xy) |> 
  mutate(predict = tw_lda3_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  geom_point(data = tw_subset3,
             aes(x = temp, y = humid, colour = month)) +
  labs(x = "温度", y = "湿度",
       fill = "予測", colour = "真値",
       title = "多値判別")
