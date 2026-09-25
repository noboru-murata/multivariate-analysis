### 第3講 資料

### 第3講 資料

#' 東京の気候データを用いた例
#' データの読み込み
tw_data <-
  read_csv("data/tokyo_weather.csv") |>
  mutate(date = date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week)) |>
  set_names(c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))
tw_subset <- read_csv("data/tokyo_weather.csv") |>
  filter(month == 8) |> # 9月のデータの抽出
  mutate(date = date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week)) |>
  set_names(c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))
#' モデル式
tw_formula1 <- 気温 ~ 気圧
tw_formula2 <- 気温 ~ 日射
tw_formula3 <- 気温 ~ 気圧 + 日射
tw_formula4 <- 気温 ~ 気圧 + 日射 + 湿度
tw_formula5 <- 気温 ~ 気圧 + 日射 + 雲量
#' 推定
tw_lm1 <- lm(tw_formula1, data = tw_subset, y = TRUE)
tw_lm2 <- lm(tw_formula2, data = tw_subset, y = TRUE)
tw_lm3 <- lm(tw_formula3, data = tw_subset, y = TRUE)
tw_lm4 <- lm(tw_formula4, data = tw_subset, y = TRUE)
tw_lm5 <- lm(tw_formula5, data = tw_subset, y = TRUE)

#' データの整理
tw_tbl <-
  tw_subset |>
  mutate(日付 = as.character(日付)) |> # pdfの表示
  gt()

#| tbl-cap: "東京の8月の気候データ"
tw_tbl

#| tbl-cap: "東京の8月の気候データ"
tw_tbl |>
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(80),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 5.5
#' 関連するデータの散布図
tw_subset |>
  select(気温,気圧,日射,湿度,雲量) |>
  ggpairs()

#| fig-width: 6
#' 観測値とあてはめ値の比較
tw_subset |>
  mutate(モデル1 = fitted(tw_lm1),    # モデルごとに予測値をデータフレームに追加
         モデル2 = fitted(tw_lm2),
         モデル3 = fitted(tw_lm3),
         モデル4 = fitted(tw_lm4),
         モデル5 = fitted(tw_lm5)) |>
  pivot_longer(starts_with("モデル"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = 気温, y = fitted)) + # 気温の実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(y = "あてはめ値") 

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差") |>
    add_glance_table(
      include = c(r.squared,adj.r.squared))}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~.x |> arrange(factor(variable,
                          levels = c("気圧",
                                     "日射",
                                     "湿度",
                                     "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "決定係数によるモデルの比較"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    add_glance_table(include = c(r.squared,statistic,p.value)) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~.x |> arrange(factor(variable,
                          levels = c("気圧",
                                     "日射",
                                     "湿度",
                                     "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "F統計量によるモデルの比較"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>                      # 標準の表
    modify_column_unhide(columns = c(statistic)) |> # 標準誤差とt統計量を表示
    add_significance_stars(
      pattern = "{estimate}({std.error})"
    ) |>
    add_significance_stars(
      hide_ci = TRUE,
      hide_se = TRUE,
      hide_p = FALSE,
      pattern = "{p.value}{stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**",
                  statistic = "**t統計量**",
                  p.value = "**p値**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
} 
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>            # 標準の表
    modify_column_unhide(columns = c(statistic)) |> # 標準誤差とt統計量を表示
    add_significance_stars(
      hide_ci = TRUE,
      hide_se = FALSE,
      hide_p = FALSE,
      pattern = "{p.value}{stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数**",
                  std.error = "**SE**",
                  statistic = "**t統計量**",
                  p.value = "**p値**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
} 
tw_gt <- 
  tbl_merge(
    tbls = list(
      ## my_gts(tw_lm1),
      ## my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",3:5)) |>
  ## tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~ .x |> arrange(factor(variable,
                           levels = c("(Intercept)",
                                      "気圧",
                                      "日射",
                                      "湿度",
                                      "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "t統計量によるモデルの比較"
tw_gt |>
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 診断プロット (モデル3)
autoplot(tw_lm3)

#' 診断プロット (モデル4)
autoplot(tw_lm4)

#' 診断プロット (モデル5)
autoplot(tw_lm5)

#' 東京の気候データによる分析
#' 信頼区間と予測区間の計算
tw_train <- tw_data |> filter(month(日付) %in% 8) # 推定用データ
tw_test  <- tw_data |> filter(month(日付) %in% 9) # 予測用データ
tw_model <- 気温 ~ 日射 + 気圧 + 湿度 # モデルの定義 
tw_lm <- lm(tw_model, data = tw_train) # モデルの推定

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
tw_gt <- 
  tbl_regression(tw_lm, intercept = TRUE) |>
  modify_column_unhide(columns = statistic) |>
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}"
  ) |>
  modify_header(label = "**変数**",
                estimate = "**係数**",
                std.error = "**標準誤差**",
                statistic = "**t統計量**",
                p.value = "**p値**") |>
  remove_abbreviation() |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "推定されたモデル"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(50),
              latex.tbl.pos = "h") |>
  as_latex()

#' 信頼区間
tw_train_conf <- # あてはめ値と信頼区間を付加
  augment(tw_lm,
          newdata = tw_train,
          interval = "confidence")
tw_test_conf <- # 新規データへのあてはめ値と信頼区間を付加
  augment(tw_lm,
          newdata = tw_test,
          interval = "confidence")

#' 予測区間
tw_train_pred <- # あてはめ値と予測区間を付加
  augment(tw_lm,
          newdata = tw_train,
          interval = "prediction")
tw_test_pred <- # 新規データへのあてはめ値と予測区間を付加
  augment(tw_lm,
          newdata = tw_test,
          interval = "prediction")

#| fig-width: 6
#' 8月のデータで推定したモデルで8月をあてはめた信頼区間
tw_train_conf |>
  ggplot(aes(x = 日付, y = 気温)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = .fitted), colour = "blue") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "royalblue") +
  ylim(c(20,34)) + # 4つのグラフで気温の範囲を揃える
  labs(title = "信頼区間")

#| fig-width: 6
#' 8月のデータで推定したモデルで8月をあてはめた予測区間
tw_train_pred |>
  ggplot(aes(x = 日付, y = 気温)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = .fitted), colour = "blue") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "steelblue") +
  ylim(c(20,34)) + # 4つのグラフで気温の範囲を揃える
  labs(title = "予測区間")

#| fig-width: 6
#' 8月のモデルで9月をあてはめた信頼区間
tw_test_conf |>
  ggplot(aes(x = 日付, y = 気温)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = .fitted), colour = "blue") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "royalblue") +
  ylim(c(20,34)) + # 4つのグラフで気温の範囲を揃える
  labs(title = "信頼区間")

#| fig-width: 6
#' 8月のモデルで9月をあてはめた予測区間
tw_test_pred |>
  ggplot(aes(x = 日付, y = 気温)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = .fitted), colour = "blue") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "steelblue") +
  ylim(c(20,34)) + # 4つのグラフで気温の範囲を揃える
  labs(title = "予測区間")

#' 表の作成
bb_data <-
  MASS::Animals |>
  rownames_to_column() |>
  as_tibble()
bb_tbl <-
  bb_data |>
  gt()

bb_tbl

bb_tbl |>
  tab_options(table.font.size = 11,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(50),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 6
#' 散布図 (normal plot)
MASS::Animals |>
  ggplot(aes(body, brain)) +
  geom_text_repel(aes(label = rownames(MASS::Animals)),
            size = 4,
            vjust = 1) +
  geom_point(colour = "royalblue") + 
  labs(title = "体重と脳の重さ (normal plot)",
       x = "体重 [kg]", y = "脳の重さ [g]") 

#| fig-width: 6
#' 散布図 (normal plot)
MASS::Animals |>
  ggplot(aes(body, brain)) +
  geom_text_repel(aes(label = rownames(MASS::Animals)),
                  size = 4,
                  vjust = 1) +
  geom_point(colour = "royalblue") + 
  scale_x_log10() +
  labs(title = "体重と脳の重さ (log-normal plot)",
       x = "体重 [kg]", y = "脳の重さ [g]") 

#| fig-width: 6
#' 散布図 (log-log plot)
MASS::Animals |>
  ggplot(aes(body, brain)) +
  geom_text_repel(aes(label = rownames(MASS::Animals)),
                  size = 4) +
  geom_point(colour = "royalblue") + 
  scale_x_log10() + scale_y_log10() +
  labs(title = "体重と脳の重さ (log-log plot)",
       x = "体重 [kg]", y = "脳の重さ [g]") 

#| fig-width: 6
#' 回帰式の表示
MASS::Animals |>
  ggplot(aes(body, brain)) +
  geom_smooth(method = lm, colour = "blue") + 
  geom_text_repel(aes(label = rownames(MASS::Animals)),
                  size = 4) +
  geom_point(colour = "royalblue") + 
  scale_x_log10() + scale_y_log10() +
  labs(title = "体重と脳の重さ",
       x = "体重 [kg]", y = "脳の重さ [g]") 

#| fig-width: 6
#' 回帰式の表示
MASS::Animals |>
  ggplot(aes(body, brain)) +
  geom_smooth(data = slice(MASS::Animals,-c(6,16,26)), # 外れ値を除去
              method = lm, colour = "blue") + 
  geom_text_repel(aes(label = rownames(MASS::Animals)),
                  size = 4) +
  geom_point(colour = "royalblue") + 
  scale_x_log10() + scale_y_log10() +
  labs(title = "体重と脳の重さ",
       x = "体重 [kg]", y = "脳の重さ [g]") 

#' 関連データの散布図
tw_subset <-
  tw_data |>
  filter(month(日付) == 9)
tw_subset |>
  select(気温, 日射, 気圧) |>
  ggpairs()

#' 日射と気圧の線形回帰モデル
tw_lm1 <- lm(気温 ~ 日射 + 気圧, data = tw_subset)
#' 日射と気圧の交互作用を加えた線形回帰モデル
tw_lm2 <- lm(気温 ~ 日射 * 気圧, data = tw_subset)
#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    add_glance_table(
      include = c(r.squared,adj.r.squared,statistic,p.value),
      label = list(statistic = "F統計量",
                   p.value = "p値")) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差") 
}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2)),
    tab_spanner = c("交互作用なし","交互作用あり")) |>
  modify_table_body(
    ~ .x |> arrange(factor(variable,
                           levels = c("日射",
                                      "気圧",
                                      "日射:気圧")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "交互作用の効果"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(50),
              latex.tbl.pos = "h") |>
  as_latex()

#' 関連データの散布図
tw_data_cat <-
  tw_data |>
  mutate(雨の有無 = 降雨 > 0,
         月 = as_factor(month(日付)))
tw_data_cat |>
  select(気温, 雨の有無, 月) |>
  ggpairs(aes(colour = 雨の有無),
          upper = list(continuous = wrap("cor", alpha = 0.6),
                       combo = wrap("box_no_facet", alpha = 0.6),
                       discrete = wrap("count", alpha = 0.6)),
          lower = list(continuous = wrap("points", alpha = 0.6),
                       combo = wrap("facethist", alpha = 0.6),
                       discrete = wrap("facetbar", alpha = 0.6)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.6),
                      discrete = wrap("barDiag", alpha = 0.6)))

#' 雨と気温の関係を分析
tw_lm3 <- lm(気温 ~ 雨の有無, data = tw_data_cat)
#' 月毎の気温の差を考慮して月を表す変数をダミー化する
tw_lm4 <- lm(気温 ~ 雨の有無 + 月, data = tw_data_cat)
#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>                      # 標準の表
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    add_glance_table(include = c(r.squared,
                                 adj.r.squared,
                                 statistic,
                                 p.value),
                     label = list(statistic = "F統計量",
                                  p.value = "p値")) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm3),
      my_gts(tw_lm4)),
    tab_spanner = c("雨の有無","雨の有無+月")) |>
  modify_table_body(
    ~ .x |> arrange(factor(variable,
                           levels = c("雨の有無",
                                      "月",
                                      "(Intercept)")))
  ) |>
  as_gt()

tw_gt

#| tbl-cap: "カテゴリカル変数の効果"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(45),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 8
#' 雨の有無の気温への影響
augment(tw_lm4, data = tw_data_cat) |>
  ggplot(aes(x = 日付, y = 気温)) +
  geom_point(aes(colour = 雨の有無)) +
  geom_point(aes(y = .fitted), colour = "limegreen") +
  labs(y = "あてはめ値")
