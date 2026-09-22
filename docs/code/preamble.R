library(conflicted)
conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    dplyr::lag(),
    )
library(tidyverse)
library(ggfortify)     # 分析結果の視覚化にggplotを利用するためのパッケージ
library(GGally)
library(ggrepel)
library(gt)            # 表を作成するためのパッケージ
library(gtsummary)
library(broom)
library(broom.helpers) # gtsummary のいくつかの関数で利用(インストールされていれば不要)
library(MASS)          # パッケージ MASS に含まれる判別分析のための関数を利用
library(cluster)       # クラスタ分析のためのパッケージ
library(ggdendro)      # ggplot でデンドログラムを描くためのパッケージ
library(tsibble)       # 時系列を扱うためのパッケージ
library(feasts)        # ggplotで時系列を扱うための拡張パッケージ
library(fable)         # 時系列関連
library(ggtime)        # 時系列関連
#' 日本語表示の設定
#' 出力形式に応じた作図デバイス
if (knitr::is_latex_output()) knitr::opts_chunk$set(dev = "cairo_pdf")

#' 日本語表示の設定
if (Sys.info()["sysname"] == "Darwin") {
    jp_font <- if (knitr::is_latex_output()) {
                   "Hiragino Maru Gothic ProN"   # cairo は fontconfig 経由なのでファミリ名
               } else {
                   "HiraMaruProN-W4"             # quartz/AGG は PostScript 名
               }
    theme_update(text = element_text(family = jp_font))
    update_geom_defaults("text", list(family = theme_get()$text$family))
    update_geom_defaults("label", list(family = theme_get()$text$family))
    update_geom_defaults("text_repel", list(family = theme_get()$text$family))
    update_geom_defaults("label_repel", list(family = theme_get()$text$family))
} else {
    jp_font <- NULL
}
#' 色の設定
library(see)
options(ggplot2.discrete.colour = function() scale_colour_material(),
        ggplot2.discrete.fill = function() scale_fill_material())
