--[[ no-hrule.lua ------------------------------------------------------------

`---` は revealjs ではスライドの区切りだが、pandoc の LaTeX ライタは
HorizontalRule として

    \begin{center}\rule{0.5\linewidth}{0.5pt}\end{center}

を吐くため、PDF では本文中に横罫線が残ってしまう。このフィルタは PDF 系の
出力でだけそれを取り除く。HTML / revealjs では何もしないので、スライドの
分割はそのまま働く。

使い方（qmd の YAML、またはプロジェクトの _quarto.yml）:

    filters:
      - no-hrule.lua

罫線の代わりに改ページを入れたいときは下の HRULE_PDF を "newpage" に。
------------------------------------------------------------------------- ]]

local HRULE_PDF = "drop"   -- "drop" | "newpage"

function HorizontalRule(el)
  if quarto.doc.is_format("pdf")
     or quarto.doc.is_format("latex")
     or quarto.doc.is_format("beamer") then
    if HRULE_PDF == "newpage" then
      return pandoc.RawBlock("latex", "\\newpage")
    end
    return {}      -- 出力しない
  end
  return nil       -- revealjs / html などはそのまま
end
