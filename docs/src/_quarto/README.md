# Decorated slide headers for Quarto reveal.js

Two matching theme layers, one for a dark deck and one for a light deck:

- `#` (h1) — centered section title
- `##` (h2) — logo on the left, title text right aligned, and a rule underneath
  that starts solid at the left and fades out towards the right

```
dracula/dracula-header.scss   on top of Quarto's dracula theme
default/default-header.scss   on top of Quarto's default theme
*/logo.svg                    the bundled logo: a plain 10x64 vertical bar
*/logo-mark.svg, logo.png     an abstract mark, if you prefer a real logo shape
svg2datauri.py                turns your logo into a $header-logo: line
logo.scss                     template for pointing the theme at your own logo
```

Both files expose the same variables and the same per-slide classes, so
switching a deck between light and dark is a one-line change.

## Use

```yaml
# dark
format:
  revealjs:
    theme: [dracula, dracula-header.scss]
```

```yaml
# light
format:
  revealjs:
    theme: [default, default-header.scss]
```

Copy just the one `.scss` next to your `.qmd`. The placeholder logo is embedded
in it as a data URI, so there is nothing else to copy and nothing is fetched at
render time.

## Your own logo

SCSS variables cannot be set from YAML, so point the theme at your file with a
one-line SCSS listed **after** the header theme in `theme:`. `logo.scss` in this
folder is a ready-made template.

```yaml
format:
  revealjs:
    theme: [default, default-header.scss, logo.scss]
resources:
  - images/
```

```scss
/*-- scss:defaults --*/
$header-logo: url("../../../../../images/logo.png");
```

### Why five `../`

Quarto compiles the theme to

```
<doc>_files/libs/revealjs/dist/theme/quarto-<hash>.css
```

and a `url()` in CSS resolves relative to the CSS file, not to the `.qmd`. Five
levels up lands back in your document's directory, so `images/logo.png` next to
the `.qmd` is `../../../../../images/logo.png` from the stylesheet. A bare
`url("logo.png")` looks inside the theme folder and 404s.

Verified on Quarto 1.8, including with `embed-resources: true`, where pandoc
follows that path and inlines the image for you. Add `resources:` so the file is
copied to the output directory.

A root-relative `url("/images/logo.png")` also works, but only for a deck served
over http(s) — not when opening the `.html` off disk.

### Or embed it, and forget about paths

```bash
python3 svg2datauri.py images/logo.png
```

Paste the printed `$header-logo:` line into `logo.scss` (or over the one in the
theme). SVG is percent-encoded, raster formats are base64-encoded; either way
the result is one self-contained line that cannot break when Quarto changes its
output layout. This is how the bundled placeholder logo is stored.

The script also reports the pixel size and, for a wide logo, the
`$header-logo-width` to use:

```
// my-logo.png: data URI is 2.4 KB
// 640x160 px
// wide logo — set  $header-logo-width: 4.60em;
```

### The bundled bar

`logo.svg` in each theme folder is a plain vertical bar — a 10 x 64 rounded
rect, coloured to match the start of the header rule (`#bd93f9` on dracula,
`#2a76dd` on default). It is what the theme ships with, embedded as a data URI.

Because it is 10 wide and 64 tall, the logo box has to be narrow or the title
sits far from it:

```scss
$header-logo-width: 0.18em;   // = $header-logo-size * 10/64
```

To make the bar thicker or thinner, change the `width`/`viewBox` in the SVG and
re-run `svg2datauri.py`, or just scale `$header-logo-width` — the bar is drawn
with `background-size: contain`, so a wider box makes a proportionally taller
bar, not a stretched one. To change its colour, edit the `fill` in `logo.svg`.

`logo-mark.svg` and `logo.png` are the earlier abstract mark, kept in case you
want a conventional logo shape; they are square, so pair them with
`$header-logo-width: $header-logo-size;`.

### Raster logos

- **Size.** The logo box is about 1.15 em tall and reveal.js scales the whole
  deck up on large displays, so aim for **256 px on the long edge**. Below
  128 px it softens on a HiDPI screen; the script warns about this.
- **Shape.** Square logos need nothing. A wide one needs `$header-logo-width` —
  the image is drawn with `background-size: contain`, so it is never stretched
  and a slightly generous width just adds empty space.
- **Transparency.** Use a transparent background. If your logo only exists on
  the wrong background, set `$header-logo-plate: true` — it draws a plate behind
  the logo (light on the dracula theme, dark on the default theme).
- **Weight.** base64 grows the file by about a third and it lands in every
  rendered HTML. A 256 px logo is typically 3–15 KB. If yours is much larger:
  ```bash
  magick my-logo.png -resize 256x256 -strip logo.png
  ```

## Knobs

All in `scss:defaults`. Defaults differ per theme where marked.

| variable | dracula | default | what it does |
|---|---|---|---|
| `$header-rule-color` | `#bd93f9` | `#2a76dd` | rule color at the left edge |
| `$header-rule-color-2` | `#ff79c6` | `#6741d9` | tint at 35%; set equal to the above for one hue |
| `$header-rule-height` | `4px` | `4px` | rule thickness |
| `$header-rule-fade` | `85%` | `85%` | where the rule becomes fully transparent |
| `$header-rule-gap` | `0.25em` | `0.25em` | gap between title and rule |
| `$header-logo-size` | `1.15em` | `1.15em` | logo height, relative to the h2 font |
| `$header-logo-width` | `0.18em` | `0.18em` | logo box width; `$header-logo-size` for a square logo, more for a wide one |
| `$header-logo-gap` | `0.5em` | `0.5em` | gap between logo and title |
| `$header-logo-offset` | `0` | `0` | vertical nudge for the logo |
| `$header-logo-plate` | `false` | `false` | backing plate behind the logo |
| `$header-logo-plate-color` | `#f8f8f2` | `#222` | plate color |
| `$header-h1-color` | `#f8f8f2` | `#222` | h1 color |
| `$header-h2-color` | `#f8f8f2` | `#222` | h2 color |
| `$presentation-h1-font-size` | `2.5em` | `2.5em` | h1 size |

## Per-slide opt-outs

```markdown
## No logo on this one {.no-logo}
## No rule under this one {.no-rule}
## Plain heading {.plain-header}
```

## Vertical alignment

The header stays pinned at the top; only what follows it moves.

```markdown
## Title {.v-center}    body centred in the space under the header
## Title {.v-bottom}    body pushed to the bottom
## Title {.v-top}       explicit top (to opt out of a deck-wide default)
```

Deck-wide default in `scss:defaults` — changing this one line is all it takes:

```scss
$slide-v-align: center;   // top (default) | center | bottom
```

The deck's own title slide is skipped: an auto margin under its `h1` would tear
the subtitle, author and date away from the title. A slide that names its own
alignment (`{.v-top}` and friends) is left to that class.

Use reveal's own `center: false` (Quarto's default) with this. `center: true`
would move the header as well, which is the thing this is meant to avoid.

Implemented with flexbox and auto margins: the heading takes the free space
below it, and a second auto margin on the last child splits that space in two,
so the block between them ends up centred. Consequences worth knowing:

- An aligned slide turns its direct children into flex items, and **`float` has
  no effect on a flex item**. Keep float-based column helpers on top-aligned
  slides.
- `display` is set `!important`, because reveal.css marks the visible slide
  `display: block !important`.
- Only the slide on screen is switched to flex, so reveal's `display: none` for
  the others is untouched. print-pdf, where every slide is shown at once and
  reveal wraps each in a `.pdf-page`, has its own selector.

## Notes

- The h2 is a flex container: the logo is `::before` with `margin-right: auto`
  (which is what pushes the title right), and the rule is `::after` with
  `flex: 0 0 100%` so it sits on its own full-width line. No extra markup.
- The gradient ends on its own hue at zero alpha rather than on the
  `transparent` keyword, so it fades cleanly instead of drifting towards grey.
- Nested (vertical) slides get a thinner, dimmer rule so the hierarchy reads.
- `print-color-adjust: exact` is set on the header, otherwise Chrome drops the
  gradient and the logo when exporting to PDF.
- Pandoc marks every `#` slide `title-slide slide level1`, and Quarto then
  shrinks that h1 to the h2 size unless reveal.js has set
  `data-navigation-mode="linear"`. Both files restore the h1 size in all
  navigation modes.
