Source files for my [personal website](https://michaelwillox.github.io/), built using [Quarto](https://quarto.org/)

## Adding a publication

All publications live in [`data/publications.yml`](data/publications.yml). The Research page (with abstracts) and the CV page (citations only, plus the PDF at `pages/CV.pdf`) are both built from that file by the `{{< publications >}}` shortcode in `_extensions/publications/`. No R is needed.

1. Copy an entry in `data/publications.yml` and fill it in. `type` decides the section: `journal`, `report`, `working`, or `phd`. Add `draft: true` to hide an entry until it is ready.
2. Render the site (`quarto render` or the Render button in RStudio) and commit the `docs/` folder as usual.

Rendering the CV PDF needs Quarto 1.4 or later (Typst is bundled, so no LaTeX is needed).
