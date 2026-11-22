Source files for my [personal website](https://michaelwillox.github.io/), built using [Quarto](https://quarto.org/).

## Publishing

Edit the `.qmd` files and push to `main`. A GitHub Action (`.github/workflows/publish.yml`) renders the site and deploys it to GitHub Pages, usually within two minutes. Progress shows on the repository's **Actions** tab.

To preview locally, run `quarto preview` (or click **Render** in RStudio). The local output goes to `_site/`, which is not committed.

## Layout

- `index.qmd`: home page
- `pages/`: About, Research, Blog listing, CV
- `posts/<name>/index.qmd`: one folder per blog post, with its images and any R scripts or data
- `images/`: shared images

## Adding a publication

All publications live in [`data/publications.yml`](data/publications.yml). The Research page (with abstracts) and the CV page (citations only, plus the PDF at `pages/CV.pdf`) are both built from that file by the `{{< publications >}}` shortcode in `_extensions/publications/`. No R is needed.

1. Copy an entry in `data/publications.yml` and fill it in. `type` decides the section: `journal`, `report` (Statistics Canada working papers and reports), `working` (work in progress), or `phd`. Add `draft: true` to hide an entry until it is ready.
2. Push to `main`. The GitHub Action rebuilds the Research page, the CV and the CV PDF.
