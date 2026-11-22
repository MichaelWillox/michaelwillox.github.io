Source files for my [personal website](https://michaelwillox.github.io/), built using [Quarto](https://quarto.org/).

## Publishing

Edit the `.qmd` files and push to `main`. A GitHub Action (`.github/workflows/publish.yml`) renders the site and deploys it to GitHub Pages, usually within two minutes. Progress shows on the repository's **Actions** tab.

To preview locally, run `quarto preview` (or click **Render** in RStudio). The local output goes to `_site/`, which is not committed.

## Layout

- `index.qmd`: home page
- `pages/`: About, Research, Blog listing, CV
- `posts/<name>/index.qmd`: one folder per blog post, with its images and any R scripts or data
- `images/`: shared images
