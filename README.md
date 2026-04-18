# substackstats

A small Shiny app for exploring Substack post stats.

## Run it

In R:

```r
shiny::runApp()
```

Or from a shell:

```sh
Rscript -e 'shiny::runApp()'
```

## Data sources

The app supports:

- Uploading `posts.json` or `roffe_posts.csv` in the UI, or
- Uploading `posts.json` or `user_posts.csv` in the UI, or
- Using local “test data” from `~/Downloads/posts.json` and/or `~/Downloads/user_posts.csv`.

When “Auto” is selected and both files exist, it merges them on `id`:

- `user_posts.csv` provides most numeric metrics
- `posts.json` provides “Tilgang” (Alle / Kun betalende)
