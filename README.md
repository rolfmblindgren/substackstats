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

- Uploading `posts.json` and/or `user_posts.csv` in the UI.

If you upload both files, the app merges them on `id`:

- `user_posts.csv` provides most numeric metrics
- `posts.json` provides “Tilgang” (Alle / Kun betalende)
