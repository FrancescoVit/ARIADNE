# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

ARIADNE (shARed mInotAur Database exploratioN Environment) is a single-page R Shiny dashboard for exploring
European soil biodiversity data collected in the MINOTAUR project. It's plain base R + Shiny — no package
manager, build step, lockfile, or test suite.

## Running the app

Open the project in R/RStudio and run:

```r
shiny::runApp()
```

`app.R` just sources `global.R`, `ui.R`, `server.R` in that order and calls `shinyApp(ui, server)`.
`global.R` auto-installs any of its required packages (`shiny`, `shinydashboard`, `tidyverse`, `plotly`,
`rstatix`, `maps`, `shinythemes`) that aren't already present, so the first run may take a while.

**The app will not start without local data.** `global.R` reads CSVs from `next_release/data_source/`
(a hardcoded relative path, `MINOTAUR_rawdata_source`). That folder is git-ignored entirely — it is not part
of this repo and must be obtained/placed locally before the app can run. There is no synthetic/sample dataset
checked in.

There is no linter, formatter, or automated test suite configured for this project.

## Deployment

Deployed to shinyapps.io via `rsconnect` (see `rsconnect/shinyapps.io/.../ariadne.dcf`) at
https://rg7u3g-francesco0vitali.shinyapps.io/ariadne/. The `rsconnect/` folder is git-ignored except for
tracked deployment metadata; do not commit account tokens (`rsconnect::setAccountInfo` credentials belong in
a local, untracked `.Rhistory`/session only, never in source files).

## Architecture

The app is three top-level files at the repo root, each sourced once by `app.R`:

- **`global.R`** — data layer. Loads raw CSVs (metadata + per-taxon observation tables) from
  `next_release/data_source/`, selects/curates a subset of columns from each, joins the four metadata tables
  (`study`, `soil`, `scope`, `agri`) into one `metadata_MINOTAUR_selected` data frame keyed by
  `id_sampling_point`, and builds lookup structures used throughout the UI/server: `sample_list` (sampling
  point IDs per biota group: `bact`/`fung`/`micro`/`meso`/`macro`), `common_vars` (named vector mapping
  human-readable labels to the metadata column they plot against), and factor-level vectors
  (`farming_systems`, `land_uses`, `country_codes`, `macro_taxon_codes`).
- **`ui.R`** — a `shinydashboard` with three tabs: **Home** (static landing page), **Overview** (sample
  selection filters + map/country summary plots), **Analysis** (per-taxon biodiversity index plots, tables,
  and correlation/tile plots).
- **`server.R`** — one large `server` function built around a single reactive filter,
  `sites_selected_reactive()`, which applies the Overview tab's filters (`input$type` land use,
  `input$manag` management, `input$state_select` countries, `input$biolevel_select` biota groups) to
  `metadata_MINOTAUR_selected` and returns the filtered sample set. Every downstream output (map, country
  bar chart, and every per-taxon biodiversity block) re-derives from this one reactive, so a change to the
  filtering logic in `sites_selected_reactive()` affects the entire app.

### The per-taxon repeated block

The Analysis tab's "Biodiversity and Ecological indices" panel repeats the same structure for five biota
groups — **Bacteria, Fungi, Microfauna, Mesofauna, Macrofauna** — each as its own collapsible box in `ui.R`
and its own matching block of `output$...` handlers in `server.R` (e.g. `plot_biodiv_index_bacteria`,
`table_bacteria_correl_or_kruskal`, `tile_plot_bacteria`, and the equivalent `_fungi`/`_meso`/`_micro`/`_macro`
outputs). These blocks are structurally parallel but not factored into a shared function/module — when fixing
a bug or changing behavior in one taxon's block, check whether the same fix is needed in the other four
(this is exactly the pattern the "fix biota level filtering" commit addressed).

### `past_release/` vs `next_release/`

- **`past_release/`** holds frozen, versioned snapshots of previous releases (`global4.0.R`/`server4.0.R`/
  `ui4.0.R`, `...4.1.R`, etc.) for reference/history. These are not sourced by the live app — do not edit them
  to fix current bugs.
- **`next_release/`** (git-ignored, not present in a fresh clone) is the working area where the *next* version
  and its `data_source/` CSVs live locally before being promoted to the repo root. If you find yourself
  needing the raw data files, this is where they'd be placed.

Current live version is tracked in the header comments of `app.R`/`global.R`/`server.R`/`ui.R` (update all
four together when bumping the version).
