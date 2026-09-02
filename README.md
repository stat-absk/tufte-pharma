# Show every patient

A 15-minute Quarto presentation on Edward Tufte's principles for the figures
in a clinical study report, aimed at clinical statisticians. Nineteen slides:
all ten of Tufte's rules, each demonstrated as a side-by-side pair (the junk
version on the left, the revised version on the right), then a kill list
drawn the same way (response pie, dual-axis overlay, rainbow heatmap), and a
pre-ship checklist.

Every figure comes from two invented studies (MER-201, a phase 2 trial of
meriplatinib vs placebo; MER-102, a phase 1b dose-finding study) so the
numbers reconcile across slides. Nothing is real.

## Build

```sh
Rscript R/sim_data.R        # regenerate data/ (deterministic, seeds fixed)
quarto render tufte-pharma.qmd
```

Open `tufte-pharma.html` in a browser. `embed-resources: true`, so the file
is self-contained and can be mailed around. Press `s` while presenting for
speaker notes; each slide's notes carry a talk track and a minute budget
that sums to about 15.

## Layout

| Path | Purpose |
|---|---|
| `tufte-pharma.qmd` | The deck: revealjs, all figures built inline with ggplot2 + survival |
| `theme.scss` | Reveal theme: paper ground, Charter/EB Garamond, IBM Plex Mono, one red accent |
| `R/sim_data.R` | Simulates the datasets into `data/` (ADTTE-shaped PFS, tumor response, labs, eGFR, forest) |
| `R/theme_tufte.R` | `pal` tokens, `theme_tufte()`, and `theme_junk()` (stock ggplot look, used for every "before" panel) |
| `data/` | Generated CSVs, committed for reproducibility |

## Requirements

R with ggplot2, dplyr, survival, scales, ragg; Quarto ≥ 1.4 (the RStudio-bundled
quarto works). Figures use the system fonts Charter and Menlo, with Georgia
as fallback.

## Sources

*The Visual Display of Quantitative Information* (1983), *Envisioning
Information* (1990), *Visual Explanations* (1997). Quotes cited by page in
the slides.
