# Tufte style tokens and ggplot themes for the deck.
# pal: the one palette. theme_tufte(): the clean look. theme_junk(): ggplot's
# stock look, kept deliberately for every "as most tools draw it" panel.

library(ggplot2)

pal <- list(
  paper  = "#faf8f2",  ink   = "#191710",  rule = "#d5cfbf",
  muted  = "#7b7568",  g1    = "#e9e5d8",  g2   = "#c2bcab",
  g3     = "#6d6759",  accent = "#b3261e"
)

theme_tufte <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "Charter") %+replace%
    theme(
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = pal$paper, colour = NA),
      plot.background   = element_rect(fill = pal$paper, colour = NA),
      axis.line         = element_line(colour = pal$g3, linewidth = 0.3),
      axis.ticks        = element_line(colour = pal$g3, linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.text         = element_text(family = "Menlo", size = base_size * 0.55,
                                       colour = pal$g3),
      axis.title        = element_text(family = "Menlo", size = base_size * 0.55,
                                       colour = pal$muted),
      axis.title.y      = element_text(angle = 90, margin = margin(r = 6)),
      axis.title.x      = element_text(margin = margin(t = 6)),
      plot.title        = element_text(size = base_size * 0.62, family = "Menlo",
                                       colour = pal$muted, hjust = 0,
                                       margin = margin(b = 10)),
      plot.caption      = element_text(size = base_size * 0.5, family = "Menlo",
                                       colour = pal$muted, hjust = 0,
                                       margin = margin(t = 10)),
      plot.margin       = margin(8, 24, 8, 8),
      strip.text        = element_text(family = "Menlo", size = base_size * 0.55,
                                       colour = pal$g3, hjust = 0,
                                       margin = margin(b = 4)),
      panel.spacing     = unit(14, "pt"),
      legend.position   = "none",
      plot.title.position = "plot"
    )
}

# ggplot2's default grey theme, unretouched: the "before" look.
theme_junk <- function(base_size = 16) {
  theme_grey(base_size = base_size, base_family = "Helvetica") +
    theme(
      panel.border     = element_rect(colour = "grey25", fill = NA, linewidth = 0.8),
      legend.background = element_rect(colour = "grey25"),
      plot.title = element_text(size = base_size * 0.62, hjust = 0,
                                margin = margin(b = 10))
    )
}
