# Simulate the fictional studies behind the deck and write them to data/.
# MER-201: phase 2, meriplatinib 20 mg vs placebo, N = 240, primary endpoint PFS.
# MER-102: phase 1b dose-finding (placebo, 10 mg, 20 mg).
# Every number is invented. Run once; the qmd reads the CSVs.

# ---- PFS (ADTTE-shaped): search seeds so the KM medians land near 18.3 / 11.2 ----
sim_arm <- function(median_true, n) {
  lam <- log(2) / median_true
  t_event <- rexp(n, lam)
  t_cens  <- ifelse(runif(n) < 0.85, runif(n, 14, 24), runif(n, 4, 24))
  aval <- pmin(t_event, t_cens, 24)
  cnsr <- as.integer(t_event > pmin(t_cens, 24))   # 1 = censored
  data.frame(aval = aval, cnsr = cnsr)
}

km_median <- function(d) {
  sf <- survival::survfit(survival::Surv(aval, 1 - cnsr) ~ 1, data = d)
  unname(summary(sf)$table["median"])
}

best <- NULL
for (seed in 1:400) {
  set.seed(seed)
  a <- sim_arm(16.5, 120); b <- sim_arm(12.0, 120)
  ma <- km_median(a); mb <- km_median(b)
  if (is.na(ma) || is.na(mb)) next
  err <- abs(ma - 18.3) + abs(mb - 11.2)
  if (is.null(best) || err < best$err) best <- list(err = err, seed = seed)
}
set.seed(best$seed)
a <- sim_arm(16.5, 120); b <- sim_arm(12.0, 120)
adtte <- rbind(
  data.frame(usubjid = sprintf("MER201-%04d", 1:120), trt = "meriplatinib 20 mg", a),
  data.frame(usubjid = sprintf("MER201-%04d", 121:240), trt = "placebo", b)
)
write.csv(adtte, "data/adtte.csv", row.names = FALSE)
message("KM medians: ", round(km_median(a), 1), " / ", round(km_median(b), 1),
        "  (seed ", best$seed, ")")

# ---- Tumor size, % change from baseline at week 12 ----
set.seed(77)
clip <- function(x) pmax(-72, pmin(58, x))
tumor <- rbind(
  data.frame(trt = "meriplatinib 20 mg", pchg = clip(rnorm(60, -21.5, 26))),
  data.frame(trt = "placebo",            pchg = clip(rnorm(60,  -3.0, 21)))
)
write.csv(tumor, "data/tumor_wk12.csv", row.names = FALSE)

# ---- ORR (for the lie-factor slide) ----
write.csv(data.frame(trt = c("meriplatinib 20 mg", "placebo"), orr = c(42.3, 38.1)),
          "data/orr.csv", row.names = FALSE)

# ---- Safety labs: mean % change from baseline by week and arm ----
weeks <- c(0, 2, 4, 8, 12, 18, 26)
lab <- function(analyte, act, plc) rbind(
  data.frame(analyte = analyte, week = weeks, trt = "meriplatinib 20 mg", pchg = act),
  data.frame(analyte = analyte, week = weeks, trt = "placebo",            pchg = plc)
)
lab_means <- rbind(
  lab("ALT",        c(0, 4, 9, 14, 18, 22, 24),      c(0, 1, 2, 1, 2, 3, 2)),
  lab("AST",        c(0, 3, 6, 9, 12, 14, 15),       c(0, 1, 1, 2, 2, 2, 3)),
  lab("ALP",        c(0, 1, 2, 3, 4, 4, 5),          c(0, 1, 1, 2, 2, 3, 3)),
  lab("Bilirubin",  c(0, 1, 3, 4, 4, 5, 5),          c(0, 0, 1, 1, 2, 2, 2)),
  lab("Creatinine", c(0, 2, 4, 6, 7, 8, 9),          c(0, 1, 1, 1, 2, 2, 2)),
  lab("Hemoglobin", c(0, -2, -4, -5, -6, -6, -6),    c(0, -1, -1, -2, -2, -2, -2))
)
write.csv(lab_means, "data/lab_means.csv", row.names = FALSE)

# ---- eGFR, subject level, meriplatinib arm: 30 patients, 3 decline past -30% ----
set.seed(11)
egfr <- do.call(rbind, lapply(1:30, function(i) {
  base <- runif(1, 68, 105); drift <- runif(1, -0.15, 0.08)
  data.frame(usubjid = sprintf("MER201-%04d", i), week = weeks,
             egfr = base + drift * weeks + runif(length(weeks), -4, 4))
}))
for (spec in list(c(7, 92, 1.45), c(3, 84, 1.05), c(19, 99, 1.18))) {
  id <- sprintf("MER201-%04d", spec[1])
  egfr$egfr[egfr$usubjid == id] <-
    spec[2] - spec[3] * weeks + runif(length(weeks), -2, 2)
  egfr$egfr[egfr$usubjid == id][1] <- spec[2]
}
write.csv(egfr, "data/egfr.csv", row.names = FALSE)

# ---- MER-102 dose-finding: tumor burden mean % change by dose ----
prof <- rbind(
  data.frame(dose = "placebo", week = weeks, pchg = c(0, -1, -2, -3, -4, -4, -4)),
  data.frame(dose = "10 mg",   week = weeks, pchg = c(0, -4, -8, -11, -13, -14, -15)),
  data.frame(dose = "20 mg",   week = weeks, pchg = c(0, -6, -11, -15, -18, -20, -22))
)
write.csv(prof, "data/dose_profile.csv", row.names = FALSE)

# ---- PFS subgroup forest ----
forest <- data.frame(
  subgroup = c("Overall", "Age under 65", "Age 65 and over", "Male", "Female",
               "ECOG 0", "ECOG 1", "Prior platinum", "No prior platinum"),
  n  = c(240, 134, 106, 138, 102, 118, 122, 96, 144),
  hr = c(0.62, 0.58, 0.68, 0.65, 0.57, 0.55, 0.72, 0.81, 0.51),
  lo = c(0.47, 0.41, 0.45, 0.46, 0.37, 0.37, 0.49, 0.53, 0.35),
  hi = c(0.81, 0.83, 1.02, 0.92, 0.88, 0.80, 1.06, 1.24, 0.74)
)
write.csv(forest, "data/forest.csv", row.names = FALSE)
message("data/ written")

# ---- Best overall response, meriplatinib arm (for the pie-chart kill) ----
bor <- data.frame(
  response = c("Partial response", "Stable disease", "Progressive disease",
               "Complete response", "Not evaluable"),
  n = c(20, 22, 10, 5, 3)
)
write.csv(bor, "data/bor.csv", row.names = FALSE)

# ---- PK / response over time (for the dual-axis kill) ----
weeks_pk <- c(0, 2, 4, 8, 12, 18, 26)
pk <- data.frame(
  week = weeks_pk,
  conc = c(0, 410, 560, 640, 660, 665, 670),           # ng/mL, to steady state
  pchg = c(0, -6, -11, -15, -18, -20, -22)             # mean tumor change, %
)
write.csv(pk, "data/pk.csv", row.names = FALSE)

# ---- Subject-level ALT fold-change (for the rainbow-heatmap kill) ----
set.seed(23)
alt_subj <- do.call(rbind, lapply(1:24, function(i) {
  peak <- if (i <= 4) runif(1, 2.2, 3.4) else runif(1, 0.9, 1.8)
  shape <- pmin(1, weeks_pk / 12)
  data.frame(usubjid = sprintf("MER201-%04d", i), week = weeks_pk,
             fold = round(1 + (peak - 1) * shape + rnorm(7, 0, 0.08), 2))
}))
write.csv(alt_subj, "data/alt_subj.csv", row.names = FALSE)
message("extra data written")
