# Simulate the fictional studies behind the deck and write them to data/.
# MER-201: phase 2, active treatment vs placebo, N = 240, primary endpoint PFS.
# MER-102: phase 1b dose-finding (placebo, 10 mg, 20 mg).
# Every number is invented. Run once; the qmd reads the CSVs.
# Base write.csv on purpose: reruns must be byte-identical, so the RNG call
# order below is load-bearing. Do not reorder draws.

library(survival)

# PFS (ADTTE-shaped) ----
# Search seeds so the KM medians land near 18.3 / 11.2 months.
sim_arm <- function(median_true, n) {
  lam <- log(2) / median_true
  t_event <- rexp(n, lam)
  t_cens  <- ifelse(runif(n) < 0.85, runif(n, 14, 24), runif(n, 4, 24))
  aval <- pmin(t_event, t_cens, 24)
  cnsr <- as.integer(t_event > pmin(t_cens, 24))   # 1 = censored
  data.frame(aval = aval, cnsr = cnsr)
}

km_median <- function(arm) {
  fit <- survfit(Surv(aval, 1 - cnsr) ~ 1, data = arm)
  unname(summary(fit)$table["median"])
}

best <- NULL
for (seed in 1:400) {
  set.seed(seed)
  active  <- sim_arm(16.5, 120)
  placebo <- sim_arm(12.0, 120)
  med_active  <- km_median(active)
  med_placebo <- km_median(placebo)
  if (is.na(med_active) || is.na(med_placebo)) next
  err <- abs(med_active - 18.3) + abs(med_placebo - 11.2)
  if (is.null(best) || err < best$err) best <- list(err = err, seed = seed)
}
set.seed(best$seed)
active  <- sim_arm(16.5, 120)
placebo <- sim_arm(12.0, 120)
adtte <- rbind(
  data.frame(usubjid = sprintf("MER201-%04d", 1:120),
             trt = "active treatment", active),
  data.frame(usubjid = sprintf("MER201-%04d", 121:240),
             trt = "placebo", placebo)
)
write.csv(adtte, "data/adtte.csv", row.names = FALSE)
message("KM medians: ", round(km_median(active), 1), " / ",
        round(km_median(placebo), 1), "  (seed ", best$seed, ")")

# Tumor size, % change from baseline at week 12 ----
set.seed(77)
clip <- function(x) pmax(-72, pmin(58, x))
tumor <- rbind(
  data.frame(trt = "active treatment", pchg = clip(rnorm(60, -21.5, 26))),
  data.frame(trt = "placebo",            pchg = clip(rnorm(60,  -3.0, 21)))
)
write.csv(tumor, "data/tumor_wk12.csv", row.names = FALSE)

# ORR (for the lie-factor slide) ----
write.csv(data.frame(trt = c("active treatment", "placebo"),
                     orr = c(42.3, 38.1)),
          "data/orr.csv", row.names = FALSE)

# Safety labs: mean % change from baseline by week and arm ----
weeks <- c(0, 2, 4, 8, 12, 18, 26)
lab_arm_rows <- function(analyte, active_pchg, placebo_pchg) {
  rbind(
    data.frame(analyte = analyte, week = weeks,
               trt = "active treatment", pchg = active_pchg),
    data.frame(analyte = analyte, week = weeks,
               trt = "placebo", pchg = placebo_pchg)
  )
}
lab_means <- rbind(
  lab_arm_rows("ALT",        c(0, 4, 9, 14, 18, 22, 24),   c(0, 1, 2, 1, 2, 3, 2)),
  lab_arm_rows("AST",        c(0, 3, 6, 9, 12, 14, 15),    c(0, 1, 1, 2, 2, 2, 3)),
  lab_arm_rows("ALP",        c(0, 1, 2, 3, 4, 4, 5),       c(0, 1, 1, 2, 2, 3, 3)),
  lab_arm_rows("Bilirubin",  c(0, 1, 3, 4, 4, 5, 5),       c(0, 0, 1, 1, 2, 2, 2)),
  lab_arm_rows("Creatinine", c(0, 2, 4, 6, 7, 8, 9),       c(0, 1, 1, 1, 2, 2, 2)),
  lab_arm_rows("Hemoglobin", c(0, -2, -4, -5, -6, -6, -6), c(0, -1, -1, -2, -2, -2, -2))
)
write.csv(lab_means, "data/lab_means.csv", row.names = FALSE)

# eGFR, subject level, active treatment arm ----
# 30 patients drift gently; 3 decline past -30% (one steeply, the focal patient).
set.seed(11)
egfr <- do.call(rbind, lapply(1:30, function(i) {
  base <- runif(1, 68, 105)
  drift <- runif(1, -0.15, 0.08)
  data.frame(usubjid = sprintf("MER201-%04d", i), week = weeks,
             egfr = base + drift * weeks + runif(length(weeks), -4, 4))
}))
decliners <- data.frame(
  patient  = c(7, 3, 19),
  baseline = c(92, 84, 99),
  slope    = c(1.45, 1.05, 1.18)
)
for (i in seq_len(nrow(decliners))) {
  id <- sprintf("MER201-%04d", decliners$patient[i])
  declining <- decliners$baseline[i] - decliners$slope[i] * weeks +
    runif(length(weeks), -2, 2)
  declining[1] <- decliners$baseline[i]
  egfr$egfr[egfr$usubjid == id] <- declining
}
write.csv(egfr, "data/egfr.csv", row.names = FALSE)

# MER-102 dose-finding: tumor burden mean % change by dose ----
dose_profile <- rbind(
  data.frame(dose = "placebo", week = weeks,
             pchg = c(0, -1, -2, -3, -4, -4, -4)),
  data.frame(dose = "10 mg",   week = weeks,
             pchg = c(0, -4, -8, -11, -13, -14, -15)),
  data.frame(dose = "20 mg",   week = weeks,
             pchg = c(0, -6, -11, -15, -18, -20, -22))
)
write.csv(dose_profile, "data/dose_profile.csv", row.names = FALSE)

# PFS subgroup forest ----
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

# Best overall response, active treatment arm (for the pie-chart kill) ----
bor <- data.frame(
  response = c("Partial response", "Stable disease", "Progressive disease",
               "Complete response", "Not evaluable"),
  n = c(20, 22, 10, 5, 3)
)
write.csv(bor, "data/bor.csv", row.names = FALSE)

# PK / response over time (for the dual-axis kill) ----
pk <- data.frame(
  week = weeks,
  conc = c(0, 410, 560, 640, 660, 665, 670),   # ng/mL, to steady state
  pchg = c(0, -6, -11, -15, -18, -20, -22)     # mean tumor change, %
)
write.csv(pk, "data/pk.csv", row.names = FALSE)

# Subject-level ALT fold-change (for the rainbow-heatmap kill) ----
set.seed(23)
alt_subj <- do.call(rbind, lapply(1:24, function(i) {
  peak <- if (i <= 4) runif(1, 2.2, 3.4) else runif(1, 0.9, 1.8)
  onset <- pmin(1, weeks / 12)
  data.frame(usubjid = sprintf("MER201-%04d", i), week = weeks,
             fold = round(1 + (peak - 1) * onset + rnorm(7, 0, 0.08), 2))
}))
write.csv(alt_subj, "data/alt_subj.csv", row.names = FALSE)
message("extra data written")
