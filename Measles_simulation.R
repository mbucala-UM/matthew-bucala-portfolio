# =============================================================================
# Comparative Modeling of Masking, Room Ventilation, and Vaccination to Reduce
# Measles Transmission in Outpatient Waiting Rooms
#
# Tang, Bucala, Brenner | Revised for ICHE (Round 2)
#
# This script is a SINGLE consolidated pipeline replacing the three concatenated
# script blocks in the previous version. All parameters, seeds, and outputs are
# documented and printed to console for reviewer verification.
#
# Changes from prior version (point-by-point, per Reviewer 5):
#   1. Consolidated three duplicate script blocks into one pipeline.
#   2. Explicit set.seed() calls at every stochastic section (42, 123, 456).
#   3. Parameters aligned to manuscript: p_base = 0.25, mask_eff = 0.60,
#      p_unvax = 0.08, ACH_baseline = 6, wait = 30 min.
#   4. Mask adherence now modeled continuously per the manuscript formula:
#      f_mask = 1 - (adherence * effectiveness), NOT as a binary toggle.
#   5. Vaccination applied per-agent via Bernoulli; only unvaccinated are at risk.
#   6. Overdispersion formally tested (Pearson dispersion + LR test + AIC).
#      In these simulation data Pearson dispersion ~ 1.0 and NB theta diverges,
#      so Poisson GLM is the PRIMARY model (NB retained as sensitivity analysis).
#      This resolves Reviewer 5's point that Poisson AIC was lower than NB.
#   7. Full pairwise comparison table (emmeans Tukey) and per-pair Cliff's delta
#      exported to CSV for supplemental table.
#   8. Primary scenarios (seed 42) and mask adherence sensitivity (seed 123) now
#      share the same baseline parameterization, so Fig 1/2 baseline aligns with
#      the primary analysis baseline (addresses Reviewer 6 point 1).
#   9. Wait time added to tornado sensitivity (was missing).
#  10. All outputs write to C:/Users/mbucala/Downloads/{figures,tables};
#      no hardcoded setwd().
#  11. All key numbers printed to console with labeled headers.
# =============================================================================


# =============================================================================
# 1. SETUP
# =============================================================================
suppressPackageStartupMessages({
  library(simmer)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(MASS)       # NOTE: masks dplyr::select; use dplyr::select() explicitly
  library(emmeans)
  library(effsize)
  library(tidyr)
  library(broom)
  library(scales)
})

# -----------------------------------------------------------------------------
# Output location — all figures and tables write to Downloads
# -----------------------------------------------------------------------------
out_root    <- "C:/Users/mbucala/Downloads"
out_figures <- file.path(out_root, "figures")
out_tables  <- file.path(out_root, "tables")
if (!dir.exists(out_root))    dir.create(out_root,    recursive = TRUE)
if (!dir.exists(out_figures)) dir.create(out_figures, recursive = TRUE)
if (!dir.exists(out_tables))  dir.create(out_tables,  recursive = TRUE)

# Console helper
hdr <- function(x) {
  cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
}
sub_hdr <- function(x) {
  cat("\n", strrep("-", 60), "\n", x, "\n", strrep("-", 60), "\n", sep = "")
}


# =============================================================================
# 2. GLOBAL PARAMETERS (aligned to manuscript + supplemental)
# =============================================================================
params <- list(
  # Environmental
  room_seating_capacity   = 10,      # seats (single well-mixed compartment)
  air_changes_per_hour    = 6,       # ACH baseline; ASHRAE 170 healthcare standard
  virus_airborne_duration = 120,     # minutes; measles aerosol stability / WHO guidance

  # Clinic flow
  simulation_duration     = 480,     # 8-hour clinic day (minutes)
  arrival_rate            = 12/60,   # 12 patients/hour => lambda = 0.2/minute (Poisson)
  mean_wait_time          = 30,      # minutes (exponential)

  # Epidemiologic
  p_unvaccinated          = 0.08,    # US national ~92% MMR coverage
  mask_adherence          = 0.0,     # proportion masked (0-1), continuous
  mask_effectiveness      = 0.60,    # surgical mask combined source + personal protection
  transmission_prob       = 0.25     # per-contact baseline probability
)

hdr("PARAMETER SET (matches manuscript & supplemental)")
print(params)


# =============================================================================
# 3. TRANSMISSION PROBABILITY MODEL
#    P_transmission = p_base * f_vent(ACH) * f_mask
#    f_vent = exp(-0.05 * ACH)
#    f_mask = 1 - (adherence * effectiveness)
# =============================================================================
adjusted_trans_prob <- function(mask_adherence = params$mask_adherence,
                                air_changes    = params$air_changes_per_hour,
                                p) {
  base       <- p$transmission_prob
  f_vent     <- exp(-0.05 * air_changes)
  f_mask     <- 1 - (mask_adherence * p$mask_effectiveness)
  prob       <- base * f_vent * f_mask
  # Guard rails
  prob <- pmin(pmax(prob, 0), 1)
  return(prob)
}

# Verification table matching supplemental "Example Calculations"
hdr("TRANSMISSION PROBABILITY VERIFICATION (Supplemental Table)")
example_calcs <- tibble(
  scenario      = c("Baseline", "Masking only", "Ventilation only", "Bundled"),
  ACH           = c(6, 6, 12, 12),
  mask_adh      = c(0.0, 1.0, 0.0, 1.0),
  f_vent        = exp(-0.05 * ACH),
  f_mask        = 1 - (mask_adh * params$mask_effectiveness),
  P_transmission = params$transmission_prob * f_vent * f_mask
)
print(example_calcs, digits = 3)


# =============================================================================
# 4. SIMULATION ENGINE (simmer discrete-event)
# =============================================================================
patient_trajectory <- trajectory("patient") |>
  seize("seat", 1) |>
  timeout(function() rexp(1, 1 / params$mean_wait_time)) |>
  release("seat", 1)

create_clinic_model <- function(p) {
  env <- simmer("clinic", verbose = FALSE)
  env |>
    add_resource("seat", capacity = p$room_seating_capacity) |>
    add_generator("patient", patient_trajectory,
                  function() rexp(1, p$arrival_rate))
  return(env)
}

# -----------------------------------------------------------------------------
# run_scenario: one iteration = one 8-hour clinic day
#   (1) Poisson arrivals to a 10-seat waiting room
#   (2) One randomly designated index case among arrivals (infectious throughout)
#   (3) Each remaining agent drawn as unvaccinated via Bernoulli(p_unvax)
#   (4) Each unvaccinated agent exposed via Bernoulli(P_transmission)
# -----------------------------------------------------------------------------
run_scenario <- function(p, runs = 500) {
  replicate(runs, {
    env <- create_clinic_model(p)
    env |> run(until = p$simulation_duration)
    arrivals     <- get_mon_arrivals(env)
    n_agents     <- nrow(arrivals)
    # One index case per day; remaining are potential contacts
    exposed      <- max(0, n_agents - 1)
    # Agent-level vaccination assignment
    n_unvax      <- rbinom(1, exposed, p$p_unvaccinated)
    # Agent-level transmission draw
    inf_prob     <- adjusted_trans_prob(p$mask_adherence,
                                        p$air_changes_per_hour, p)
    infected     <- rbinom(1, n_unvax, inf_prob)
    return(infected)
  })
}


# =============================================================================
# 5. PRIMARY INTERVENTION SCENARIOS  (n = 500 each, seed = 42)
# =============================================================================
hdr("PRIMARY SCENARIOS (n=500 per arm, seed=42)")

set.seed(42)
params_baseline    <- params
params_masking     <- modifyList(params, list(mask_adherence = 1.0))
params_ventilation <- modifyList(params, list(air_changes_per_hour = 12))
params_bundled     <- modifyList(params, list(mask_adherence = 1.0,
                                              air_changes_per_hour = 12,
                                              mean_wait_time = 10))

# Run sequentially so a single seed governs the full primary analysis
res_baseline    <- run_scenario(params_baseline,    runs = 500)
res_masking     <- run_scenario(params_masking,     runs = 500)
res_ventilation <- run_scenario(params_ventilation, runs = 500)
res_bundled     <- run_scenario(params_bundled,     runs = 500)

scenario_results <- bind_rows(
  tibble(scenario = "baseline",    infected = res_baseline),
  tibble(scenario = "masking",     infected = res_masking),
  tibble(scenario = "ventilation", infected = res_ventilation),
  tibble(scenario = "bundled",     infected = res_bundled)
) |>
  mutate(scenario = factor(scenario,
                           levels = c("baseline", "ventilation",
                                      "masking",  "bundled")))

# Descriptive summary (feeds the Results section)
scenario_summary <- scenario_results |>
  group_by(scenario) |>
  summarise(
    n          = dplyr::n(),
    mean       = round(mean(infected), 2),
    sd         = round(sd(infected), 2),
    median     = median(infected),
    q25        = quantile(infected, 0.25),
    q75        = quantile(infected, 0.75),
    max        = max(infected),
    pct_zero   = round(mean(infected == 0) * 100, 1),
    pct_95th   = quantile(infected, 0.95),
    .groups    = "drop"
  ) |>
  mutate(
    pct_reduction_vs_baseline = round(
      (mean[scenario == "baseline"] - mean) /
        mean[scenario == "baseline"] * 100, 1)
  )

sub_hdr("Descriptive summary by scenario")
print(scenario_summary)
write.csv(scenario_summary, file.path(out_tables, "table_s1_scenario_summary.csv"), row.names = FALSE)


# =============================================================================
# 6. STATISTICAL MODELING
#    Note from validation run: the binomial-of-binomial data-generating process
#    produces approximately Poisson counts (Pearson dispersion ~1.0, NB theta
#    diverges, AIC favors Poisson). We therefore use Poisson GLM as the PRIMARY
#    model and retain NB as a sensitivity analysis. This also addresses
#    Reviewer 5's objection that "Poisson has lower AIC" than NB.
#
#    (a) Overdispersion check -> justifies Poisson for these data
#    (b) Poisson regression (primary)
#    (c) Tukey-adjusted pairwise comparisons via emmeans
#    (d) Non-parametric Kruskal-Wallis + Wilcoxon with Bonferroni (sensitivity)
#    (e) Cliff's delta for each scenario vs baseline + each pairwise comparison
# =============================================================================
hdr("STATISTICAL ANALYSIS")

# (a) Overdispersion check (Poisson is valid if dispersion ~ 1)
sub_hdr("Overdispersion diagnostics (Poisson vs Negative Binomial)")
poisson_model <- glm(infected ~ scenario, family = poisson, data = scenario_results)
nb_model      <- suppressWarnings(glm.nb(infected ~ scenario, data = scenario_results))

# Pearson dispersion statistic: values >>1 indicate overdispersion
pearson_disp <- sum(residuals(poisson_model, type = "pearson")^2) /
                poisson_model$df.residual
cat(sprintf("Poisson Pearson dispersion statistic: %.3f (>1 indicates overdispersion)\n",
            pearson_disp))

# Likelihood ratio test NB vs Poisson
lr_stat <- 2 * (as.numeric(logLik(nb_model)) - as.numeric(logLik(poisson_model)))
lr_pval <- pchisq(max(lr_stat, 0), df = 1, lower.tail = FALSE)
cat(sprintf("LR test (NB vs Poisson): chi-sq = %.3f, p = %.4g\n",
            lr_stat, lr_pval))

aic_tbl <- AIC(poisson_model, nb_model)
print(aic_tbl)
cat("\nInterpretation: Poisson preferred if dispersion ~ 1, NB theta diverges,\n",
    "or LR test not significant. In our simulation data the binomial-of-binomial\n",
    "generating process does not exhibit overdispersion, so Poisson is the\n",
    "appropriate primary model.\n", sep = "")
cat("NB theta:", round(nb_model$theta, 3),
    "| SE:", round(nb_model$SE.theta, 3),
    "(large theta -> NB collapses to Poisson)\n")

# Define the PRIMARY model for all downstream inference
primary_model <- poisson_model

# (b) Poisson regression coefficient table (primary)
sub_hdr("Poisson regression coefficients (reference: baseline)")
poisson_coefs <- tidy(primary_model, conf.int = TRUE, exponentiate = TRUE)
print(poisson_coefs, digits = 3)
write.csv(poisson_coefs, file.path(out_tables, "table_s2_poisson_coefficients.csv"),
          row.names = FALSE)

# NB sensitivity (kept for supplemental)
nb_coefs <- tidy(nb_model, conf.int = TRUE, exponentiate = TRUE)
write.csv(nb_coefs,
          file.path(out_tables, "table_s2b_nb_sensitivity_coefficients.csv"),
          row.names = FALSE)

# (c) Pairwise comparisons (Tukey-adjusted)
sub_hdr("Pairwise comparisons (emmeans, Tukey-adjusted, log scale)")
emm       <- emmeans(primary_model, ~ scenario)
emm_pairs <- pairs(emm, adjust = "tukey")
emm_pairs_tbl <- as_tibble(summary(emm_pairs, infer = TRUE))
print(emm_pairs_tbl, digits = 3)

# Back-transform to rate ratios for interpretation
sub_hdr("Pairwise rate ratios (back-transformed)")
emm_rr <- pairs(emm, adjust = "tukey", type = "response")
emm_rr_tbl <- as_tibble(summary(emm_rr, infer = TRUE))
print(emm_rr_tbl, digits = 3)

# (d) Non-parametric sensitivity
sub_hdr("Non-parametric tests")
kw <- kruskal.test(infected ~ scenario, data = scenario_results)
cat(sprintf("Kruskal-Wallis: chi-sq = %.2f, df = %d, p = %.4g\n",
            kw$statistic, kw$parameter, kw$p.value))
wilcox_mat <- pairwise.wilcox.test(scenario_results$infected,
                                   scenario_results$scenario,
                                   p.adjust.method = "bonferroni",
                                   exact = FALSE)
print(wilcox_mat)

# (e) Cliff's delta for every scenario vs baseline AND every pairwise comparison
sub_hdr("Cliff's delta (all pairwise comparisons)")
levs <- levels(scenario_results$scenario)
pair_grid <- expand.grid(group1 = levs, group2 = levs,
                         stringsAsFactors = FALSE) |>
  filter(group1 != group2) |>
  # keep only unique unordered pairs (lower triangle)
  mutate(key = pmin(group1, group2), key2 = pmax(group1, group2)) |>
  distinct(key, key2) |>
  rename(group1 = key, group2 = key2)

cliff_tbl <- pair_grid |>
  rowwise() |>
  mutate(
    d_obj = list(cliff.delta(
      scenario_results$infected[scenario_results$scenario == group1],
      scenario_results$infected[scenario_results$scenario == group2])),
    estimate   = d_obj$estimate,
    lower_ci   = d_obj$conf.int[1],
    upper_ci   = d_obj$conf.int[2],
    magnitude  = as.character(d_obj$magnitude)
  ) |>
  ungroup() |>
  dplyr::select(-d_obj)

print(cliff_tbl, digits = 3)
write.csv(cliff_tbl, file.path(out_tables, "table_s3_cliffs_delta.csv"), row.names = FALSE)

# Convenience: key effect sizes called out in the abstract
sub_hdr("Key effect sizes for abstract")
key_abs <- cliff_tbl |>
  filter((group1 == "baseline" & group2 == "masking") |
         (group1 == "baseline" & group2 == "ventilation") |
         (group1 == "baseline" & group2 == "bundled"))
print(key_abs, digits = 3)


# =============================================================================
# 7. ASSEMBLE FULL PAIRWISE TABLE FOR SUPPLEMENT
#    Joins GLM rate ratios + Tukey p-values + Cliff's delta in one table.
#    Note: emmeans returns lower.CL/upper.CL for Poisson (t-based CI) and
#    asymp.LCL/asymp.UCL for NB (z-based CI). We normalize to ci_lower/ci_upper.
#    Cliff's delta is computed in the same orientation as each emmeans contrast
#    to guarantee alignment.
# =============================================================================
sub_hdr("Assembled pairwise comparison table (for supplemental)")

# Normalize CI columns (handle Poisson vs NB naming)
emm_rr_norm <- emm_rr_tbl
if ("asymp.LCL" %in% names(emm_rr_norm)) {
  emm_rr_norm <- emm_rr_norm |>
    rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)
}

nb_pairs <- emm_rr_norm |>
  dplyr::select(contrast, ratio, SE, lower.CL, upper.CL, p.value) |>
  rename(rate_ratio   = ratio,
         rr_lower_ci  = lower.CL,
         rr_upper_ci  = upper.CL,
         p_tukey      = p.value) |>
  mutate(contrast = as.character(contrast))

# Parse each emmeans contrast into its two groups (handles both "a - b" and
# "a / b" orientations) and compute Cliff's delta in that exact direction.
parse_contrast <- function(s) {
  parts <- strsplit(s, "\\s*[-/]\\s*")[[1]]
  list(group1 = trimws(parts[1]), group2 = trimws(parts[2]))
}

full_pairs <- nb_pairs |>
  rowwise() |>
  mutate(
    .parsed = list(parse_contrast(contrast)),
    group1  = .parsed$group1,
    group2  = .parsed$group2,
    .cd     = list(cliff.delta(
      scenario_results$infected[scenario_results$scenario == group1],
      scenario_results$infected[scenario_results$scenario == group2])),
    cliffs_delta = .cd$estimate,
    cd_lower_ci  = .cd$conf.int[1],
    cd_upper_ci  = .cd$conf.int[2],
    cd_magnitude = as.character(.cd$magnitude)
  ) |>
  ungroup() |>
  dplyr::select(contrast, rate_ratio, rr_lower_ci, rr_upper_ci, p_tukey,
                cliffs_delta, cd_lower_ci, cd_upper_ci, cd_magnitude)

print(full_pairs, digits = 3)
write.csv(full_pairs, file.path(out_tables, "table_s4_full_pairwise.csv"), row.names = FALSE)


# =============================================================================
# 8. MASK ADHERENCE SENSITIVITY ANALYSIS  (for Figures 1/2)
#    Seed 123; same baseline parameters as the primary analysis so that the
#    adherence = 0% point matches the primary baseline within sampling variation.
# =============================================================================
hdr("MASK ADHERENCE SENSITIVITY (seed=123, n=200 per level)")

set.seed(123)
adherence_grid <- seq(0, 1, by = 0.10)
adherence_results <- lapply(adherence_grid, function(a) {
  p <- modifyList(params, list(mask_adherence = a))
  run_scenario(p, runs = 200)
})

adherence_summary <- tibble(
  adherence   = adherence_grid,
  mean_inf    = sapply(adherence_results, mean),
  sd_inf      = sapply(adherence_results, sd),
  pct_zero    = sapply(adherence_results, function(x) mean(x == 0) * 100)
) |>
  mutate(
    pct_reduction = round((mean_inf[adherence == 0] - mean_inf) /
                           mean_inf[adherence == 0] * 100, 1)
  )

print(adherence_summary, digits = 3)
write.csv(adherence_summary, file.path(out_tables, "table_s5_adherence_sensitivity.csv"), row.names = FALSE)


# =============================================================================
# 9. ONE-WAY TORNADO SENSITIVITY  (seed=456, n=200 per level)
#    Parameters: ACH, wait time, unvaccinated proportion, mask adherence
# =============================================================================
hdr("ONE-WAY TORNADO SENSITIVITY (seed=456, n=200 per level)")

set.seed(456)
run_tornado <- function(param_name, values) {
  sapply(values, function(val) {
    p <- params
    p[[param_name]] <- val
    mean(run_scenario(p, runs = 200))
  })
}

ach_range  <- seq(2, 15, by = 1)
wait_range <- c(5, 10, 15, 20, 30, 45, 60)
vax_range  <- seq(0.05, 0.50, by = 0.05)   # unvaccinated proportion
mask_range <- seq(0, 1, by = 0.10)

ach_sens  <- tibble(parameter = "air_changes_per_hour",
                    value = ach_range,
                    mean_infected = run_tornado("air_changes_per_hour", ach_range))
wait_sens <- tibble(parameter = "mean_wait_time",
                    value = wait_range,
                    mean_infected = run_tornado("mean_wait_time", wait_range))
vax_sens  <- tibble(parameter = "p_unvaccinated",
                    value = vax_range,
                    mean_infected = run_tornado("p_unvaccinated", vax_range))
mask_sens <- tibble(parameter = "mask_adherence",
                    value = mask_range,
                    mean_infected = run_tornado("mask_adherence", mask_range))

tornado_df <- bind_rows(ach_sens, wait_sens, vax_sens, mask_sens)
sub_hdr("Tornado sensitivity results")
print(tornado_df, n = Inf, digits = 3)
write.csv(tornado_df, file.path(out_tables, "table_s6_tornado_sensitivity.csv"), row.names = FALSE)


# =============================================================================
# 10. EXTENDED UNVACCINATED SWEEP (50-95%)
#     Matches the range reported in manuscript Results (7.88 at 50% -> 15.95 at 95%)
# =============================================================================
hdr("EXTENDED UNVACCINATED SWEEP (seed=456, 50-95%, n=200)")

unvax_wide <- seq(0.50, 0.95, by = 0.05)
unvax_results <- tibble(
  p_unvaccinated = unvax_wide,
  mean_infected  = run_tornado("p_unvaccinated", unvax_wide)
)
print(unvax_results, digits = 3)
write.csv(unvax_results, file.path(out_tables, "table_s7_unvax_sweep.csv"), row.names = FALSE)


# =============================================================================
# 11. MANUSCRIPT NUMBER CHECK
#     Print the exact numeric claims made in the abstract and main text so that
#     the revised manuscript can be audited line-by-line against this output.
# =============================================================================
hdr("MANUSCRIPT NUMBER CHECK (reconcile text vs code)")

baseline_row    <- scenario_summary |> filter(scenario == "baseline")
masking_row     <- scenario_summary |> filter(scenario == "masking")
ventilation_row <- scenario_summary |> filter(scenario == "ventilation")
bundled_row     <- scenario_summary |> filter(scenario == "bundled")

cat("\nABSTRACT / RESULTS BASELINE:\n")
cat(sprintf("  mean = %.2f (SD %.2f), median = %.1f (IQR %.1f-%.1f), max = %d, %%zero = %.1f%%\n",
            baseline_row$mean, baseline_row$sd, baseline_row$median,
            baseline_row$q25, baseline_row$q75, baseline_row$max,
            baseline_row$pct_zero))

cat("\nUNIVERSAL MASKING:\n")
cat(sprintf("  mean = %.2f (SD %.2f), median = %.1f (IQR %.1f-%.1f), max = %d, %%zero = %.1f%%, reduction = %.1f%%\n",
            masking_row$mean, masking_row$sd, masking_row$median,
            masking_row$q25, masking_row$q75, masking_row$max,
            masking_row$pct_zero, masking_row$pct_reduction_vs_baseline))

cat("\nENHANCED VENTILATION:\n")
cat(sprintf("  mean = %.2f (SD %.2f), median = %.1f (IQR %.1f-%.1f), max = %d, %%zero = %.1f%%, reduction = %.1f%%\n",
            ventilation_row$mean, ventilation_row$sd, ventilation_row$median,
            ventilation_row$q25, ventilation_row$q75, ventilation_row$max,
            ventilation_row$pct_zero,
            ventilation_row$pct_reduction_vs_baseline))

cat("\nBUNDLED:\n")
cat(sprintf("  mean = %.2f (SD %.2f), median = %.1f (IQR %.1f-%.1f), max = %d, %%zero = %.1f%%, reduction = %.1f%%\n",
            bundled_row$mean, bundled_row$sd, bundled_row$median,
            bundled_row$q25, bundled_row$q75, bundled_row$max,
            bundled_row$pct_zero, bundled_row$pct_reduction_vs_baseline))

cat("\nMASK ADHERENCE (Figures 1/2 source data):\n")
ma_60  <- adherence_summary |> filter(adherence == 0.6)
ma_100 <- adherence_summary |> filter(adherence == 1.0)
ma_0   <- adherence_summary |> filter(adherence == 0.0)
cat(sprintf("  0%%  adherence: mean = %.2f\n",  ma_0$mean_inf))
cat(sprintf("  60%% adherence: mean = %.2f, reduction = %.1f%%\n",
            ma_60$mean_inf, ma_60$pct_reduction))
cat(sprintf("  100%% adherence: mean = %.2f, reduction = %.1f%%, %%zero = %.1f%%\n",
            ma_100$mean_inf, ma_100$pct_reduction, ma_100$pct_zero))

cat("\nKEY P-VALUES (from Poisson regression, Tukey-adjusted):\n")
pval_masking_vs_base  <- emm_pairs_tbl |>
  filter(grepl("baseline", contrast) & grepl("masking", contrast))
pval_vent_vs_base     <- emm_pairs_tbl |>
  filter(grepl("baseline", contrast) & grepl("ventilation", contrast))
pval_bundled_vs_mask  <- emm_pairs_tbl |>
  filter(grepl("bundled", contrast) & grepl("masking", contrast))
cat(sprintf("  masking vs baseline:     p = %.4g\n",
            pval_masking_vs_base$p.value))
cat(sprintf("  ventilation vs baseline: p = %.4g\n",
            pval_vent_vs_base$p.value))
cat(sprintf("  bundled vs masking:      p = %.4g\n",
            pval_bundled_vs_mask$p.value))

cat("\nKEY CLIFF'S DELTA VALUES (for abstract/results):\n")
print(key_abs |>
        dplyr::select(group1, group2, estimate, lower_ci, upper_ci, magnitude),
      digits = 3)


# =============================================================================
# 12. FIGURE GENERATION
#     Addresses Reviewer 6:
#       - Fig 2 dropped (redundant with Fig 1); means now annotated on Fig 1.
#       - theme_classic() removes gray gridlines.
#       - Fig 3: %zero annotations above zero-infection bars.
#       - Fig 4: separated bar clusters via facet_wrap (cleaner than dodged bars).
# =============================================================================
hdr("GENERATING FIGURES")

scenario_colors <- c(baseline    = "#D62728",
                     ventilation = "#1F77B4",
                     masking     = "#2CA02C",
                     bundled     = "#9467BD")

# ---- Figure 1 (was Fig 1+2): Mask adherence dose-response with mean labels ----
fig1 <- ggplot(adherence_summary, aes(x = adherence * 100, y = mean_inf)) +
  geom_hline(yintercept = adherence_summary$mean_inf[1],
             linetype = "dashed", color = "gray40") +
  annotate("text",
           x = 85, y = adherence_summary$mean_inf[1] + 0.05,
           label = sprintf("Baseline (no masking): %.2f",
                           adherence_summary$mean_inf[1]),
           color = "gray30", size = 3.5, hjust = 0) +
  geom_line(color = "#2CA02C", linewidth = 1) +
  geom_point(color = "#2CA02C", size = 3) +
  geom_text(aes(label = sprintf("%.2f", mean_inf)),
            vjust = -1.0, size = 3) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Figure 1. Impact of Mask Adherence on Measles Transmission",
       subtitle = "Mean secondary infections decrease with increasing mask adherence (n=200 sims/level)",
       x = "Mask Adherence (%)",
       y = "Mean Secondary Infections",
       caption = "Each point represents 200 independent simulations of 8-hour clinic days.") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_figures, "fig1_mask_doseresponse.png"), fig1,
       width = 9, height = 5.5, dpi = 300)

# ---- Figure 2 (was Fig 3): Per-scenario histograms, %zero annotated ----
fig2_df <- scenario_results |>
  mutate(scenario_label = recode(scenario,
                                 baseline    = "Baseline",
                                 ventilation = "Enhanced Ventilation",
                                 masking     = "Universal Masking",
                                 bundled     = "Bundled"))
# Compute zero-infection labels per facet
zero_labels <- fig2_df |>
  group_by(scenario_label) |>
  summarise(pct_zero = mean(infected == 0) * 100,
            .groups  = "drop") |>
  mutate(label = sprintf("%.1f%% zero", pct_zero),
         x = 0, y = Inf)

fig2 <- ggplot(fig2_df, aes(x = infected, fill = scenario_label)) +
  geom_histogram(binwidth = 1, color = "black", boundary = -0.5) +
  geom_text(data = zero_labels,
            aes(x = x, y = y, label = label),
            vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ scenario_label, ncol = 2) +
  scale_fill_manual(values = c("Baseline" = "#D62728",
                               "Enhanced Ventilation" = "#1F77B4",
                               "Universal Masking"    = "#2CA02C",
                               "Bundled"              = "#9467BD")) +
  scale_x_continuous(breaks = 0:7) +
  labs(title = "Figure 2. Secondary Infection Distribution by Intervention Strategy",
       subtitle = "n = 500 simulations per scenario; 8-hour clinic day",
       x = "Number of Secondary Infections",
       y = "Frequency (Number of Simulations)",
       caption = "Histograms with integer bins appropriately represent discrete count data.") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold"))

ggsave(file.path(out_figures, "fig2_scenario_histogram.png"), fig2,
       width = 9, height = 7, dpi = 300)

# ---- Figure 3 (was Fig 4): Faceted comparison (replaces dodged bars) ----
# Reviewer 6 requested separation between the bar groupings; faceting by count
# bin is cleaner than forcing dodge spacing.
fig3 <- ggplot(fig2_df, aes(x = scenario_label, fill = scenario_label)) +
  geom_bar(color = "black") +
  facet_wrap(~ infected, nrow = 1,
             labeller = labeller(infected = function(x) paste0(x, " inf."))) +
  scale_fill_manual(values = c("Baseline" = "#D62728",
                               "Enhanced Ventilation" = "#1F77B4",
                               "Universal Masking"    = "#2CA02C",
                               "Bundled"              = "#9467BD")) +
  labs(title = "Figure 3. Comparison of Secondary Infection Distributions Across Scenarios",
       subtitle = "Each panel shows the count of simulations producing that many secondary infections",
       x = NULL,
       y = "Frequency (Number of Simulations)",
       fill = "Scenario") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(out_figures, "fig3_histogram_comparison.png"), fig3,
       width = 11, height = 5.5, dpi = 300)

# ---- Figure 4 (was Fig 5): ECDF without gridlines ----
ecdf_df <- fig2_df
fig4 <- ggplot(ecdf_df, aes(x = infected, color = scenario_label)) +
  stat_ecdf(linewidth = 1.2, geom = "step") +
  scale_color_manual(values = c("Baseline" = "#D62728",
                                "Enhanced Ventilation" = "#1F77B4",
                                "Universal Masking"    = "#2CA02C",
                                "Bundled"              = "#9467BD")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 0:7) +
  labs(title = "Figure 4. Cumulative Distribution of Infections by Scenario",
       subtitle = "Higher/leftward curves indicate better intervention performance",
       x = "Number of Secondary Infections",
       y = "Cumulative Proportion of Simulations",
       color = "Scenario") +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(out_figures, "fig4_ecdf.png"), fig4, width = 9, height = 5.5, dpi = 300)

# ---- Figure 5 (was Fig 6): Tornado sensitivity, no gridlines ----
tornado_plot_df <- tornado_df |>
  mutate(parameter_label = recode(parameter,
                                  air_changes_per_hour = "Air Changes per Hour (ACH)",
                                  mean_wait_time       = "Mean Wait Time (min)",
                                  mask_adherence       = "Mask Adherence Rate",
                                  p_unvaccinated       = "Proportion Unvaccinated"))

fig5 <- ggplot(tornado_plot_df, aes(x = value, y = mean_infected, color = parameter_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ parameter_label, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("Air Changes per Hour (ACH)" = "#1F77B4",
                                "Mean Wait Time (min)"       = "#FF7F0E",
                                "Mask Adherence Rate"         = "#2CA02C",
                                "Proportion Unvaccinated"     = "#D62728"),
                     guide = "none") +
  labs(title = "Figure 5. Sensitivity of Secondary Infections to Key Parameters",
       subtitle = "One-way sensitivity analysis; n = 200 simulations per parameter value",
       x = "Parameter Value",
       y = "Mean Secondary Infections") +
  theme_classic(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(out_figures, "fig5_tornado_sensitivity.png"), fig5,
       width = 13, height = 4.5, dpi = 300)

cat("\nFigures saved to ", out_figures, ":\n",
    "  fig1_mask_doseresponse.png\n",
    "  fig2_scenario_histogram.png\n",
    "  fig3_histogram_comparison.png\n",
    "  fig4_ecdf.png\n",
    "  fig5_tornado_sensitivity.png\n", sep = "")

cat("\nTables saved to ", out_tables, ":\n",
    "  table_s1_scenario_summary.csv\n",
    "  table_s2_nb_coefficients.csv\n",
    "  table_s3_cliffs_delta.csv\n",
    "  table_s4_full_pairwise.csv\n",
    "  table_s5_adherence_sensitivity.csv\n",
    "  table_s6_tornado_sensitivity.csv\n",
    "  table_s7_unvax_sweep.csv\n", sep = "")

hdr("SCRIPT COMPLETE")
cat("All numeric results printed above can be audited against the manuscript.\n")
cat("Run date:", as.character(Sys.Date()), "\n")
cat("Session info:\n")
print(sessionInfo())
