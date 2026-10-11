# =====================================================================
# Source-language preservation in multilingual LLM translation:
# analysis code
#
# Reproduces the three statistical models (SM1-SM3), inter-rater
# reliability, the appendix tables and the paper figures from the
# released, anonymised annotations in data/.
#
# Run from the repository root:
#   Rscript analysis.R            full run (several hours)
#   QUICK=1 Rscript analysis.R    pipeline check with short chains
#                                 (minutes; results are NOT for reporting)
#
# Outputs go to output/ (or output_quick/):
#   tables/     CSV tables (D1_ ... D5_ match Appendix D.1-D.5)
#   figures/    fig_*.pdf paper figures; *.png other plots
#   summaries/  model summaries and contrasts as text
#   fits/       cached brms fits (reloaded on re-runs unless the
#               data or model change)
# =====================================================================

library(tidyverse)        # dplyr, tidyr, ggplot2, stringr, forcats, readr, purrr
library(brms)
library(posterior)
library(bayesplot)
library(tidybayes)
library(emmeans)
library(marginaleffects)
library(scales)
library(patchwork)
library(irrCAC)
library(flextable)
library(officer)


# =====================================================================
# 0. Settings
# =====================================================================

DATA_FILE      <- "data/annotations.csv"
EXCLUSIONS_LOG <- "data/exclusions_log.csv"

QUICK   <- identical(Sys.getenv("QUICK"), "1")
OUT_DIR <- if (QUICK) "output_quick" else "output"

SEED          <- 2026         # used by set.seed() below and by every brm() call
set.seed(SEED)
CHAINS        <- if (QUICK) 2 else 4
CORES         <- min(CHAINS, parallel::detectCores())
ITER          <- if (QUICK) 300 else 4000   # 12,000 post-warmup draws in the full run
WARMUP        <- if (QUICK) 150 else 1000
FILE_REFIT    <- "on_change"  # "always" forces refitting; "never" always reloads
SUMMARY_DRAWS <- if (QUICK) 200 else 4000   # draws used for large derived summaries

EXPECTED <- c(languages = 15, models = 7, segments = 104, raters = 75)

QUALITY_LEVELS <- c("serious_failures", "imperfect_not_terrible",
                    "mostly_good_small_issues", "very_good_nearly_perfect")

# The first level is the SM3 reference category
ENGLISH_LEVELS <- c("english_culturally_appropriate",
                    "good_translations_exist_english_hurts",
                    "no_good_translation_english_best")
HARM_DPAR   <- "mugoodtranslationsexistenglishhurts"
NOGOOD_DPAR <- "munogoodtranslationenglishbest"

# Model names as printed in Table 1 (used in tables and figures only)
MODEL_LABELS <- c("Deepseek V3.1" = "DeepSeek V3.1",
                  "gpt-oss 120b"  = "gpt-oss 120B")
pretty_model <- function(x) unname(recode(as.character(x), !!!MODEL_LABELS))

# PDF device that embeds fonts: Quartz on macOS (no XQuartz needed),
# cairo elsewhere. Base pdf() is a last resort and does not embed fonts.
PDF_DEVICE <- if (capabilities("aqua")) {
  function(filename, ...) grDevices::quartz(type = "pdf", file = filename, ...)
} else if (capabilities("cairo")) {
  cairo_pdf
} else {
  warning("No Quartz or cairo device: PDF figures will not embed fonts.")
  pdf
}

for (d in c("tables", "figures", "summaries", "fits"))
  dir.create(file.path(OUT_DIR, d), recursive = TRUE, showWarnings = FALSE)
tab_path   <- function(f) file.path(OUT_DIR, "tables", f)
fig_path   <- function(f) file.path(OUT_DIR, "figures", f)
sum_path   <- function(f) file.path(OUT_DIR, "summaries", f)
save_table <- function(x, f) { write_csv(x, tab_path(f)); invisible(x) }
# Text output (model summaries, contrasts): written to summaries/ and shown
save_summary <- function(x, f) {
  out <- capture.output(print(x))
  writeLines(out, sum_path(f))
  cat(out, sep = "\n")
  invisible(x)
}
# Plots: written to figures/ and shown when running interactively
save_plot <- function(p, f, width = 8, height = 6) {
  ggsave(fig_path(f), p, width = width, height = height, units = "in", bg = "white")
  if (interactive()) print(p)
  invisible(p)
}
# Paper figures: saved at the size they print in a two-panel ACL figure
save_paper_figure <- function(p, f, width = 3.1, height = 3) {
  ggsave(fig_path(f), p, width = width, height = height, units = "in",
         device = PDF_DEVICE)
  invisible(p)
}
# Posterior predictive check, saved as a PNG and as a small PDF panel
save_pp_check <- function(fit, name) {
  set.seed(SEED)
  save_plot(pp_check(fit, type = "bars", ndraws = 100), paste0(name, "_pp_check.png"), 6, 4)
  set.seed(SEED)
  p <- pp_check(fit, type = "bars", ndraws = 100) +
    theme(text = element_text(size = 8), legend.position = "bottom")
  ggsave(fig_path(paste0(name, "_pp_check_panel.pdf")), p,
         width = 2.1, height = 2.1, device = PDF_DEVICE)
}


# =====================================================================
# 1. Load the anonymised annotations
#    Exclusions were applied before release (Appendix D.1;
#    data/exclusions_log.csv). See data/README.md for the codebook.
# =====================================================================

dat <- read_csv(DATA_FILE, col_types = cols(.default = col_character())) |>
  rename(source.Language = language, source.model = model,
         source.email_number = email_number)

stopifnot(all(dat$segment_quality %in% c(QUALITY_LEVELS, "not_translated")))

dat <- dat |>
  mutate(
    was_translated  = factor(if_else(segment_quality == "not_translated",
                                     "not_translated", "translated"),
                             levels = c("not_translated", "translated")),
    quality_numeric = match(segment_quality, QUALITY_LEVELS),  # 1-4; NA if retained
    # Appropriateness applies only to retained segments
    english_cat     = if_else(was_translated == "not_translated" &
                                best_if_english %in% ENGLISH_LEVELS,
                              best_if_english, NA_character_)
  )

counts <- c(languages = n_distinct(dat$source.Language),
            models    = n_distinct(dat$source.model),
            segments  = n_distinct(dat$segment_id),
            raters    = n_distinct(dat$rater_id))
print(counts)
if (!all(counts == EXPECTED[names(counts)]))
  stop("Design counts differ from EXPECTED: is data/annotations.csv complete?")

segment_lookup <- dat |>
  distinct(segment_id, segment_english, email_number = source.email_number)
stopifnot(nrow(segment_lookup) == counts[["segments"]])

# ---- Analysis subsets ----
YesTranslated <- dat |>
  filter(was_translated == "translated") |>
  mutate(quality_ord = factor(quality_numeric, levels = 1:4, ordered = TRUE))

NotTranslated <- dat |> filter(was_translated == "not_translated")

# SM3 uses retained segments that have an appropriateness response
NotTranslated_rated <- NotTranslated |>
  filter(!is.na(english_cat)) |>
  mutate(english_cat = factor(english_cat, levels = ENGLISH_LEVELS))


# =====================================================================
# 2. Descriptive statistics (Appendix D.1)
# =====================================================================

# Exclusions were applied before release; their counts ship with the data
if (file.exists(EXCLUSIONS_LOG)) {
  exclusion_log <- read_csv(EXCLUSIONS_LOG, show_col_types = FALSE)
  save_table(exclusion_log, "D1_exclusions.csv")
  print(exclusion_log)
}

analytic_n <- tibble(
  sample = c("Design: segments x models x raters",
             "SM1: all judgements",
             "SM2: translated segments",
             "Retained (not translated) segments",
             "SM3: retained segments with an appropriateness response"),
  n = c(prod(EXPECTED[c("segments", "models", "raters")]),
        nrow(dat), nrow(YesTranslated), nrow(NotTranslated),
        nrow(NotTranslated_rated))
)
save_table(analytic_n, "D1_analytic_n.csv")
print(analytic_n)

raw_summary <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      n              = n(),
      pct_translated = 100 * mean(was_translated == "translated"),
      mean_quality   = mean(quality_numeric, na.rm = TRUE),   # 1-4, translated only
      n_retained     = sum(was_translated == "not_translated"),
      n_rated        = sum(!is.na(english_cat)),
      pct_culturally_appropriate =
        100 * sum(english_cat == ENGLISH_LEVELS[1], na.rm = TRUE) / n_rated,
      pct_harmful =
        100 * sum(english_cat == ENGLISH_LEVELS[2], na.rm = TRUE) / n_rated,
      pct_no_good_translation =
        100 * sum(english_cat == ENGLISH_LEVELS[3], na.rm = TRUE) / n_rated,
      .groups = "drop"
    )
}

raw_overall     <- raw_summary(dat)
raw_by_language <- raw_summary(dat, source.Language) |> arrange(desc(pct_harmful))
raw_by_model    <- raw_summary(dat, source.model) |>
  mutate(source.model = pretty_model(source.model)) |>
  arrange(desc(pct_translated))
raw_by_segment  <- raw_summary(dat, segment_id) |>
  left_join(segment_lookup, by = "segment_id") |>
  relocate(segment_english, email_number, .after = segment_id) |>
  arrange(pct_translated)

save_table(raw_overall,     "D1_raw_overall.csv")
save_table(raw_by_language, "D1_raw_by_language.csv")
save_table(raw_by_model,    "D1_raw_by_model.csv")
save_table(raw_by_segment,  "D1_raw_by_segment.csv")
print(raw_by_language, n = Inf)
print(raw_by_model)



# =====================================================================
# 3. Modelling helpers
# =====================================================================

# Sum coding with named contrasts (main effects = deviations from the
# grand mean). contr.sum leaves out the LAST level of each factor, so
# its coefficient is not printed by summary(); grand_mean_effects()
# below recovers every level, including that one.
sum_code <- function(d) {
  d <- as.data.frame(d)
  for (v in c("source.Language", "source.model")) {
    d[[v]] <- droplevels(factor(d[[v]]))
    contrasts(d[[v]]) <- contr.sum(nlevels(d[[v]]))
    colnames(contrasts(d[[v]])) <- levels(d[[v]])[-nlevels(d[[v]])]
  }
  d
}

fit_brm <- function(name, formula, data, family, ...) {
  brm(formula, data = data, family = family,
      chains = CHAINS, cores = CORES, iter = ITER, warmup = WARMUP,
      seed = SEED,
      file = file.path(OUT_DIR, "fits", name), file_refit = FILE_REFIT,
      ...)
}

# Convergence summary for Appendix D.3
diagnose <- function(fit, name) {
  s  <- summarise_draws(as_draws_df(fit), "rhat", "ess_bulk", "ess_tail") |>
    filter(!variable %in% c("lp__", "lprior"))   # model parameters only
  np <- nuts_params(fit)
  tibble(
    model            = name,
    n_obs            = nobs(fit),
    draws            = ndraws(fit),
    max_rhat         = max(s$rhat, na.rm = TRUE),
    worst_rhat_param = s$variable[which.max(s$rhat)],
    n_rhat_over_1.01 = sum(s$rhat > 1.01, na.rm = TRUE),
    min_ess_bulk     = min(s$ess_bulk, na.rm = TRUE),
    min_ess_tail     = min(s$ess_tail, na.rm = TRUE),
    divergent        = sum(np$Value[np$Parameter == "divergent__"]),
    max_treedepth    = max(np$Value[np$Parameter == "treedepth__"]),
    hit_treedepth_10 = sum(np$Value[np$Parameter == "treedepth__"] >= 10)
  )
}

summarise_value <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(estimate = mean(.value),
              lower    = quantile(.value, 0.025),
              upper    = quantile(.value, 0.975),
              .groups  = "drop") |>
    mutate(credible = lower > 0 | upper < 0)
}

strip_level <- function(x) str_trim(str_remove_all(as.character(x), "^\\(|\\)$"))

# Each level's deviation from the grand mean, with 95% CrI, for ALL levels
# (on the linear-predictor scale: log-odds for SM1 and SM3, latent probit for SM2)
grand_mean_effects <- function(fit, factor, ...) {
  emmeans(fit, specs = factor, ...) |>
    contrast(method = "eff") |>
    gather_emmeans_draws() |>
    summarise_value(contrast) |>
    mutate(level = strip_level(str_remove(contrast, " effect$")), .before = 1) |>
    select(-contrast)
}

# Language x model interaction terms (departure from additivity) for all 105 cells
interaction_effects <- function(fit, ...) {
  emmeans(fit, ~ source.Language * source.model, ...) |>
    contrast(interaction = c("eff", "eff")) |>
    gather_emmeans_draws() |>
    summarise_value(source.Language_eff, source.model_eff) |>
    transmute(language = strip_level(str_remove(source.Language_eff, " effect$")),
              model    = pretty_model(strip_level(str_remove(source.model_eff, " effect$"))),
              estimate, lower, upper, credible)
}

sd_table <- function(fit) {
  as_tibble(posterior_summary(fit, variable = "^sd_", regex = TRUE),
            rownames = "parameter")
}

# By-segment random intercepts, labelled with the segment text
segment_effects <- function(fit, par = "Intercept") {
  re <- ranef(fit)$segment_id
  tibble(segment_id = dimnames(re)[[1]],
         estimate   = re[, "Estimate", par],
         lower      = re[, "Q2.5",     par],
         upper      = re[, "Q97.5",    par]) |>
    left_join(segment_lookup, by = "segment_id") |>
    relocate(segment_english, email_number, .after = segment_id)
}

# Split emmeans contrast labels "A - B" (levels may be wrapped in parentheses)
split_pair <- function(x) {
  p <- str_split_fixed(as.character(x), " - ", 2)
  tibble(a = strip_level(p[, 1]), b = strip_level(p[, 2]))
}

text_colour_for <- function(values, limits) {
  hex <- col_numeric(viridisLite::plasma(256), domain = limits)(values)
  rgb <- col2rgb(hex) / 255
  ifelse(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ] > 0.5, "black", "white")
}

# Language x model heatmap. paper = TRUE gives print-size text for the
# two-panel ACL figure (save with save_paper_figure()).
heatmap_lang_model <- function(d, value, legend_title, paper = FALSE) {
  rng <- range(d[[value]])
  d$text_col <- text_colour_for(d[[value]], rng)
  ggplot(d, aes(x = pretty_model(source.model), y = source.Language,
                fill = .data[[value]])) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", .data[[value]]), colour = text_col),
              size = if (paper) 1.8 else 2.5) +
    scale_fill_viridis_c(option = "plasma", limits = rng, name = legend_title) +
    scale_colour_identity() +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = if (paper) 7 else 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid  = element_blank()) +
    if (paper) theme(legend.position   = "top",
                     legend.key.height = unit(0.2, "cm"),
                     legend.key.width  = unit(0.9, "cm"))
}




# Pairwise contrast heatmap (* = 95% CrI excludes 0)
pairwise_heatmap <- function(emm, level_order, title, axis_label,
                             reverse = FALSE, fill_name = "Difference\n(A − B)",
                             labeller = identity) {
  d <- pairs(emm, reverse = reverse) |>
    gather_emmeans_draws() |>
    summarise_value(contrast)
  d <- bind_cols(d, split_pair(d$contrast)) |>
    mutate(a = factor(a, levels = level_order),
           b = factor(b, levels = level_order))
  ggplot(d, aes(x = b, y = a, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%+.2f%s", estimate, ifelse(credible, "*", ""))),
              size = 2.5) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, name = fill_name) +
    scale_x_discrete(drop = FALSE, labels = labeller) +
    scale_y_discrete(drop = FALSE, labels = labeller) +
    labs(x = paste(axis_label, "B"), y = paste(axis_label, "A"),
         title = title, subtitle = "* = 95% credible interval excludes 0") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

diagnostics <- list()
priors_used <- list()


# =====================================================================
# 4. SM1: was the segment translated or retained in English?
#    Bernoulli (logit); positive = more likely translated
# =====================================================================

sm1_data <- sum_code(dat)

sm1 <- fit_brm(
  "sm1_occurrence",
  was_translated ~ source.Language * source.model +
    (1 | segment_id) + (1 | rater_id),
  data = sm1_data, family = bernoulli()
)

save_summary(summary(sm1), "sm1_summary.txt")
diagnostics$SM1 <- diagnose(sm1, "SM1")
priors_used$SM1 <- as_tibble(prior_summary(sm1)) |> mutate(model = "SM1")
print(diagnostics$SM1)

# ---- Appendix tables ----
sm1_fixef     <- as_tibble(fixef(sm1), rownames = "parameter")
sm1_sd        <- sd_table(sm1)
sm1_model_eff <- grand_mean_effects(sm1, "source.model") |>
  mutate(level = pretty_model(level)) |> arrange(desc(estimate))
sm1_lang_eff  <- grand_mean_effects(sm1, "source.Language") |> arrange(desc(estimate))
sm1_int_eff   <- interaction_effects(sm1)
sm1_segments  <- segment_effects(sm1) |>
  left_join(raw_by_segment |> select(segment_id, n, pct_translated), by = "segment_id") |>
  arrange(estimate)

save_table(sm1_fixef,     "D4_sm1_fixed_effects_printed.csv")
save_table(sm1_sd,        "D4_sm1_random_effect_sds.csv")
save_table(sm1_model_eff, "D4_sm1_model_effects.csv")
save_table(sm1_lang_eff,  "D4_sm1_language_effects.csv")
save_table(sm1_int_eff,   "D4_sm1_interaction_effects.csv")
save_table(sm1_segments,  "D4_sm1_segment_intercepts.csv")
print(sm1_model_eff)
print(sm1_lang_eff, n = Inf)
print(sm1_segments, n = Inf)

# ---- Estimated marginal means (probability scale) ----
emm_lang_p    <- emmeans(sm1, ~ source.Language, epred = TRUE)
emm_model_p   <- emmeans(sm1, ~ source.model,    epred = TRUE)
pairs_lang_p  <- pairs(emm_lang_p)
pairs_model_p <- pairs(emm_model_p)
save_summary(emm_model_p,   "sm1_emm_models_probability.txt")
save_summary(emm_lang_p,    "sm1_emm_languages_probability.txt")
save_summary(pairs_model_p, "sm1_pairwise_models_probability.txt")
save_summary(pairs_lang_p,  "sm1_pairwise_languages_probability.txt")

# ---- Plots ----
p <- sm1 |>
  gather_draws(`b_.*`, regex = TRUE) |>
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = 2)
save_plot(p, "sm1_coefficients.png", 8, 24)

p <- gather_emmeans_draws(emm_model_p) |>
  ggplot(aes(x = .value, y = pretty_model(source.model))) +
  stat_halfeye(.width = c(0.66, 0.95)) +
  labs(x = "P(translated)", y = NULL, title = "Model") +
  theme_minimal()
save_plot(p, "sm1_emm_models.png", 7, 5)

p <- gather_emmeans_draws(emm_lang_p) |>
  ggplot(aes(x = .value, y = source.Language)) +
  stat_halfeye(.width = c(0.66, 0.95)) +
  labs(x = "P(translated)", y = NULL, title = "Language") +
  theme_minimal()
save_plot(p, "sm1_emm_languages.png", 7, 7)

p <- gather_emmeans_draws(pairs_lang_p) |>
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(.width = c(0.66, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in P(translated)", y = NULL,
       title = "Language pairwise differences") +
  theme_minimal()
save_plot(p, "sm1_pairwise_languages_intervals.png", 8, 24)

p <- gather_emmeans_draws(pairs_model_p) |>
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(.width = c(0.66, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in P(translated)", y = NULL,
       title = "Model pairwise differences") +
  theme_minimal()
save_plot(p, "sm1_pairwise_models_intervals.png", 8, 7)

p <- pairwise_heatmap(emm_lang_p, levels(sm1_data$source.Language),
                      "Pairwise differences between languages, P(translated)",
                      "Language", reverse = TRUE, fill_name = "Diff in\nP(translated)")
save_plot(p, "sm1_pairwise_languages.png", 9, 8)

p <- pairwise_heatmap(emm_model_p, levels(sm1_data$source.model),
                      "Pairwise differences between models, P(translated)",
                      "Model", reverse = TRUE, fill_name = "Diff in\nP(translated)",
                      labeller = pretty_model)
save_plot(p, "sm1_pairwise_models.png", 7, 6)

# ---- Language x model heatmap (paper figure, panel a) ----
sm1_cells <- emmeans(sm1, ~ source.Language * source.model, epred = TRUE) |>
  as.data.frame() |>
  as_tibble() |>
  rename(estimate = emmean) |>
  mutate(source.Language = fct_reorder(source.Language, estimate, mean))
save_table(sm1_cells |> mutate(source.model = pretty_model(source.model)),
           "D4_sm1_cell_probabilities.csv")

save_plot(heatmap_lang_model(sm1_cells, "estimate", "P(translated)"),
          "sm1_heatmap_screen.png", 8, 7)
save_paper_figure(heatmap_lang_model(sm1_cells, "estimate", "P(translated)", paper = TRUE),
                  "fig_sm1_translated_heatmap.pdf")



# ---- Conditional effects: language x model ----
ce <- conditional_effects(sm1, effects = "source.Language:source.model",
                          re_formula = NA)[["source.Language:source.model"]]
p <- ggplot(ce, aes(x = source.Language, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.2) +
  facet_wrap(~ pretty_model(source.model), ncol = 4) +
  coord_flip() +
  theme_minimal() +
  labs(y = "P(translated)", x = "Language")
save_plot(p, "sm1_conditional_effects.png", 10, 10)

save_pp_check(sm1, "sm1")


# =====================================================================
# 5. SM2: translation quality, given the segment was translated
#    Cumulative probit on the 1-4 scale; positive = higher quality
# =====================================================================

sm2_data <- sum_code(YesTranslated)

sm2 <- fit_brm(
  "sm2_quality",
  quality_ord ~ source.Language * source.model +
    (1 | segment_id) + (1 | rater_id),
  data = sm2_data, family = cumulative(link = "probit")
)

save_summary(summary(sm2), "sm2_summary.txt")
diagnostics$SM2 <- diagnose(sm2, "SM2")
priors_used$SM2 <- as_tibble(prior_summary(sm2)) |> mutate(model = "SM2")
print(diagnostics$SM2)

# ---- Appendix tables ----
# brms's emmeans support for ordinal models ignores the thresholds, so
# these are latent-scale contrasts; the thresholds cancel in contrasts.
sm2_fixef      <- as_tibble(fixef(sm2), rownames = "parameter")
sm2_sd         <- sd_table(sm2)
sm2_thresholds <- as_tibble(posterior_summary(sm2, variable = "^b_Intercept", regex = TRUE),
                            rownames = "parameter")
sm2_model_eff  <- grand_mean_effects(sm2, "source.model") |>
  mutate(level = pretty_model(level)) |> arrange(desc(estimate))
sm2_lang_eff   <- grand_mean_effects(sm2, "source.Language") |> arrange(desc(estimate))
sm2_int_eff    <- interaction_effects(sm2)
sm2_segments   <- segment_effects(sm2) |> arrange(estimate)

save_table(sm2_fixef,      "D4_sm2_fixed_effects_printed.csv")
save_table(sm2_sd,         "D4_sm2_random_effect_sds.csv")
save_table(sm2_thresholds, "D4_sm2_thresholds.csv")
save_table(sm2_model_eff,  "D4_sm2_model_effects.csv")
save_table(sm2_lang_eff,   "D4_sm2_language_effects.csv")
save_table(sm2_int_eff,    "D4_sm2_interaction_effects.csv")
save_table(sm2_segments,   "D4_sm2_segment_intercepts.csv")
print(sm2_model_eff)
print(sm2_lang_eff, n = Inf)

emm_model_q <- emmeans(sm2, ~ source.model)
emm_lang_q  <- emmeans(sm2, ~ source.Language)
save_summary(emm_model_q,        "sm2_emm_models_latent.txt")
save_summary(emm_lang_q,         "sm2_emm_languages_latent.txt")
save_summary(pairs(emm_model_q), "sm2_pairwise_models_latent.txt")
save_summary(pairs(emm_lang_q),  "sm2_pairwise_languages_latent.txt")

p <- sm2 |>
  gather_draws(`b_source.*`, regex = TRUE) |>
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = 2)
save_plot(p, "sm2_coefficients.png", 8, 24)

# ---- Expected quality (1-4) by language x model (paper figure, panel b) ----
newdat <- expand_grid(source.Language = levels(sm2_data$source.Language),
                      source.model    = levels(sm2_data$source.model))
set.seed(SEED)                                       # fixes which draws are subsampled
pe <- posterior_epred(sm2, newdata = newdat, re_formula = NA,
                      ndraws = SUMMARY_DRAWS)        # draws x cells x categories
expected_draws <- Reduce(`+`, lapply(seq_len(dim(pe)[3]), function(k) pe[, , k] * k))

sm2_cells <- newdat |>
  mutate(estimate = colMeans(expected_draws),
         lower    = apply(expected_draws, 2, quantile, 0.025),
         upper    = apply(expected_draws, 2, quantile, 0.975),
         source.Language = fct_reorder(source.Language, estimate, mean))
save_table(sm2_cells |> mutate(source.model = pretty_model(source.model)),
           "D4_sm2_cell_expected_quality.csv")

save_plot(heatmap_lang_model(sm2_cells, "estimate", "Expected\nquality"),
          "sm2_heatmap_screen.png", 8, 7)
save_paper_figure(heatmap_lang_model(sm2_cells, "estimate", "Expected quality", paper = TRUE),
                  "fig_sm2_quality_heatmap.pdf")

# ---- Category probabilities per language ----
ce <- conditional_effects(sm2, effects = "source.Language",
                          re_formula = NA, categorical = TRUE)[["source.Language:cats__"]]
p <- ggplot(ce, aes(x = source.Language, y = estimate__, fill = cats__)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma", name = "Quality level") +
  theme_minimal() +
  labs(y = "Probability", x = "Language")
save_plot(p, "sm2_category_probabilities.png", 8, 6)

# ---- Pairwise contrasts (latent probit scale) ----
p <- pairwise_heatmap(emm_model_q, levels(sm2_data$source.model),
                      "Pairwise model contrasts in quality (A − B)", "Model",
                      labeller = pretty_model)
save_plot(p, "sm2_pairwise_models.png", 7, 6)

p <- pairwise_heatmap(emm_lang_q, levels(sm2_data$source.Language),
                      "Pairwise language contrasts in quality (A − B)", "Language")
save_plot(p, "sm2_pairwise_languages.png", 9, 8)

save_pp_check(sm2, "sm2")


# =====================================================================
# 6. SM3: was retaining English appropriate? (retained segments only)
#    Categorical (multinomial logit). Reference category:
#    english_culturally_appropriate. "Harmful" = good translations
#    exist and English hurts.
# =====================================================================

save_summary(table(NotTranslated$best_if_english, useNA = "ifany"), "sm3_response_counts.txt")
save_summary(prop.table(table(NotTranslated_rated$english_cat)),    "sm3_response_shares.txt")

sm3_data <- sum_code(NotTranslated_rated)

sm3_priors <- c(
  prior(normal(0, 2), class = "b", dpar = "mugoodtranslationsexistenglishhurts"),
  prior(normal(0, 2), class = "b", dpar = "munogoodtranslationenglishbest")
)

sm3 <- fit_brm(
  "sm3_appropriateness",
  english_cat ~ source.Language * source.model +
    (1 | segment_id) + (1 | rater_id),
  data = sm3_data, family = categorical(link = "logit"),
  prior = sm3_priors, control = list(adapt_delta = 0.95)
)

save_summary(summary(sm3), "sm3_summary.txt")
diagnostics$SM3 <- diagnose(sm3, "SM3")
priors_used$SM3 <- as_tibble(prior_summary(sm3)) |> mutate(model = "SM3")
print(diagnostics$SM3)

# ---- Appendix tables: log-odds vs the reference category ----
sm3_fixef   <- as_tibble(fixef(sm3), rownames = "parameter")
sm3_sd      <- sd_table(sm3)
sm3_effects <- map_dfr(
  c(harmful_vs_culturally_appropriate             = HARM_DPAR,
    no_good_translation_vs_culturally_appropriate = NOGOOD_DPAR),
  function(dp) bind_rows(
    grand_mean_effects(sm3, "source.model", dpar = dp) |>
      mutate(factor = "model", level = pretty_model(level)),
    grand_mean_effects(sm3, "source.Language", dpar = dp) |>
      mutate(factor = "language")
  ),
  .id = "comparison"
)
sm3_segments <- bind_rows(
  segment_effects(sm3, paste0(HARM_DPAR, "_Intercept")) |>
    mutate(comparison = "harmful_vs_culturally_appropriate"),
  segment_effects(sm3, paste0(NOGOOD_DPAR, "_Intercept")) |>
    mutate(comparison = "no_good_translation_vs_culturally_appropriate")
) |>
  left_join(raw_by_segment |> select(segment_id, n_rated, pct_harmful), by = "segment_id") |>
  arrange(comparison, desc(estimate))

save_table(sm3_fixef,    "D4_sm3_fixed_effects_printed.csv")
save_table(sm3_sd,       "D4_sm3_random_effect_sds.csv")
save_table(sm3_effects,  "D4_sm3_effects.csv")
save_table(sm3_segments, "D4_sm3_segment_intercepts.csv")
print(sm3_segments |> filter(comparison == "harmful_vs_culturally_appropriate"), n = Inf)

# ---- Predicted probabilities for every language x model cell ----
# Population level (re_formula = NA): a segment and rater with average
# random intercepts. These run above the raw shares because brand names,
# codes and places supply most retentions but are a minority of segments.
nd <- sm3_data |> distinct(source.Language, source.model)
sm3_draws <- epred_draws(sm3, newdata = nd, re_formula = NA,
                         ndraws = SUMMARY_DRAWS, seed = SEED) |>
  ungroup()

all_pairs <- sm3_draws |>
  group_by(source.model, .category) |>
  compare_levels(.epred, by = source.Language) |>
  group_by(source.model, .category, source.Language) |>   # source.Language holds "A - B"
  summarise(est   = median(.epred),
            lower = quantile(.epred, 0.025),
            upper = quantile(.epred, 0.975),
            p_dir = mean(.epred > 0),
            .groups = "drop")
save_table(all_pairs |> mutate(source.model = pretty_model(source.model)),
           "D4_sm3_language_pairs_within_model.csv")
print(all_pairs, n = 50)

# ---- Probability that a retention was judged harmful ----
# Harmful vs. appropriate (the other two categories), averaged over the
# other factor within each draw. Reports mean and median: check which the
# paper should cite.
marginal_harm <- function(by) {
  sm3_draws |>
    filter(.category == ENGLISH_LEVELS[2]) |>
    group_by(across(all_of(c(".draw", by)))) |>
    summarise(p = mean(.epred), .groups = "drop")
}
summarise_harm <- function(d, by) {
  d |>
    group_by(across(all_of(by))) |>
    summarise(p_harmful_mean   = mean(p),
              p_harmful_median = median(p),
              lower            = quantile(p, 0.025),
              upper            = quantile(p, 0.975),
              .groups = "drop")
}

harm_lang_draws  <- marginal_harm("source.Language")
harm_model_draws <- marginal_harm("source.model")

harm_by_language <- summarise_harm(harm_lang_draws, "source.Language") |>
  left_join(raw_by_language |> select(source.Language, n_rated, raw_pct_harmful = pct_harmful),
            by = "source.Language") |>
  arrange(desc(p_harmful_median))
harm_by_model <- summarise_harm(harm_model_draws, "source.model") |>
  mutate(source.model = pretty_model(source.model)) |>
  left_join(raw_by_model |> select(source.model, n_rated, raw_pct_harmful = pct_harmful),
            by = "source.model") |>
  arrange(desc(p_harmful_median))

save_table(harm_by_language, "D4_sm3_p_harmful_by_language.csv")
save_table(harm_by_model,    "D4_sm3_p_harmful_by_model.csv")
print(harm_by_language, n = Inf)
print(harm_by_model)

p <- ggplot(harm_by_language,
            aes(x = p_harmful_median, y = fct_reorder(source.Language, p_harmful_median))) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  linewidth = 0.5, size = 0.4, colour = "#9a3324") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "P(retention judged harmful: a good translation existed)",
       y = NULL,
       title = "Harmful retention by language",
       subtitle = "Posterior median and 95% CrI, typical segment, averaged over the seven models") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title.position = "plot")
save_plot(p, "sm3_harm_by_language.png", 7, 5)

harm_pairs <- harm_lang_draws |>
  compare_levels(p, by = source.Language) |>
  group_by(source.Language) |>                         # holds the "A - B" label
  summarise(diff  = median(p),
            lower = quantile(p, 0.025),
            upper = quantile(p, 0.975),
            p_dir = mean(p > 0),
            .groups = "drop") |>
  mutate(credible = lower > 0 | upper < 0) |>
  arrange(desc(abs(diff)))
save_table(harm_pairs, "D4_sm3_harm_language_pairs.csv")
print(harm_pairs, n = 30)

# ---- Conditional effects: language, by model ----
conds <- make_conditions(sm3, vars = "source.model")
p <- plot(conditional_effects(sm3, effects = "source.Language", conditions = conds,
                              categorical = TRUE, re_formula = NA),
          plot = FALSE)[[1]]
save_plot(p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
          "sm3_conditional_effects_brms.png", 14, 8)

ce <- conditional_effects(sm3, effects = "source.Language", conditions = conds,
                          categorical = TRUE, re_formula = NA)[[1]]
p <- ggplot(ce, aes(x = fct_reorder(source.Language, estimate__), y = estimate__,
                    colour = pretty_model(source.model))) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.2,
                  position = position_dodge(width = 0.6)) +
  facet_wrap(~ cats__, scales = "free_x") +
  coord_flip() +
  labs(x = NULL, y = "Predicted probability", colour = "Model") +
  theme_minimal(base_size = 12) +
  theme(panel.spacing = unit(1, "lines"),
        strip.text = element_text(face = "bold", size = 9))
save_plot(p, "sm3_conditional_effects_by_model.png", 12, 7)

save_pp_check(sm3, "sm3")

# ---- Three-panel heatmap: every outcome, language x model ----
preds <- predictions(
  sm3,
  type       = "response",
  newdata    = datagrid(source.Language = unique, source.model = unique),
  re_formula = NA                  # population level, as in the other figures
) |>
  as.data.frame()

lang_order <- preds |>
  filter(group != "no_good_translation_english_best") |>
  group_by(source.Language) |>
  summarise(rank_val = max(estimate), .groups = "drop") |>
  arrange(rank_val) |>
  pull(source.Language)

preds <- preds |>
  mutate(
    source.Language = factor(source.Language, levels = lang_order),
    source.model    = unname(abbreviate(pretty_model(source.model), minlength = 10)),
    group = recode(group,
                   "english_culturally_appropriate"        = "English culturally appropriate",
                   "good_translations_exist_english_hurts" = "Good translations exist, English hurts",
                   "no_good_translation_english_best"      = "No good translation, English best")
  )

make_panel <- function(d, ttl, fill_cap, drop_y = FALSE) {
  p <- ggplot(d, aes(source.model, source.Language, fill = estimate)) +
    geom_tile(color = "grey90", linewidth = 0.3) +
    geom_text(aes(label = ifelse(estimate >= 0.005, sprintf("%.2f", estimate), ""),
                  color = estimate > 0.5 * fill_cap),
              size = 2.6, show.legend = FALSE) +
    scale_fill_viridis_c(name = "P(outcome)", limits = c(0, fill_cap), oob = squish) +
    scale_color_manual(values = c("FALSE" = "white", "TRUE" = "grey15"), guide = "none") +
    labs(title = ttl, x = "Model", y = "Language") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid  = element_blank(),
          plot.title  = element_text(face = "bold", size = 10),
          legend.position = "right")
  if (drop_y)
    p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  p
}

# cap each panel near its own maximum so one outlier doesn't flatten the rest
cap_for <- function(g) max(0.05, quantile(preds$estimate[preds$group == g], 0.98, names = FALSE))

g1 <- "English culturally appropriate"
g2 <- "Good translations exist, English hurts"
g3 <- "No good translation, English best"

p <- (make_panel(filter(preds, group == g1), g1, cap_for(g1)) +
      make_panel(filter(preds, group == g2), g2, cap_for(g2), drop_y = TRUE) +
      make_panel(filter(preds, group == g3), g3, cap_for(g3), drop_y = TRUE)) +
  plot_annotation(
    title    = "Predicted probability of each outcome",
    subtitle = "Typical segment and rater; each panel has its own colour scale"
  )
save_plot(p, "sm3_outcome_heatmaps.png", 14, 6)


# =====================================================================
# 7. Inter-rater reliability (Appendix D.5)
#    Krippendorff's alpha + Gwet's AC1/AC2 for each outcome, overall and
#    by language, model and language x model.
#
#    Raters are nested within language, so pooled matrices are sparse by
#    design; both coefficients use only the raters who rated each item.
# =====================================================================

irr_sm1 <- dat |> mutate(was_translated_num = as.integer(was_translated == "translated"))
irr_sm2 <- YesTranslated
irr_sm3 <- NotTranslated_rated |> mutate(english_cat = as.character(english_cat))

# Units x raters matrix. The item key includes language so items never
# collide when strata pool across languages.
build_matrix <- function(d, value_col) {
  d <- d |> mutate(item = paste(segment_id, source.model, source.Language, sep = "__"))
  dups <- sum(duplicated(d[c("item", "rater_id")]))
  if (dups > 0) warning(dups, " duplicate item x rater rows dropped")
  d |>
    distinct(item, rater_id, .keep_all = TRUE) |>
    select(item, rater_id, value = all_of(value_col)) |>
    pivot_wider(names_from = rater_id, values_from = value) |>
    column_to_rownames("item") |>
    as.matrix()
}

# irrCAC returns conf.int as a string like "(0.227,0.358)"
parse_ci <- function(ci_string) {
  nums <- suppressWarnings(as.numeric(str_match(ci_string, "\\(([^,]+),([^)]+)\\)")[, 2:3]))
  c(lower = nums[1], upper = nums[2])
}

compute_irr <- function(m, kripp_w, gwet_w, min_items = 5) {
  m <- m[, colSums(!is.na(m)) > 0, drop = FALSE]
  m <- m[rowSums(!is.na(m)) >= 2, , drop = FALSE]

  out <- tibble(n_items = nrow(m), n_raters = ncol(m),
                kripp_alpha = NA_real_, kripp_lci = NA_real_, kripp_uci = NA_real_,
                gwet_ac = NA_real_, gwet_lci = NA_real_, gwet_uci = NA_real_,
                note = NA_character_)

  if (nrow(m) < min_items || ncol(m) < 2) {
    out$note <- "insufficient data"
    return(out)
  }

  ka <- tryCatch(krippen.alpha.raw(m, weights = kripp_w)$est, error = function(e) NULL)
  ga <- tryCatch(gwet.ac1.raw(m, weights = gwet_w)$est,       error = function(e) NULL)

  if (!is.null(ka)) {
    ci <- parse_ci(ka$conf.int)
    out$kripp_alpha <- ka$coeff.val; out$kripp_lci <- ci[1]; out$kripp_uci <- ci[2]
  }
  if (!is.null(ga)) {
    ci <- parse_ci(ga$conf.int)
    out$gwet_ac <- ga$coeff.val; out$gwet_lci <- ci[1]; out$gwet_uci <- ci[2]
  }
  if (is.null(ka) || is.null(ga)) out$note <- "estimation failed"
  out
}

irr_strata <- function(df, value_col, kripp_w, gwet_w, group_vars = NULL, min_items = 5) {
  if (length(group_vars) == 0) {
    return(compute_irr(build_matrix(df, value_col), kripp_w, gwet_w, min_items) |>
             mutate(stratum_type = "Overall (pooled)", stratum = "Overall", .before = 1))
  }
  keys <- df |> distinct(across(all_of(group_vars))) |> arrange(across(everything()))
  map_dfr(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    sub <- df |> semi_join(key, by = group_vars)
    compute_irr(build_matrix(sub, value_col), kripp_w, gwet_w, min_items) |>
      mutate(stratum_type = paste(group_vars, collapse = " x "),
             stratum      = paste(unlist(key), collapse = " | "),
             .before = 1)
  })
}

run_all_strata <- function(df, value_col, kripp_w, gwet_w, outcome_name, min_items = 5) {
  bind_rows(
    irr_strata(df, value_col, kripp_w, gwet_w, NULL,              min_items),
    irr_strata(df, value_col, kripp_w, gwet_w, "source.Language", min_items),
    irr_strata(df, value_col, kripp_w, gwet_w, "source.model",    min_items),
    irr_strata(df, value_col, kripp_w, gwet_w, c("source.Language", "source.model"), min_items)
  ) |>
    mutate(outcome = outcome_name, .before = 1)
}

irr_all <- bind_rows(
  run_all_strata(irr_sm1, "was_translated_num", "unweighted", "unweighted",
                 "Was translated (binary)"),
  run_all_strata(irr_sm2, "quality_numeric",    "ordinal",    "quadratic",
                 "Quality (ordinal, given translated)"),
  run_all_strata(irr_sm3, "english_cat",        "unweighted", "unweighted",
                 "English appropriateness (nominal, given retained)")
)

save_summary(irr_all |> count(outcome, stratum_type), "irr_strata_counts.txt")
save_summary(irr_all |> filter(!is.na(note)),         "irr_skipped_strata.txt")

# n-weighted rollup of the per-language rows, for comparison with the pooled row
overall_weighted <- irr_all |>
  filter(stratum_type == "source.Language", is.na(note)) |>
  group_by(outcome) |>
  summarise(stratum_type = "Overall (n-weighted from languages)",
            stratum      = "Overall",
            kripp_alpha  = weighted.mean(kripp_alpha, n_items),
            gwet_ac      = weighted.mean(gwet_ac,     n_items),
            n_items_tot  = sum(n_items),
            n_raters = NA_real_, kripp_lci = NA_real_, kripp_uci = NA_real_,
            gwet_lci = NA_real_, gwet_uci = NA_real_, note = NA_character_,
            .groups = "drop") |>
  rename(n_items = n_items_tot)

irr_full <- bind_rows(irr_all, overall_weighted) |>
  mutate(stratum_type = factor(stratum_type,
                               levels = c("Overall (pooled)",
                                          "Overall (n-weighted from languages)",
                                          "source.Language", "source.model",
                                          "source.Language x source.model"))) |>
  arrange(outcome, stratum_type, stratum)

# Median and range of alpha across language x model cells, per outcome
irr_cell_range <- irr_full |>
  filter(stratum_type == "source.Language x source.model", is.na(note)) |>
  group_by(outcome) |>
  summarise(median_alpha = median(kripp_alpha), min_alpha = min(kripp_alpha),
            max_alpha = max(kripp_alpha), .groups = "drop")
print(irr_cell_range)

fmt_est <- function(x) ifelse(is.na(x), "—", sprintf("%.2f", x))
fmt_ci  <- function(l, u) ifelse(is.na(l), "—", sprintf("[%.2f, %.2f]", l, u))

irr_clean <- irr_full |>
  mutate(stratum = if_else(stratum_type == "source.model",
                           pretty_model(stratum), stratum),
         `Krippendorff's α` = fmt_est(kripp_alpha),
         `α 95% CI`         = fmt_ci(kripp_lci, kripp_uci),
         `Gwet's AC`        = fmt_est(gwet_ac),
         `AC 95% CI`        = fmt_ci(gwet_lci, gwet_uci),
         `N items`          = n_items,
         `N raters`         = ifelse(is.na(n_raters), "—", as.character(n_raters)),
         Stratification     = as.character(stratum_type)) |>
  select(Outcome = outcome, Stratification, Stratum = stratum, `N items`, `N raters`,
         `Krippendorff's α`, `α 95% CI`, `Gwet's AC`, `AC 95% CI`)

irr_main  <- irr_clean |> filter(Stratification != "source.Language x source.model")
irr_cells <- irr_clean |>
  filter(Stratification == "source.Language x source.model") |>
  separate(Stratum, into = c("Language", "Model"), sep = " \\| ") |>
  mutate(Model = pretty_model(Model)) |>
  select(-Stratification)

print(irr_main,  n = Inf)
print(irr_cells, n = 20)

ft_main <- flextable(irr_main) |>
  merge_v(j = c("Outcome", "Stratification")) |>
  bold(i = ~ grepl("^Overall", Stratification)) |>
  bg(i = ~ grepl("^Overall", Stratification), bg = "#f2f2f2") |>
  align(align = "center", part = "all") |>
  align(j = c("Outcome", "Stratification", "Stratum"), align = "left", part = "all") |>
  autofit() |>
  set_caption("Inter-rater reliability by outcome, overall and by language and model")

ft_cells <- flextable(irr_cells) |>
  merge_v(j = c("Outcome", "Language")) |>
  align(align = "center", part = "all") |>
  align(j = c("Outcome", "Language", "Model"), align = "left", part = "all") |>
  autofit() |>
  set_caption("Inter-rater reliability within each language × model cell")

read_docx() |>
  body_add_par("Inter-rater reliability", style = "heading 1") |>
  body_add_flextable(ft_main) |>
  body_add_par("", style = "Normal") |>
  body_add_par("Language × model cells", style = "heading 2") |>
  body_add_flextable(ft_cells) |>
  print(target = tab_path("D5_IRR_tables.docx"))

save_table(irr_full,       "D5_IRR_all_strata_raw.csv")
save_table(irr_cell_range, "D5_IRR_cell_range.csv")


# =====================================================================
# 8. Specification, diagnostics and software (Appendix D.2-D.3)
# =====================================================================

diagnostics <- bind_rows(diagnostics)
save_table(diagnostics, "D3_diagnostics.csv")
print(diagnostics)
if (any(diagnostics$max_rhat > 1.01) || any(diagnostics$min_ess_bulk < 400))
  warning("Some R-hat > 1.01 or bulk ESS < 400: increase ITER before reporting.")
if (any(diagnostics$divergent > 0))
  warning("Divergent transitions present: see D3_diagnostics.csv.")

save_table(bind_rows(priors_used), "D2_priors.csv")
save_table(tibble(setting = c("chains", "iter", "warmup", "post-warmup draws", "seed",
                              "SM3 adapt_delta"),
                  value   = c(CHAINS, ITER, WARMUP, CHAINS * (ITER - WARMUP), SEED, 0.95)),
           "D2_sampler_settings.csv")
writeLines(capture.output(sessionInfo()), tab_path("D2_session_info.txt"))
