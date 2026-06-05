knitr::opts_chunk$set(echo = F, message = F, warning = F, fig.retina = 4)

# Libraries
pacman::p_load(haven, estimatr, texreg, janitor, tidyverse, skimr, compareGroups, progress, data.table, Matrix)

# load dataset (observations are at the student-course level)
data_csv <- Sys.getenv(
  "GENDER_DATA_CSV",
  unset = file.path("..", "data", "my_data_simulated.csv")
)

if (!file.exists(data_csv)) {
  stop(
    "Input data not found. Set GENDER_DATA_CSV or place my_data_simulated.csv in ../data/."
  )
}

output_dir <- Sys.getenv(
  "GENDER_OUTPUT_DIR",
  unset = file.path("..", "output")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

df <-
  read_csv(data_csv, na = c("", "NA"), show_col_types = FALSE) %>%
  mutate(
    b_achievement_z = rowMeans(select(., b_math_g1_score, b_physics_g1_score), na.rm = TRUE) %>% scale() %>% as.vector,
    b_anxiety_z = rowMeans(select(., b_math_anxiety, b_physics_anxiety), na.rm = TRUE) %>% scale() %>% as.vector,
    b_confidence_z = rowMeans(select(., b_math_confidence, b_physics_confidence), na.rm = TRUE) %>% scale() %>% as.vector
  )

# Student-level slice (one row per student) – used for descriptives, overlaps,
# and dropout/switching analyses
df_stu <- df %>%
  group_by(stdid) %>%
  summarise(
    female               = first(female),
    univcode             = first(univcode),
    department_id        = first(department_id),
    fem_share            = mean(fac_female, na.rm = TRUE) * 10,   # ×10, as in Eq. 3
    father_college       = first(father_college),
    mother_college       = first(mother_college),
    reservation_stu      = first(reservation_stu),
    # baseline psych & test
    b_math_anxiety       = first(b_math_anxiety),
    b_math_confidence    = first(b_math_confidence),
    b_math_g1_score      = first(b_math_g1_score),
    b_physics_g1_score   = first(b_physics_g1_score),
    b_stay_branch        = first(b_stay_branch),
    # endline psych, test, and longer-run outcomes
    e_math_anxiety       = first(e_math_anxiety),
    e_math_confidence    = first(e_math_confidence),
    e_math_g3_score      = first(e_math_g3_score),
    e_physics_g3_score   = first(e_physics_g3_score),
    e_dropped_out        = first(e_dropped_out),
    e_switchmajor        = first(e_switchmajor),
    e_attend_grad_school = first(e_attend_grad_school),
    e_college_satisfied  = first(e_college_satisfied),
    e_stay_branch        = first(e_stay_branch),
    e_salary_expected    = first(e_salary_expected),
    e_stem_belonging_z   = first(e_stem_belonging_z),
    .groups = "drop"
  ) %>%
  mutate(
    has_admin    = 1L,
    has_b_test   = as.integer(!is.na(b_math_g1_score) | !is.na(b_physics_g1_score)),
    has_e_test   = as.integer(!is.na(e_math_g3_score) | !is.na(e_physics_g3_score)),
    has_b_psych  = as.integer(!is.na(b_math_anxiety)  | !is.na(b_math_confidence)),
    has_e_psych  = as.integer(!is.na(e_math_anxiety)  | !is.na(e_math_confidence))
  )

set.seed(9176534)
B <- 1000   # number of resamples

# Baseline covariates to test (same set as Table E2)
covars <- c("b_achievement_z", "b_anxiety_z", "b_confidence_z")

term_var <- dplyr::case_when(
  "semester_clean" %in% names(df) ~ "semester_clean",
  "semester" %in% names(df) ~ "semester",
  "term" %in% names(df) ~ "term",
  TRUE ~ NA_character_
)

if (is.na(term_var)) {
  stop("Permutation diagnostics require a semester/term variable.")
}

# Unique stratum   = institution + department + term + course
# Unique classroom = stratum + faculty/section
df_perm <-
  df %>%
  mutate(
    stratum_id = paste(univcode, department_id, .data[[term_var]], course_name, sep = "_"),
    classroom = paste(stratum_id, facid, sep = "_")
  )

# Restrict to strata with multiple classrooms
multi_classroom_strata <-
  df_perm %>%
  group_by(stratum_id) %>%
  summarise(n_classrooms = n_distinct(classroom), .groups = "drop") %>%
  filter(n_classrooms >= 2) %>%
  pull(stratum_id)

df_perm <- df_perm %>% filter(stratum_id %in% multi_classroom_strata)

# ---- Function: per-classroom empirical p-values for ONE covariate ----
compute_classroom_pvalues <- function(covar) {

  # Keep only students with a non-missing value for this covariate
  d <- df_perm %>%
    select(stratum_id, classroom, value = all_of(covar)) %>%
    filter(!is.na(value))

  # After dropping NAs, drop strata that no longer have >=2 classrooms
  ok <- d %>%
    group_by(stratum_id) %>%
    summarise(n_cl = n_distinct(classroom), .groups = "drop") %>%
    filter(n_cl >= 2) %>% pull(stratum_id)
  d <- d %>% filter(stratum_id %in% ok)

  # STEP 1: actual (observed) average for each classroom
  obs_means <- d %>%
    group_by(stratum_id, classroom) %>%
    summarise(obs_mean = mean(value), n_students = n(), .groups = "drop")

  # ---- Precompute structure ONCE ----
  dt          <- as.data.table(d)
  classroom_f <- factor(d$classroom, levels = obs_means$classroom)
  n_per_cl    <- as.numeric(table(classroom_f))

  # Sparse membership matrix M (n_classrooms x n_students), row-normalized:
  # M %*% v returns the vector of classroom means of v, in obs_means order.
  M <- sparseMatrix(
    i    = as.integer(classroom_f),
    j    = seq_along(classroom_f),
    x    = 1 / n_per_cl[as.integer(classroom_f)],
    dims = c(nlevels(classroom_f), length(classroom_f))
  )

  # STEPS 2-3: B resamples. Within each course, shuffle values without
  # replacement; synthetic classroom means via one matrix multiply.
  perm_means_mat <- matrix(NA_real_, nrow = nrow(obs_means), ncol = B)
  t_start <- Sys.time()

  for (b in seq_len(B)) {
    dt[, value_perm := sample(value), by = stratum_id]   # within-stratum shuffle
    perm_means_mat[, b] <- as.numeric(M %*% dt$value_perm)

    if (b %% 1000 == 0 && interactive()) {
      elapsed <- as.numeric(Sys.time() - t_start, units = "secs")
      eta     <- elapsed / b * (B - b)
      cat(sprintf("  %s: %d/%d  elapsed %.0fs  eta %.0fs\n",
                  covar, b, B, elapsed, eta))
      flush.console()
    }
  }

  # STEPS 4-6: place each classroom's actual average in its synthetic
  # distribution. p-value = proportion of synthetic averages greater than
  # the actual average. Mid-p correction (count half of exact ties) makes
  # this valid for discrete covariates (binary vars and integer test scores).
  classroom_pvals <- sapply(seq_len(nrow(obs_means)), function(i) {
    obs  <- obs_means$obs_mean[i]
    perm <- perm_means_mat[i, ]
    (sum(perm > obs) + 0.5 * sum(perm == obs)) / length(perm)
  })

  tibble(
    covariate  = covar,
    stratum_id = obs_means$stratum_id,
    classroom  = obs_means$classroom,
    n_students = obs_means$n_students,
    obs_mean   = obs_means$obs_mean,
    p_value    = classroom_pvals
  )
}

# STEP 7: repeat for all covariates; one p-value per classroom per covariate
all_classroom_pvals <- map_dfr(covars, compute_classroom_pvalues)

# STEP 8: test uniformity of the per-classroom p-value distribution
uniformity_tests <- all_classroom_pvals %>%
  group_by(covariate) %>%
  summarise(
    n_classrooms = n(),
    ks_p     = suppressWarnings(ks.test(p_value, "punif", 0, 1)$p.value),
    chisq_p  = {
      bins <- cut(p_value, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
      suppressWarnings(chisq.test(table(bins))$p.value)
    },
    .groups = "drop"
  )

knitr::kable(
  uniformity_tests,
  digits = 3,
  caption = "Tests of uniformity of per-classroom p-values, by baseline covariate. Under random assignment, p-values should be uniformly distributed."
)

perm_cdf_plot <- all_classroom_pvals %>%
  mutate(
    covariate_label = recode(
      covariate,
      b_achievement_z = "Baseline achievement",
      b_anxiety_z = "Baseline STEM-related anxiety",
      b_confidence_z = "Baseline STEM-related confidence"
    )
  ) %>%
  ggplot(aes(p_value)) +
  stat_ecdf(geom = "step", linewidth = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#B2182B", linewidth = 0.6) +
  facet_wrap(~ covariate_label, nrow = 1) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  labs(
    x = "Per-classroom empirical p-value",
    y = "Cumulative share of classrooms"
  ) +
  theme_classic(base_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    panel.spacing = unit(0.75, "lines"),
    plot.margin = margin(4, 4, 4, 4)
  )

ggsave(
  file.path(output_dir, "fig_perm_randomization_cdf.pdf"),
  perm_cdf_plot,
  width = 7.2,
  height = 2.55,
  units = "in"
)

perm_cdf_plot

ggplot(all_classroom_pvals, aes(p_value)) +
  geom_histogram(breaks = seq(0, 1, 0.1),
                 fill = "grey75", colour = "white") +
  # expected count per bin if p-values were uniform: n_classrooms / 10
  geom_hline(data = uniformity_tests,
             aes(yintercept = n_classrooms / 10),
             linetype = "dashed", colour = "red") +
  facet_wrap(~ covariate, ncol = 2, scales = "free_y") +
  labs(
    x = "Per-classroom empirical p-value",
    y = "Number of classrooms",
    caption = paste0(
      "Histogram of per-classroom p-values (10 bins). Dashed red line = expected ",
      "count per bin under uniformity (random assignment). ", B,
      " placebo assignments."
    )
  ) +
  theme_minimal(base_size = 10)

set.seed(9176534)
B <- 1000   # number of resamples

covars <- c("father_college", "mother_college", "reservation_stu", "female")

# df_perm, stratum_id, classroom already built above and restricted to
# strata with >= 2 classrooms.

# T = size-weighted between-classroom dispersion, summed over strata
#   T_s = sum_j  n_sj * (mean_sj - mean_s)^2
compute_T <- function(stratum_id, classroom, values) {
  d <- data.table(stratum_id = stratum_id, classroom = classroom, v = values)
  cl <- d[, .(cl_mean = mean(v), n_cj = .N), by = .(stratum_id, classroom)]
  co <- d[, .(stratum_mean = mean(v)), by = stratum_id]
  cl <- merge(cl, co, by = "stratum_id")
  cl[, sum(n_cj * (cl_mean - stratum_mean)^2)]
}

# Standardized (F-style) variant: each stratum's between-classroom SS is
# divided by its degrees of freedom and its within-stratum variance
compute_T_std <- function(stratum_id, classroom, values) {
  d <- data.table(stratum_id = stratum_id, classroom = classroom, v = values)
  cl <- d[, .(cl_mean = mean(v), n_cj = .N), by = .(stratum_id, classroom)]
  co <- d[, .(stratum_mean = mean(v), within_var = var(v),
              n_cl = uniqueN(classroom)), by = stratum_id]
  cl <- merge(cl, co, by = "stratum_id")
  contrib <- cl[, .(between = sum(n_cj * (cl_mean - stratum_mean)^2)),
                by = stratum_id]
  contrib <- merge(contrib, co[, .(stratum_id, within_var, n_cl)],
                   by = "stratum_id")
  contrib[within_var > 0 & n_cl > 1,
          sum((between / (n_cl - 1)) / within_var)]
}

# Within-stratum shuffle helper (without replacement)
# returns a permuted copy of `values`, shuffled only within each stratum
shuffle_within_stratum <- function(stratum_id, values) {
  idx <- ave(seq_along(values), stratum_id,
             FUN = function(i) sample(i))   # permuted row positions per stratum
  values[idx]
}

# Omnibus test for ONE covariate
compute_omnibus_test <- function(covar) {

  d <- df_perm %>%
    select(stratum_id, classroom, value = all_of(covar)) %>%
    filter(!is.na(value))

  # After dropping NAs, keep only strata that still have >= 2 classrooms
  ok <- d %>%
    group_by(stratum_id) %>%
    summarise(n_cl = n_distinct(classroom), .groups = "drop") %>%
    filter(n_cl >= 2) %>% pull(stratum_id)
  d <- d %>% filter(stratum_id %in% ok)

  sid <- d$stratum_id
  cls <- d$classroom
  val <- d$value

  # Observed statistics
  T_obs     <- compute_T(sid, cls, val)
  T_obs_std <- compute_T_std(sid, cls, val)

  # Permutation distribution: shuffle students across classrooms within
  # stratum (preserves classroom sizes), recompute statistics each time
  T_perm     <- numeric(B)
  T_perm_std <- numeric(B)

  for (b in seq_len(B)) {
    val_perm      <- shuffle_within_stratum(sid, val)
    T_perm[b]     <- compute_T(sid, cls, val_perm)
    T_perm_std[b] <- compute_T_std(sid, cls, val_perm)
  }

  tibble(
    covariate     = covar,
    n_strata      = length(ok),
    n_classrooms  = dplyr::n_distinct(cls),
    T_obs         = T_obs,
    p_omnibus     = (sum(T_perm     >= T_obs)     + 1) / (B + 1),
    T_obs_std     = T_obs_std,
    p_omnibus_std = (sum(T_perm_std >= T_obs_std) + 1) / (B + 1)
  )
}

omnibus_results <- map_dfr(covars, compute_omnibus_test)

knitr::kable(
  omnibus_results,
  digits = 3,
  caption = "Omnibus randomization test of covariate balance across classrooms within course-by-institution-by-term strata. p_omnibus uses size-weighted between-classroom dispersion; p_omnibus_std additionally standardizes each stratum by its within-stratum variance. Under random assignment, observed dispersion should not exceed the permutation distribution."
)

covariate_labels <- c(
  b_achievement_z = "Mathematics and science proficiency",
  b_anxiety_z = "STEM-related anxiety",
  b_confidence_z = "STEM-related confidence",
  father_college = "Father attended college",
  mother_college = "Mother attended college",
  reservation_stu = "Eligible for caste-based affirmative action",
  female = "Female student"
)

fmt_p <- function(x) sprintf("%.3f", x)

strata_counts <- all_classroom_pvals %>%
  group_by(covariate) %>%
  summarise(n_strata = n_distinct(stratum_id), .groups = "drop")

panel_a <- uniformity_tests %>%
  left_join(strata_counts, by = "covariate") %>%
  transmute(
    covariate = covariate_labels[covariate],
    strata = n_strata,
    classrooms = n_classrooms,
    ks_p = fmt_p(ks_p),
    chisq_p = fmt_p(chisq_p)
  )

panel_b <- omnibus_results %>%
  transmute(
    covariate = covariate_labels[covariate],
    strata = n_strata,
    classrooms = n_classrooms,
    p_omnibus = fmt_p(p_omnibus),
    p_omnibus_std = fmt_p(p_omnibus_std)
  )

perm_table_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{footnotesize}",
  "\\caption{Design-based permutation diagnostics for random assignment to classrooms.}",
  "\\label{tab:permutation_diagnostics}",
  "\\begin{adjustbox}{max width=\\textwidth}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "\\multicolumn{5}{l}{\\textit{Panel A: Uniformity of per-classroom empirical $P$-values}} \\\\",
  "\\midrule",
  "Baseline covariate & Strata & Classrooms & KS $P$-value & $\\chi^2$ $P$-value \\\\",
  "\\midrule",
  paste0(panel_a$covariate, " & ", panel_a$strata, " & ", panel_a$classrooms, " & ", panel_a$ks_p, " & ", panel_a$chisq_p, " \\\\"),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B: Omnibus within-stratum permutation tests}} \\\\",
  "\\midrule",
  "Baseline covariate & Strata & Classrooms & Size-weighted $P$-value & Standardized $P$-value \\\\",
  "\\midrule",
  paste0(panel_b$covariate, " & ", panel_b$strata, " & ", panel_b$classrooms, " & ", panel_b$p_omnibus, " & ", panel_b$p_omnibus_std, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\item Notes: This table reports design-based permutation diagnostics using realized student-classroom rosters. Placebo assignments reassign students across classrooms within course-by-institution-by-term strata while preserving realized classroom sizes. Each diagnostic uses 1,000 placebo assignments. Panel A reports tests of whether per-classroom empirical $P$-values are uniformly distributed for continuous baseline covariates. Panel B reports omnibus randomization $P$-values for binary covariates based on across-classroom dispersion within strata. The size-weighted statistic sums classroom-size-weighted dispersion around the stratum mean; the standardized statistic scales each stratum by its within-stratum variance.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{adjustbox}",
  "\\end{footnotesize}",
  "\\end{table}"
)

writeLines(perm_table_lines, file.path(output_dir, "tab_perm_randomization_diagnostics.tex"))
