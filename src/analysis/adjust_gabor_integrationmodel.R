library(tidyverse)
library(gghalves)
library(ggplot2)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# ---- Read and prepare data -----------------------------------------

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

data <- data_exp4 %>%
  dplyr::select(label, ori, participant,
                inner_resp, midd_resp, outer_resp) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  mutate(
    arr     = if_else(str_detect(label, "snake"), "snake", "ladder"),
    abs_ori = abs(ori),
    err_inner = inner_resp - ori,
    err_mid   = midd_resp  - ori,
    err_outer = outer_resp - ori
  )

# fit by orienation

FIT_BY_ORIENTATION <- FALSE

group_vars <- c("participant", "arr")
if (FIT_BY_ORIENTATION) {
  group_vars <- c(group_vars, "abs_ori")
}
# ---- to 3-column error matrix 

err_matrix <- function(df) {
  as.matrix(df[, c("err_inner", "err_mid", "err_outer")])
}

# ---- negative log-likelihood ----

# err_i = mu_i + lambda_i * G + eps_i
# G ~ N(0, 1), eps_i ~ N(0, psi_i)
# cov(errors) = lambda lambda' + diag(psi)

# optimizer's gooal: find parameters that minimize this value, the best fit data

nll_factor <- function(par, X) {
  mu  <- par[1:3]
  lam <- par[4:6]
  psi <- exp(par[7:9])   # constrain private variances to be positive
  
  S <- lam %*% t(lam) + diag(psi)
  
  ld <- determinant(S, logarithm = TRUE)
  if (ld$sign <= 0) return(1e10)
  
  Xc <- sweep(X, 2, mu)
  Si <- solve(S)
  
  # negative log-likelihood, omitting the constant term
  0.5 * (nrow(X) * as.numeric(ld$modulus) + sum((Xc %*% Si) * Xc))
}

# ---- fit here-----

fit_factor <- function(X, n_starts = 8) {
  X <- X[complete.cases(X), , drop = FALSE]
  
  if (nrow(X) < 15) {
    return(list(index = NA_real_,
                shared_frac = rep(NA_real_, 3),
                loadings = rep(NA_real_, 3),
                psi = rep(NA_real_, 3),
                n = nrow(X)))
  }
  
  C <- cov(X)
  
  p0 <- c(
    colMeans(X),
    sqrt(pmax(diag(C), 1e-3)) * 0.6,
    log(pmax(diag(C), 1e-3) * 0.5 + 1)
  )
  
  best <- NULL
  
  for (i in seq_len(n_starts)) {
    init <- p0 + rnorm(9, 0, 0.2)
    
    fit <- optim(
      init,
      nll_factor,
      X = X,
      method = "Nelder-Mead",
      control = list(maxit = 20000, reltol = 1e-10)
    )
    
    if (is.null(best) || fit$value < best$value) {
      best <- fit
    }
  }
  
  mu  <- best$par[1:3]
  lam <- best$par[4:6]
  psi <- exp(best$par[7:9])
  
  shared_frac <- lam^2 / (lam^2 + psi)
  
  list(
    index = mean(shared_frac),
    shared_frac = shared_frac,
    loadings = abs(lam),
    psi = psi,
    mu = mu,
    n = nrow(X),
    value = best$value
  )
}


# # all pooled together - one for ladder one for snake
# 
# pooled <- data %>%
#   group_by(arr) %>%
#   group_modify(~{
#     f <- fit_factor(err_matrix(.x))
#     tibble(
#       index = f$index,
#       shared_inner = f$shared_frac[1],
#       shared_mid   = f$shared_frac[2],
#       shared_outer = f$shared_frac[3],
#       loading_inner = f$loadings[1],
#       loading_mid   = f$loadings[2],
#       loading_outer = f$loadings[3],
#       psi_inner = f$psi[1],
#       psi_mid   = f$psi[2],
#       psi_outer = f$psi[3],
#       n = f$n
#     )
#   }) %>%
#   ungroup()


# fit for each participant
set.seed(42)

per_pp <- data %>%
  group_by(across(all_of(group_vars))) %>%
  group_modify(~{
    f <- fit_factor(err_matrix(.x))
    tibble(
      index = f$index,
      shared_inner = f$shared_frac[1],
      shared_mid   = f$shared_frac[2],
      shared_outer = f$shared_frac[3],
      mu_inner      = f$mu[1],
      mu_mid        = f$mu[2],
      mu_outer      = f$mu[3],
      loading_inner = f$loadings[1],
      loading_mid   = f$loadings[2],
      loading_outer = f$loadings[3],
      psi_inner     = f$psi[1],
      psi_mid       = f$psi[2],
      psi_outer     = f$psi[3],
      n = f$n
    )
  }) %>%
  ungroup()

# Dynamically define the columns for wide format and summaries
wide_names <- if (FIT_BY_ORIENTATION) c("arr", "abs_ori") else "arr"

per_pp_wide <- per_pp %>%
  select(participant, all_of(wide_names), index) %>%
  pivot_wider(
    names_from = all_of(wide_names), 
    values_from = index,
    names_sep = "_"
  )

# Descriptives
idx_summary <- per_pp %>%
  group_by(across(all_of(wide_names))) %>%
  summarise(
    n = n(),
    index_M  = mean(index, na.rm = TRUE),  
    index_SD = sd(index, na.rm = TRUE),
    .groups  = "drop"
  )

print(idx_summary)

# paired t-test
tt <- t.test(per_pp_wide$ladder, per_pp_wide$snake, paired = TRUE)
print(tt)

# Cohen's dz 
diff <- per_pp_wide$ladder - per_pp_wide$snake
dz   <- mean(diff) / sd(diff)

#plot 

# Calculate averages for the large dots, grouping dynamically
data_across_pp <- per_pp %>% 
  group_by(across(all_of(wide_names))) %>% 
  summarise(
    avg_index = mean(index, na.rm = TRUE),
    .groups = "drop"
  )

# Build the base plot
p_idx <- ggplot(per_pp, aes(x = arr, y = index, fill = arr)) +
  geom_half_violin(
    side = "l", alpha = 0.3, width = 1, color = "white", trim = FALSE
  ) + 
  geom_line(aes(group = participant), color = "grey", linewidth = 0.4) +
  geom_point(aes(color = arr), size = 2.2, alpha = 0.5) +
  geom_point(
    data = data_across_pp,
    aes(x = arr, y = avg_index, color = arr),
    size = 6
  ) +
  scale_fill_manual(values  = c(ladder = "#F28522", snake = "#674EA7")) +
  scale_color_manual(values = c(ladder = "#F28522", snake = "#674EA7")) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = "Integration Index") +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

# Conditionally add faceting
if (FIT_BY_ORIENTATION) {
  p_idx <- p_idx + facet_wrap(~ abs_ori, labeller = label_both)
}

print(p_idx)
# ggsave("p_idx.svg", p_idx, width = 6, height = 5, units = "in")



# Per-location shared fractions, averaged across participant
perloc_summary <- per_pp %>%
  group_by(arr) %>%
  summarise(
    inner = mean(shared_inner, na.rm = TRUE),
    mid   = mean(shared_mid,   na.rm = TRUE),
    outer = mean(shared_outer, na.rm = TRUE)
  )
perloc_summary

# Confirm there is NO inner-vs-outer gradient
snake_pl <- per_pp %>% filter(arr == "snake") 
t.test(snake_pl$shared_inner, snake_pl$shared_outer, paired = TRUE)

ladder_pl <- per_pp %>% filter(arr == "ladder")
t.test(ladder_pl$shared_inner, ladder_pl$shared_outer, paired = TRUE)


# ---- Independent (no shared factor) model: diagonal covariance only ---
# err_i = mu_i + eps_i ; cov = diag(psi). 6 params (3 mu, 3 psi).
# Compare against the one-factor model (9 params) by AIC/BIC.
# NOTE: full NLL WITH the constant term, so AIC/BIC are on the correct scale.

nll_full <- function(S, X, mu) {
  Xc <- sweep(X, 2, mu)
  ld <- determinant(S, logarithm = TRUE)
  0.5 * (nrow(X) * as.numeric(ld$modulus) +
           sum((Xc %*% solve(S)) * Xc) +
           nrow(X) * ncol(X) * log(2 * pi))     #constant included
}

# independent model: MLE is just per-column mean and variance
fit_independent <- function(X) {
  X  <- X[complete.cases(X), , drop = FALSE]
  mu <- colMeans(X)
  psi <- apply(X, 2, function(v) mean((v - mean(v))^2))  # MLE variance
  S  <- diag(psi)
  nll <- nll_full(S, X, mu)
  k  <- 6
  list(nll = nll, k = k, aic = 2*k + 2*nll, bic = k*log(nrow(X)) + 2*nll)
}

# one-factor model, but recompute its FULL nll (with constant) for fair comparison
fit_factor_ic <- function(X) {
  f   <- fit_factor(X)                     # your existing fit
  lam <- f$loadings; psi <- f$psi; mu <- f$mu
  S   <- lam %*% t(lam) + diag(psi)
  nll <- nll_full(S, X, mu)
  k   <- 9
  list(nll = nll, k = k, aic = 2*k + 2*nll, bic = k*log(nrow(X)) + 2*nll)
}

# run per arrangement (pooled)
set.seed(42)
model_compare <- map_dfr(c("ladder", "snake"), function(a) {
  X   <- err_matrix(filter(data, arr == a))
  ind <- fit_independent(X)
  fac <- fit_factor_ic(X)
  tibble(
    arr = a,
    AIC_independent = ind$aic, AIC_factor = fac$aic,
    BIC_independent = ind$bic, BIC_factor = fac$bic,
    dAIC = ind$aic - fac$aic,   # positive => factor model wins
    dBIC = ind$bic - fac$bic
  )
})

model_compare

# ---- Function: empirical correlation matrix -------------------------

get_empirical_cor <- function(df) {
  X <- df %>%
    dplyr::select(err_inner, err_mid, err_outer)
  
  cor_mat <- cor(X, use = "complete.obs")
  
  cor_df <- as.data.frame(as.table(cor_mat)) %>%
    rename(
      location_y = Var1,
      location_x = Var2,
      r = Freq
    )
  
  cor_df
}



# Model-implied correlations vs observed  (shows the model captures data)

# ---- Fit pooled model per arrangement, keep loadings + psi ----------
# (pooled across participants: this figure is about structure, not stats)
fit_ladder <- fit_factor(err_matrix(filter(data, arr == "ladder")))
fit_snake  <- fit_factor(err_matrix(filter(data, arr == "snake")))

# ---- turn fitted loadings + psi into a model correlation matrix -----
# cov = lambda lambda' + diag(psi), then scale to correlation
model_cor_from_fit <- function(f) {
  lam <- f$loadings
  psi <- f$psi
  S   <- lam %*% t(lam) + diag(psi)      # model covariance
  D   <- sqrt(diag(S))
  Cmod <- S / (D %o% D)                   # -> correlation
  dimnames(Cmod) <- list(
    c("err_inner", "err_mid", "err_outer"),
    c("err_inner", "err_mid", "err_outer")
  )
  as.data.frame(as.table(Cmod)) %>%
    rename(location_y = Var1, location_x = Var2, r = Freq)
}

model_cor_df <- bind_rows(
  model_cor_from_fit(fit_ladder) %>% mutate(arr = "ladder"),
  model_cor_from_fit(fit_snake)  %>% mutate(arr = "snake")
) %>%
  mutate(source = "Model")

# ---- Observed correlations
obs_cor_df <- data %>%
  group_by(arr) %>%
  group_modify(~ get_empirical_cor(.x)) %>%
  ungroup() %>%
  mutate(source = "Observed")


# ---- halve the matrix 
both_cor_df <- bind_rows(obs_cor_df, model_cor_df) %>%
  mutate(
    arr = factor(arr, levels = c("ladder", "snake"),
                 labels = c("Ladder", "Snake")),
    source = factor(source, levels = c("Observed", "Model")),
    location_x = factor(location_x,
                        levels = c("err_inner", "err_mid", "err_outer"),
                        labels = c("Inner", "Middle", "Outer")),
    location_y = factor(location_y,
                        levels = c("err_inner", "err_mid", "err_outer"),
                        labels = c("Inner", "Middle", "Outer")),
    r_label = sprintf("%.3f", r)
  ) %>%
  # ADD THIS LINE: <= keeps one half of the matrix PLUS the diagonal 
  filter(as.numeric(location_x) <= as.numeric(location_y))

# ---- Plot: Half-matrix Observed vs Model ----------------------------
p_cor_compare <- ggplot(both_cor_df,
                        aes(x = location_x, y = location_y, fill = r)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = r_label),
            size = 4,
            fontface = "bold") +
  facet_grid(source ~ arr) +
  scale_fill_gradient2(
    low = "#3B4CC0",
    mid = "white",
    high = "#B40426",
    midpoint = 0,
    limits = c(-1.0001, 1.0001), # Keep this to prevent grey 1s!
    name = "r"
  ) +
  coord_fixed() +
  labs(
    x = NULL,
    y = NULL,
    title = "Observed vs model-implied correlations"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(p_cor_compare)
# ggsave("fig_cor_compare.svg", p_cor_compare, width = 7, height = 6, units = "in")

