# dbn_simulation.R
# Dynamic Bayesian Network (DBN) Structure Learning Simulation
# Runs 30 replications for k = 5, 7, or 10 variables

library(dbnR)
library(dplyr)

# =============================
# User parameters
# =============================
k <- 10        # Choose: 5, 7, or 10
p <- 1         # lag
T <- 1000      # time series length
n_obs <- 100   # sample size from simulated series

# =============================
# Define A1, Sigma, and DAG for k
# =============================
if (k == 5) {
  A1 <- matrix(c(
    0.00, -0.47, -0.11,  0.00,  0.00,
    -0.95,  0.00,  0.00,  0.00, -0.45,
    -0.68,  0.00,  0.00, -0.67,  0.48,
    0.00,  0.00,  0.15,  0.00, -0.80,
    -0.63,  0.00,  0.00,  0.00,  0.00), nrow=5)
  Sigma1 <- diag(c(3.88, 4.50, 4.04, 4.54, 2.83))
  dag_original <- model2network("[Series.1_t_1][Series.2_t_1][Series.3_t_1][Series.4_t_1][Series.5_t_1][Series.1_t_0|Series.2_t_1:Series.3_t_1:Series.5_t_1][Series.2_t_0|Series.1_t_1][Series.3_t_0|Series.1_t_1:Series.4_t_1][Series.4_t_0|Series.3_t_1][Series.5_t_0|Series.2_t_1:Series.3_t_1:Series.4_t_1]")
} else if (k == 7) {
  A1 <- matrix(c(
    0.00, -0.85, -0.94, 0.16, 0.76, 0.00, -0.23,
    0.15,  0.00,  0.00, 0.27,  0.00, 0.00,  0.00,
    0.00,  0.00,  0.00, 0.00, -0.64, 0.39, -0.28,
    0.00,  0.00,  0.00, 0.38,  0.00, 0.00,  0.48,
    0.00,  0.10,  0.64, 0.00,  0.00, 0.00,  0.00,
    -0.66,  0.44,  0.00, 0.38,  0.52, 0.00,  0.13,
    0.00,  0.00,  0.00, 0.00, -0.65, 0.00, -0.05), nrow=7)
  Sigma1 <- diag(c(3.88, 4.50, 4.04, 4.54, 2.83, 1.67, 2.30))
  dag_original <- model2network("[Series.1_t_1][Series.2_t_1][Series.3_t_1][Series.4_t_1][Series.5_t_1][Series.6_t_1][Series.7_t_1][Series.1_t_0|Series.2_t_1:Series.6_t_1][Series.2_t_0|Series.1_t_1:Series.5_t_1:Series.6_t_1][Series.3_t_0|Series.1_t_1:Series.5_t_1][Series.4_t_0|Series.1_t_1:Series.2_t_1:Series.4_t_1:Series.6_t_1][Series.5_t_0|Series.1_t_1:Series.3_t_1:Series.6_t_1:Series.7_t_1][Series.6_t_0|Series.3_t_1][Series.7_t_0|Series.1_t_1:Series.3_t_1:Series.4_t_1:Series.6_t_1:Series.7_t_1]")
} else if (k == 10) {
  A1 <- matrix(c(
    0.00, -0.56,  0.00,  0.00, 0.00,  0.00,  0.29, -0.11, 0.78,  0.00,
    0.21,  0.39,  0.00, -0.58, 0.00,  0.00,  0.00, -0.27, 0.95,  0.54,
    0.00,  0.86,  0.00,  0.00, 0.00,  0.33,  0.53,  0.00, 0.00,  0.00,
    0.58,  0.00,  0.00, -0.89, 0.00,  0.72,  0.00,  0.00, 0.00,  0.00,
    -0.83, -0.71, -0.75,  0.02, 0.12,  0.00,  0.31,  0.00, 0.00,  0.00,
    -0.13,  0.22, -0.09,  0.00, 0.00,  0.00,  0.00,  0.00, 0.00, -0.43,
    -0.18,  0.39, 0.88,  0.00, 0.00, -0.56,  0.00,  0.10, 0.22,  0.08,
    0.31,  0.00,  0.00,  0.00, 0.81,  0.00,  0.00,  0.00, 0.00,  0.34,
    0.00,  0.00, -0.57,  0.00, 0.28,  0.00,  0.00,  0.00, 0.00, -0.34,
    -0.91,  0.00,  0.00, -0.34, 0.77, -0.32, -0.08, -0.62, 0.00,  0.00), nrow=10)
  Sigma1 <- diag(c(3.88, 4.50, 4.04, 4.54, 2.83, 1.67, 2.30, 3.04, 3.91, 4.96))
  dag_original <- model2network("[Series.1_t_1][Series.2_t_1][Series.3_t_1][Series.4_t_1][Series.5_t_1][Series.6_t_1][Series.7_t_1][Series.8_t_1][Series.9_t_1][Series.10_t_1][Series.1_t_0|Series.2_t_1:Series.4_t_1:Series.5_t_1:Series.6_t_1:Series.7_t_1:Series.8_t_1:Series.10_t_1][Series.2_t_0|Series.1_t_1:Series.2_t_1:Series.3_t_1:Series.5_t_1:Series.6_t_1:Series.7_t_1][Series.3_t_0|Series.5_t_1:Series.6_t_1:Series.7_t_1:Series.9_t_1][Series.4_t_0|Series.2_t_1:Series.4_t_1:Series.5_t_1:Series.10_t_1][Series.5_t_0|Series.5_t_1:Series.8_t_1:Series.9_t_1:Series.10_t_1][Series.6_t_0|Series.3_t_1:Series.4_t_1:Series.7_t_1:Series.10_t_1][Series.7_t_0|Series.1_t_1:Series.3_t_1:Series.5_t_1:Series.10_t_1][Series.8_t_0|Series.1_t_1:Series.2_t_1:Series.7_t_1:Series.10_t_1][Series.9_t_0|Series.1_t_1:Series.2_t_1:Series.7_t_1][Series.10_t_0|Series.2_t_1:Series.6_t_1:Series.7_t_1:Series.8_t_1:Series.9_t_1]")
} else {
  stop("k must be 5, 7 or 10")
}

# =============================
# Define custom score functions
# =============================
define_score_fun <- function(metric_fun) {
  function(node, parents, data, args) {
    model <- if (length(parents) == 0) paste0("`", node, "` ~ 1") else
      paste0("`", node, "` ~ ", paste0("`", parents, "`", collapse="+"))
    - (metric_fun(lm(as.formula(model), data=data)) / 2)
  }
}

score_funs <- list(
  "AIC-G"      = define_score_fun(function(m) -2*logLik(m) + 2*k_fun(m)),
  "KIC"        = define_score_fun(function(m) -2*logLik(m) + 3*k_fun(m)),
  "AIC4"       = define_score_fun(function(m) -2*logLik(m) + 4*k_fun(m)),
  "CAIC"       = define_score_fun(function(m) -2*logLik(m) + k_fun(m)*(log(n_fun(m))+1)),
  "BIC-G"      = define_score_fun(function(m) -2*logLik(m) + k_fun(m)*log(n_fun(m))),
  "BICadj"     = define_score_fun(function(m) -2*logLik(m) + k_fun(m)*log((n_fun(m)+2)/24)),
  "HBIC"       = define_score_fun(function(m) -2*logLik(m) + k_fun(m)*log(n_fun(m)/(2*pi))),
  "BICQ_0.25"  = define_score_fun(function(m) -2*logLik(m) + k_fun(m)*log(n_fun(m)) - 2*k_fun(m)*log(0.25/0.75))
)

# =============================
# Run simulation
# =============================
set.seed(123)
results <- matrix(NA, 30, length(score_funs))
shd_results <- matrix(NA, 30, length(score_funs))
colnames(results) <- colnames(shd_results) <- names(score_funs)

for (i in 1:30) {
  cat("Iteration", i, "\n")
  data_sim <- MultVarSim(k, A1, p, Sigma=Sigma1, T)
  data_df <- as.data.frame(ts(data_sim)) %>% slice_head(n=n_obs)
  colnames(data_df) <- make.names(colnames(data_df))
  data_fold <- fold_dt(data_df, size=2)

  for (j in seq_along(score_funs)) {
    net <- learn_dbn_struc(data_df, 2, method="dmmhc", restrict="iamb", maximize="tabu",
                           maximize.args=list(score="custom", fun=score_funs[[j]]))
    class(net) <- "bn"
    results[i,j] <- tryCatch(score(net, data_fold, type="bic-g"), error=function(e) NA)
    shd_results[i,j] <- tryCatch(shd(dag_original, net), error=function(e) NA)
  }
}

# =============================
# Summaries
# =============================
results_df <- as.data.frame(results)
shd_df <- as.data.frame(shd_results)

score_summary <- data.frame(
  Criterion = colnames(results_df),
  Mean = colMeans(results_df, na.rm=TRUE),
  SE = apply(results_df, 2, function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))
)

shd_summary <- data.frame(
  Criterion = colnames(shd_df),
  Mean = colMeans(shd_df, na.rm=TRUE),
  SE = apply(shd_df, 2, function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))
)

print(score_summary)
print(shd_summary)

cat("Simulation completed for k =", k, "\n")
