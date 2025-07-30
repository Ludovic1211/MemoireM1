# ==============================================================
# nested_helpers.R   –  Outer / inner nested-CV utilities
# ==============================================================

library(tidyverse)
library(tidymodels)
library(furrr)
plan(multisession)

# --------------------------------------------------------------------
# 1. OUTER WRAPPER – nested CV ---------------------------------------
# --------------------------------------------------------------------
run_nested <- function(data, outcome,
                       build_inner_fun,
                       outer_v = 3, inner_v = 5,
                       seed = 123) {
  
  set.seed(seed)
  outer_folds <- vfold_cv(data, v = outer_v, strata = !!sym(outcome))
  
  map_dfr(seq_along(outer_folds$splits), function(i) {
    
    split    <- outer_folds$splits[[i]]
    id       <- outer_folds$id[[i]]
    analysis <- analysis(split)
    assess   <- assessment(split)
    
    ## 1) entraînement sur le sous-ensemble d’apprentissage
    inner_res <- build_inner_fun(analysis, outcome, inner_v)
    
    ## 2) préparation éventuelle du jeu test ------------------------
    if (inherits(inner_res$wf$pre, "recipe")) {
      rec_obj      <- workflows::extract_recipe(inner_res$wf)
      assess_ready <- bake(rec_obj, new_data = assess)
    } else {
      assess_ready <- assess
    }
    
    ## vecteur-vérité forcé à contenir les 2 niveaux globaux
    all_lv    <- levels(data[[outcome]])
    truth_vec <- factor(assess_ready[[outcome]], levels = all_lv)
    
    ## 3) prédictions ----------------------------------------------
    prob_df  <- predict(inner_res$wf, assess_ready, type = "prob")
    class_df <- predict(inner_res$wf, assess_ready, type = "class")
    
    
    pos_level <- all_lv[2]
    prob_col  <- paste0(".pred_", pos_level)
    if (!prob_col %in% names(prob_df))
      prob_col <- names(prob_df)[1]
    
    ## 4) métriques robustes ---------------------------------------
    if (n_distinct(truth_vec) < 2) {
      roc_val <- NA_real_
      acc_val <- NA_real_
    } else {
      roc_val <- roc_auc_vec(truth_vec,
                             prob_df[[prob_col]],
                             event_level = "second")
      acc_val <- accuracy_vec(truth_vec,
                              class_df$.pred_class)
    }
    
    tibble(
      fold     = id,
      roc_auc  = roc_val,
      accuracy = acc_val,
      n_vars   = inner_res$n_vars,
      time_sec = inner_res$time
    )
  }) %>%
    summarise(
      roc_auc_mean   = mean(roc_auc,  na.rm = TRUE),
      roc_auc_sd     = sd(   roc_auc,  na.rm = TRUE),
      accuracy_mean  = mean(accuracy, na.rm = TRUE),
      accuracy_sd    = sd(   accuracy, na.rm = TRUE),
      n_vars_mean    = mean(n_vars),
      time_total_sec = sum(time_sec)
    )
}




# --------------------------------------------------------------------
# 2. Chronomètre ------------------------------------------------------
time_it <- function(expr) {
  start <- Sys.time()
  res   <- eval(expr)
  attr(res, "time") <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  res
}

# ==============================================================
#  FONCTIONS inner pour chaque méthode de sélection
# ==============================================================

library(caret)          # pour RFE
library(FSelectorRcpp)  # pour information_gain
library(GA)             # pour l’algorithme génétique

# ---------- 1. Filter IG + Logit --------------
build_filter_logit <- function(analysis, outcome,
                               top_k = 20, apply_fdr = FALSE) {
  
  ig_tbl <- FSelectorRcpp::information_gain(
    as.formula(paste(outcome, "~ .")), data = analysis)
  
  ig_tbl <- if (apply_fdr) {
    ig_tbl %>%
      mutate(p_adj = p.adjust(p.value, method = "BH")) %>%
      filter(p_adj < 0.05)
  } else {
    ig_tbl %>%
      arrange(desc(importance)) %>%
      slice_head(n = top_k)
  }
  
  sel_vars <- ig_tbl$attributes
  
  rec <- recipe(as.formula(
    paste(outcome, "~", paste(sel_vars, collapse = "+"))),
    data = analysis) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  wf <- workflow() %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    add_recipe(rec)
  
  fit_wf <- time_it(fit(wf, data = analysis))
  
  list(wf = fit_wf,
       n_vars = length(sel_vars),
       time   = attr(fit_wf, "time"))
}

# ---------- 2. Wrapper greedy : RFE + Logit --------

build_rfe_logit <- function(analysis, outcome,
                            inner_v = 5,
                            sizes   = c(5, 10, 20, 40)) {
  
  x0 <- dplyr::select(analysis, -all_of(outcome))
  nzv_idx <- caret::nearZeroVar(x0, saveMetrics = FALSE)
  if (length(nzv_idx) > 0) x0 <- x0[, -nzv_idx]
  y0 <- analysis[[outcome]]
  

  set.seed(123)
  test_idx  <- caret::createFolds(y0, k = inner_v)
  train_idx <- lapply(test_idx, function(i) setdiff(seq_len(nrow(x0)), i))
  
  ctrl <- rfeControl(
    functions = lrFuncs,
    method    = "cv",
    index     = train_idx,
    indexOut  = test_idx,
    allowParallel = TRUE
  )
  
  rfe_fit <- time_it(
    rfe(
      x          = x0,
      y          = y0,
      sizes      = sizes,
      rfeControl = ctrl
    )
  )
  
  best_vars <- predictors(rfe_fit)
  
  
  analysis_sel <- dplyr::bind_cols(readmitted_bin = y0, x0)[ , c(outcome, best_vars)]
  
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis_sel) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    fit(analysis_sel)
  
  list(
    wf     = wf,
    n_vars = length(best_vars),
    time   = attr(rfe_fit, "time")
  )
}




# ---------- wrapper non-greedy : GA subset + Logit ----------
build_ga_logit <- function(analysis, outcome,
                           pop = 30, iter = 50) {
  
  full_X <- dplyr::select(analysis, -all_of(outcome))
  y      <- analysis[[outcome]]
  
  # ---------- Fitness ---------------------------------------
  bt <- function(v) paste0("`", v, "`")
  GA_fitness <- function(bits, X, y) {
    if (sum(bits) == 0) return(-Inf)
    vars <- names(X)[bits == 1]
    fmla <- as.formula(paste("y ~", paste(bt(vars), collapse = "+")))
    df   <- dplyr::bind_cols(y = y, X[, vars, drop = FALSE])
    -AIC(glm(fmla, data = df, family = binomial))
  }
  
  ga_res <- time_it(
    GA::ga(
      type    = "binary",
      fitness = GA_fitness,
      nBits   = ncol(full_X),
      X = full_X, y = y,
      popSize = pop,
      maxiter = iter,
      run     = 20,
      parallel = FALSE
    )
  )
  
  sel_vars <- names(full_X)[ga_res@solution[1, ] == 1]
  
  # ---------- Dataset réduit --------------------------------
  analysis_sel <- dplyr::select(analysis, all_of(c(outcome, sel_vars)))
  
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis_sel) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    fit(analysis_sel)
  
  list(
    wf     = wf,
    n_vars = length(sel_vars),
    time   = attr(ga_res, "time")
  )
}
# ---------- wrapper non-greedy : GA caret ----------
build_ga_logit_caret <- function(analysis, outcome,
                                 iters = 40, pop_size = 30) {
  
  # 1. Recette de base
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  # 2. AUC comme métrique interne
  two_stats <- function(dat, lev = levels(dat$obs), model = NULL) {
    twoClassSummary(dat, lev, model)
  }
  
  # 3. Fonctions GA adaptées
  ga_fun <- caretGA
  ga_fun$fitness_extern <- two_stats
  ga_fun$initial <- function(vars, popSize, ...) {
    probs <- seq(0.1, 0.9, length.out = popSize)
    t(replicate(popSize,
                rbinom(vars, 1, sample(probs, 1)),
                simplify = "matrix"))
  }
  
  ctrl_inner <- trainControl(
    method = "LGOCV", p = 0.9, number = 1,
    classProbs = TRUE, summaryFunction = two_stats,
    allowParallel = FALSE
  )
  
  ctrl_ga <- gafsControl(
    functions = ga_fun,
    method    = "cv",
    metric    = c(internal = "ROC", external = "ROC"),
    maximize  = c(internal = TRUE,  external = TRUE),
    returnResamp = "none",
    verbose = FALSE
  )
  
  # 4. Recherche GA
  ga_fit <- time_it(
    gafs(
      rec,
      data        = analysis,
      iters       = iters,
      popSize     = pop_size,
      method      = "glm",
      family      = binomial,
      metric      = "ROC",
      trControl   = ctrl_inner,
      gafsControl = ctrl_ga
    )
  )
  
  sel_vars <- ga_fit$optVariables
  
  # -----------------------------------------------
  # 5. Recette finale
  # -----------------------------------------------
  rec_final <- rec %>% 
    step_select(all_outcomes(), all_of(sel_vars), skip = TRUE)
  
  
  wf <- workflow() %>% 
    add_recipe(rec_final) %>% 
    add_model(logistic_reg() %>% set_engine("glm")) %>% 
    fit(analysis)
  
  list(
    wf     = wf,
    n_vars = length(sel_vars),
    time   = attr(ga_fit, "time")
  )
}





