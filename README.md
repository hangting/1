# Ensemble Algorithm — R

An R package for predictor screening, regression and classification models, bootstrap aggregation (bagging), weighted ensemble learning, and model evaluation.

**Package:** `simpleEnsemble` · **Version:** `0.1.0` · **Language:** R (>= 4.0.0) · **License:** GPL-3

## Overview

`simpleEnsemble` provides a statistical modeling workflow for continuous and binary responses. It combines predictor screening with individual models (linear and logistic regression, ridge, lasso, and random forests), bagging, weighted prediction ensembles, and K-fold cross-validation. It includes examples for both low- and high-dimensional settings.

This repository contains the **R source code, help files, example data, and a detailed vignette**. It was originally developed as an academic project and is shared for review and further development.

## Main features

| Function | Purpose |
| --- | --- |
| `input_data()` | Validate and prepare predictors and responses |
| `screening()` | Rank predictors by marginal association; optionally check variance inflation factors (VIF) |
| `linear_fit()` | Fit linear regression for continuous responses |
| `logistic_fit()` | Fit logistic regression for binary responses |
| `glmnet_fit()` | Fit cross-validated ridge or lasso regression |
| `randomForest_fit()` | Fit random forest regression or classification |
| `bagging_fit()` | Fit bootstrap-aggregated models |
| `bagging_score()` | Estimate a coefficient-based variable importance score using resampling |
| `ensemble_fit()` | Fit multiple models with user-defined or equal ensemble weights |
| `eval_fit()` | Evaluate models by K-fold CV using RMSE or AUC |

The predictor-screening procedure ranks marginal associations using correlation, ANOVA, or categorical association tests, depending on input types. When enabled, an optional VIF-based filter addresses collinearity among selected predictors.

## Installation

The package requires **R >= 4.0.0**. The declared R dependencies include `glmnet`, `randomForest`, `pROC`, and `car`; `knitr` and `rmarkdown` are suggested for rendering the vignette.

After publishing, install from GitHub by replacing `<owner>` with the repository account or organization:

```r
install.packages("remotes")
remotes::install_github("<owner>/Ensemble-Algorithm-R")
```

You can also clone the repository and install the package locally:

```r
install.packages("remotes")
remotes::install_local("/path/to/Ensemble-Algorithm-R")
```

> **Compatibility note:** Installation and automated package checks have not yet been executed in the current environment. The existing implementation is supplied for inspection and reproducibility rather than claimed to be a validated production release.

## Quick start

The package includes bundled example data and online help for its exported functions.

```r
library(simpleEnsemble)
set.seed(123)

data("example_data", package = "simpleEnsemble")

# Screen predictors using marginal association scores.
selected <- screening(example_data$X, example_data$y, K = 5)

# Fit a weighted ensemble: linear, ridge and lasso regression.
fit <- ensemble_fit(
  X = selected$X,
  y = example_data$y,
  models = c("linear", "ridge", "lasso"),
  porp = c(0.4, 0.3, 0.3)
)

fit$model.names
fit$porp

# Compare a base model with K-fold cross-validation.
cv_result <- eval_fit(selected$X, example_data$y, model = "linear", K = 5)
cv_result$metric
cv_result$value
```

For more complete examples, see [`vignettes/simpleEnsemble_vignettes.Rmd`](vignettes/simpleEnsemble_vignettes.Rmd) and individual function help pages under [`man/`](man/).

## Repository layout

```text
.
├── DESCRIPTION                # R package metadata and dependencies
├── NAMESPACE                  # Exported function declarations
├── R/                         # R function implementations
├── man/                       # R documentation (Rd files)
├── data/                      # Bundled example datasets
├── vignettes/                 # Detailed workflow examples
├── LICENSE                    # GNU GPL version 3
└── README.md                  # Project overview and usage
```

## Project status

**Academic project · version 0.1.0.** The source is publicly available for review. Formal automated tests, current-platform compatibility, and CRAN readiness have not been established. The package metadata intentionally omits a personal maintainer contact.


This project is distributed under the [GNU General Public License v3.0](LICENSE).
