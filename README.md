
<!-- README.md is generated from README.qmd. Please edit that file -->

# REcopol package:package:<img src="man/figures/REcopol_logo.png" align="right" width="400" />

<!-- badges: start -->
<!-- badges: end -->

The goal of {`REcopol`} is to make an easy-to-use function to analyse fossil pollen data

## Installation

You can install the development version of REcopol:package: from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("HOPE-UIB-BIO/R-Ecopol-package")
```

## Usage

The main purpose of {REcopol} is to provide tools for easy-to-use analyses of fossil pollen data. All functions can be grouped into 5 categories:

1. **Diversity estimations** - Functions to estimates diversity from community data using various methods, including taxonomic, functional. and phylogenetic diversity.
    - [diversity_estimate()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate.html)
    - [diversity_estimate_taxonomic()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_taxonomic.html)
    - [diversity_estimate_functional()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_functional.html)
    - [diversity_estimate_phylogenetic()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/diversity_estimate_phylogenetic.html)
2. **Ordinations** - Functions to fit Detrended (Canonical)
    Correspondence Analysis.
    - [fit_ordination()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination.html)
    - [fit_ordination_dca()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination_dca.html)
    - [fit_ordination_dcca()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_ordination_dcca.html)
3. **GAM modeling** - Functions to fit, predict, and visualise generalized additive models (GAM) models.
    - [fit_custom_gam()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_custom_gam.html)
    - [fit_hgam()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_hgam.html)
    - [fit_multiple_gams()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/fit_multiple_gams.html)
    - [predic_model()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/predic_model.html)
    - [plot_temporal_trend()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/plot_temporal_trend.html)
4. **Change-point analyses and regression partition** - Functions for detection of change points in temporal patterns
    - [regression_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/regression_partition.html)
    - [mv_regression_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/mv_regression_partition.html)
    - [get_change_points()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_change_points.html)
    - [get_change_points_all()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_change_points_all.html)
    - [add_data_partition()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/add_data_partition.html)
5. **Utility** - Functions for operation with (not only) fossil pollen data
    - [add_age_bin()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/add_age_bin.html)
    - [transfer_into_proportions()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/transfer_into_proportions.html)
    - [tranform_percentage_data()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/tranform_percentage_data.html)
    - [get_density()](https://hope-uib-bio.github.io/R-Ecopol-package/reference/get_density.html)
