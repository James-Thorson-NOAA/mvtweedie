# mvtweedie
[![DOI](https://zenodo.org/badge/341673481.svg)](https://zenodo.org/badge/latestdoi/341673481)

An R package to interpret a Tweedie generalized linear model (GLM) or generalized additive model (GAM) involving multiple classes as an estimate of proportions for each class, implicitly involving a multivariate-logit transformation for parameters and predictions.  This approach generalizes the Poisson-to-multinomial transformation to allow for non-integer responses, and can analyze either pre-processed (transformed to proportions) or raw (zero-inflated positive real values) data.

This approach is helpful, e.g., when analyzing diet samples that are heavily zero inflated without pre-processing the samples prior to analysis.  In these cases, the Tweedie distribution can be interpreted mechanistically as a thinned and double-marked Poisson point process representing foraging processes.


### Citation
Thorson, J. T., Arimitsu, M. L., Levi, T., & Roffler, G. (2022). [Diet analysis using generalized linear models derived from foraging processes using R package mvtweedie](https://doi.org/10.1002/ecy.3637). Ecology. 103(5): e3637.

