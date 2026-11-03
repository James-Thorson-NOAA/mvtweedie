# Multivariate Tweedie distribution for predicting diet proportions

Using regression methods to analyze diet proportions for a marked point
process

## Details

Diet samples often measure a count or biomass for different prey
categories. Rather than converting these data to a proportion and
fitting these proportions as data, we can instead represent diet samples
as an outcome from a thinnned and double-marked point process, where
marks include prey category and size per encounter, and thinning
represents variation in attack and capture rates and is conceptually
similar to detectability/catchability in other point-count sampling
analyses. Analyzing raw prey measurements (rather than proportions)
allows a wide range of models (and associated off-the-shelf software),
predictions can still be converted to proportions (with associated
standard errors) after the model is fitted, and we can represent
covariance in prey measurements within a sample using covariates that
explain sample-specific attack/capture rates.

If the prey densities follow a a Poisson point-process, and prey size
per encounter follows a gamma distribution, then the resulting
distribution for biomass of each prey follows a multivariate Tweedie
distribution. We therefore interpret the multivariate Tweedie
distribution as a "process-based" model for prey samples.

## References

Thorson, J. T., Arimitsu, M. L., Levi, T., & Roffler, G. H. (2022). Diet
analysis using generalized linear models derived from foraging processes
using R package mvtweedie. Ecology, 103(5), e3637.
[doi:10.1002/ecy.3637](https://doi.org/10.1002/ecy.3637)

## See also

[`predict.mvtweedie`](predict.mvTweedie.md) for details
