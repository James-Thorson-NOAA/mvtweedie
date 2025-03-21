# mvtweedie
[![DOI](https://zenodo.org/badge/341673481.svg)](https://zenodo.org/badge/latestdoi/341673481)

An R package to interpret a Tweedie generalized linear model (GLM) or generalized additive model (GAM) involving multiple classes as an estimate of proportions for each class, implicitly involving a multivariate-logit transformation for parameters and predictions.  This approach generalizes the Poisson-to-multinomial transformation to allow for non-integer responses, and can analyze either pre-processed (transformed to proportions) or raw (zero-inflated positive real values) data.

This approach is helpful, e.g., when analyzing diet samples that are heavily zero inflated without pre-processing the samples prior to analysis.  In these cases, the Tweedie distribution can be interpreted mechanistically as a thinned and double-marked Poisson point process representing foraging processes.


### Demo

```R
library( ggplot2 )
library( mgcv )
library( mvtweedie )

# Load data
data( "Middleton_Island_TUPU", package="mvtweedie" )
Middleton_Island_TUPU$Year = as.numeric(as.character( Middleton_Island_TUPU$Year_factor ))

# Fit GAM
fit = mgcv::gam( Response ~ group + s(Year, by=group), data=Middleton_Island_TUPU, family="tw" )
class(fit) = c( "mvtweedie", class(fit) )

# Predict values
newdata = expand.grid("group"=levels(Middleton_Island_TUPU$group), 
  "Year"=min(Middleton_Island_TUPU$Year):max(Middleton_Island_TUPU$Year))
pred = predict( fit,
                   se.fit=TRUE,
                   origdata = Middleton_Island_TUPU,
                   newdata = newdata )
newdata = cbind( newdata, fit=pred$fit, se.fit=pred$se.fit )
newdata$lower = newdata$fit - newdata$se.fit
newdata$upper = newdata$fit + newdata$se.fit

# Plot
theme_set(theme_bw())
ggplot(newdata, aes(Year, fit)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(group)) +
  scale_color_viridis_c(name = "SST") +
  ylim(0,1) +
  labs(y="Predicted proportion")
```

### Citation
Thorson, J. T., Arimitsu, M. L., Levi, T., & Roffler, G. (2022). [Diet analysis using generalized linear models derived from foraging processes using R package mvtweedie](https://doi.org/10.1002/ecy.3637). Ecology. 103(5): e3637.

# NOAA Enterprise GitHub disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
