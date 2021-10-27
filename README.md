# mvtweedie
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
Thorson, J. T., Arimitsu, M. L., Levi, T., & Roffler, G. (In press). Diet analysis using generalized linear models derived from foraging processes using R package mvtweedie. Ecology.

