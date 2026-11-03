# Middleton Island tufted puffin diets

Data to demonstrate multivariate Tweedie GAM for time-series

## Usage

``` r
data(Middleton_Island_TUPU)
```

## Value

A data long-form data frame

## Details

Data sufficient to demonstrate how to use a Tweedie Generalized Additive
Model to provide inference about proportions e.g. in food habits
analysis, where the model output is processed to represent a
multivariate logit Tweedie model.

Specifically includes Tufted Puffin (Fratercula cirrhata) bill loads
sampled at Middleton Island.

- `Response` Numeric prey biomass in bill load samples

- `Year` Numeric year

- `group` factor representing prey species or category

- `SampleID` factor with a level for every sampling occasion, e.g., for
  use in row normalization

## References

Hatch, S., and G. Sanger. 1992. Puffins as Samplers of Juvenile Pollock
and Other Forage Fish in the Gulf of Alaska. Marine Ecology Progress
Series 80: 1-14.

## Author

Mayumi Arimitsu
