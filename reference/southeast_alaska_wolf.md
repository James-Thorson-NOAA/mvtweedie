# Wolf environmental DNA in southeast Alaska

Data to demonstrate multivariate Tweedie GAM for spatial analysis

## Usage

``` r
data(southeast_alaska_wolf)
```

## Value

A data long-form data frame

## Details

Data sufficient to demonstrate how to use a Tweedie GLM to provide
inference about proportions e.g. in food habits analysis, where the
model output is processed to represent a multivariate logit Tweedie
model.

Specifically includes environmental DNA sampling of wolf scats obtained
in Southeast Alaska.

- `Latitude` Latitude for scat sample

- `Longitude` Longitude for scat sample

- `group` prey groupings from eDNA smaples

- `Response` relative read abundance calculated as the mean proportion
  of DNA sequence reads from each species among samples from a given
  scat

## References

Roffler, G. H., J. M. Allen, A. Massey, and T. Levi. 2021. Wolf Dietary
Diversity in an Island Archipelago. Bulletin of the Ecological Society
of America 102: 1-6.
[doi:10.1002/bes2.1834](https://doi.org/10.1002/bes2.1834)

## Author

Gretchen Roffler
