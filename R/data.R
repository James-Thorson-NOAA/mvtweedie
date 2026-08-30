
#' @title Middleton Island tufted puffin diets
#'
#' @description Data to demonstrate multivariate Tweedie GAM for time-series
#'
#' @details Data sufficient to demonstrate how to use a Tweedie Generalized Additive Model
#'  to provide inference about proportions e.g. in food habits analysis, where the model output is
#'  processed to represent a multivariate logit Tweedie model.
#'
#' Specifically includes Tufted Puffin (Fratercula cirrhata) bill loads sampled at Middleton Island.
#'
#' \itemize{
#'   \item \code{Response} Numeric prey biomass in bill load samples
#'   \item \code{Year} Numeric year
#'   \item \code{group} factor representing prey species or category
#'   \item \code{SampleID} factor with a level for every sampling occasion, e.g., for use in row normalization
#' }
#'
#' @return A data long-form data frame
#' 
#' @references
#' Hatch, S., and G. Sanger. 1992. Puffins as Samplers of Juvenile 
#' Pollock and Other Forage 
#' Fish in the Gulf of Alaska. Marine Ecology Progress Series 80: 1-14.
#'
#' @name Middleton_Island_TUPU
#' @docType data
#' @author Mayumi Arimitsu
#' @usage data(Middleton_Island_TUPU)
#' @keywords data
NULL

#' @title Wolf environmental DNA in southeast Alaska
#'
#' @description Data to demonstrate multivariate Tweedie GAM for spatial analysis
#'
#' @details Data sufficient to demonstrate how to use a Tweedie GLM to provide inference
#'  about proportions e.g. in food habits analysis, where the model output is
#'  processed to represent a multivariate logit Tweedie model.
#'
#' Specifically includes environmental DNA sampling of wolf scats obtained in Southeast Alaska.
#'
#' \itemize{
#'   \item \code{Latitude} Latitude for scat sample
#'   \item \code{Longitude} Longitude for scat sample
#'   \item \code{group} prey groupings from eDNA smaples
#'   \item \code{Response} relative read abundance calculated as the mean 
#'         proportion of DNA sequence reads from each species among samples
#'         from a given scat
#' }
#'
#' @return A data long-form data frame
#' 
#' @references
#' Roffler, G. H., J. M. Allen, A. Massey, and T. Levi. 2021. 
#' Wolf Dietary Diversity in an Island Archipelago. Bulletin of 
#' the Ecological Society of America 102: 1-6.
#' \doi{10.1002/bes2.1834}
#'
#' @name southeast_alaska_wolf
#' @docType data
#' @author Gretchen Roffler
#' @usage data(southeast_alaska_wolf)
#' @keywords data
NULL

