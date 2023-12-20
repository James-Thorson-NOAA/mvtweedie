
#' @title Middleton Island tufted puffin diets
#'
#' @description Data to demonstrate and test multivariate logit Tweedie model
#'
#' @details Data sufficient to demonstrate how to use a Tweedie GLM to provide inference
#'  about proportions e.g. in food habits analysis, where the model output is
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

