
#' @title Predict proportions for new data
#'
#' @description Predict proportions and associated standard errors using a standard S3 object interface
#'
#' @details A Tweedie GLM using a log-link and multiple categories can be
#' transformed to yield predicted proportions and associated SEs, where the
#' model is interpreted as a multivariate logit Tweedie distribution. This function
#' does this transformation for a model fitted using:
#' \itemize{
#' \item A generalized additive model (GAM) using \code{\link[mgcv]{gam}}
#' \item A generalized linear mixed model (GLMM) using \code{\link[glmmTMB]{glmmTMB}}
#' }
#' It then also calculates an approximation to the standard error for this proportion
#'
#' @importFrom mgcv gam predict.gam
#' @inheritParams stats::predict.lm
#' @param object output from \code{\link[mgcv]{gam}} or \code{\link[glmmTMB]{glmmTMB}}, but with
#'    \code{class(object)=c("mvtweedie",...)} where \code{...} indicates the original values for
#'    \code{class(object)}
#' @param category_name name of column that indicates grouping variable
#' @param origdata original data used when fitting
#'
#' @examples
#' # Load packages
#' library(mvtweedie)
#'
#' # load data set
#' data( Middleton_Island_TUPU, package="mvtweedie" )
#' DF = Middleton_Island_TUPU
#'
#' # Run Tweedie GLM
#' gam0 = mgcv::gam( formula = Response ~ 0 + group, data = DF, family = tw )
#'
#' # Inspect results
#' class(gam0) = c( "mvtweedie", class(gam0) )
#' predict(gam0, se.fit=TRUE, origdata = DF)
#'
#' @return
#' predict.mvtweedie produces a vector of predicted proportions or a list containing 
#' predicted proportions and standard errors.
#'
#' @method predict mvtweedie
#' @export
predict.mvtweedie <-
function( object,
          category_name = "group",
          newdata,
          origdata = object$model,
          se.fit = FALSE,
          ... )
{
  # Error checks
  if( any(c("gam","glmmTMB") %in% class(object)) ){
    if( tolower(substr(family(object)$family,1,7)) != "tweedie" ) warning("`predict.mvtweedie` only implemented for a Tweedie distribution")
    if( family(object)$link != "log" ) stop("`predict.mvtweedie` only implemented for a log link")
  }else if( "fit_model"%in%class(object) ){
    if( se.fit==TRUE ) error("se.fit not implemented for predict using VAST")
  }else{
    stop("`predict.mvtweedie` only implemented for mgcv, glmmTMB and VAST")
  }

  # Check and account for tibbles
  if( "package:tibble" %in% search() ){
    if( is_tibble(origdata) ){
      warning("Converting `origdata` from tibble to data.frame")
      origdata = as.data.frame(origdata)
    }
  }

  # Defaults
  if(missing(newdata) || is.null(newdata)) newdata = origdata

  # Predict each observation for each class
  se_pred_ic = pred_ic = array(NA, dim=c(nrow(newdata),nlevels(origdata[,category_name])))
  for(cI in 1:ncol(pred_ic)){

    # Modify data
    data = newdata
    data[,category_name] = factor( levels(origdata[,category_name])[cI], levels=levels(origdata[,category_name]) )

    # Modify class
    class(object) = setdiff( class(object), "mvtweedie" )
    #class(object) = original_class

    # Apply predict.original_class
    if( "fit_model" %in% class(object) ){
      # if using VAST
      pred_ic[,cI] = predict(object,
                   what="D_i",
                   Lat_i=object$data_frame[,'Lat_i'],
                   Lon_i=object$data_frame[,'Lon_i'],
                   t_i=object$data_frame[,'t_i'],
                   a_i=object$data_frame[,'a_i'],
                   c_iz=rep(cI-1,nrow(object$data_frame)),
                   v_i=object$data_frame[,'v_i'] )
    }else{
      pred = predict(object,
                   newdata = data,
                   type="response",
                   se.fit = se.fit )
      if( se.fit==TRUE ){
        pred_ic[,cI] = pred$fit
        se_pred_ic[,cI] = pred$se.fit
      }else{
        pred_ic[,cI] = pred
      }
    }
  }

  # Normalize probability for each observation and class
  rowsum_pred_ic = outer( rowSums(pred_ic), rep(1,ncol(pred_ic)) )
  prob_ic = pred_ic / rowsum_pred_ic
  prob_i = prob_ic[ cbind(1:nrow(pred_ic), match(newdata[,category_name],levels(origdata[,category_name]))) ]

  # return prediction
  if( se.fit==TRUE ){
    # Normalize SE-squared for each observation and class
    rowsum_se2_ic = outer( rowSums(se_pred_ic^2), rep(1,ncol(pred_ic)) )
    se2_prob_ic = prob_ic^2 * ( se_pred_ic^2/pred_ic^2 - 2*se_pred_ic^2/(pred_ic*rowsum_pred_ic) + rowsum_se2_ic/rowsum_pred_ic^2 )
    se_i = sqrt(se2_prob_ic[ cbind(1:nrow(se2_prob_ic), match(newdata[,category_name],levels(origdata[,category_name]))) ])
    out = list("fit"=prob_i, "se.fit"=se_i)
  }else{
    out = prob_i
  }
  return(out)
}

#' @name mvtweedie
#' @rdname mvtweedie
#' @title Multivariate Tweedie distribution for predicting diet proportions
#'
#' @description Using regression methods to analyze diet proportions for a marked point process
#'
#' @details Diet samples often measure a count or biomass for different prey categories.
#'          Rather than converting these data to a proportion and fitting 
#'          these proportions as data, we can instead represent diet samples as an outcome
#'          from a thinnned and double-marked point process, where marks 
#'          include prey category and size per encounter, and thinning represents
#'          variation in attack and capture rates and is conceptually similar to 
#'          detectability/catchability in other point-count sampling analyses.
#'          Analyzing raw prey measurements (rather than proportions) 
#'          allows a wide range of models (and associated off-the-shelf software),
#'          predictions can still be converted to proportions (with associated standard
#'          errors) after the model is fitted, and we can represent covariance
#'          in prey measurements within a sample using covariates
#'          that explain sample-specific attack/capture rates.  
#'          
#'          If the prey densities follow a 
#'          a Poisson point-process, and prey size per encounter follows a gamma 
#'          distribution, then the resulting distribution for biomass of each prey
#'          follows a multivariate Tweedie distribution. We therefore interpret
#'          the multivariate Tweedie distribution as a "process-based" model
#'          for prey samples.
#' 
#' @references
#' Thorson, J. T., Arimitsu, M. L., Levi, T., & Roffler, G. H. (2022). 
#' Diet analysis using generalized linear models derived from foraging 
#' processes using R package mvtweedie. Ecology, 103(5), e3637. 
#' \doi{10.1002/ecy.3637}
NULL
