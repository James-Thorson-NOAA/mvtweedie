
#' Predict proportions for new data
#'
#' Predict proportions and associated standard errors using a standard S3 object interface
#'
#' A Tweedie GLM using a log-link and multiple categories can be
#' transformed to yield predicted proportions and associated SEs, where the
#' model is interpreted as a multivariate logit Tweedie distribution. This function
#' does this transformation for a model fitted using:
#' \itemize{
#' \item A generalized additive model (GAM) using \code{\link[mgcv]{gam}}
#' \item A generalized linear mixed model (GLMM) using \code{\link[glmmTMB]{glmmTMB}}
#' }
#' It then also calculates an approximation to the standard error for this proportion
#'
#' @inheritParams stats::predict.lm
#' @param x output from \code{\link[mgcv]{gam}} or \code{\link[glmmTMB]{glmmTMB}}, but with
#'    \code{class(x)=c("mvtweedie",...)} where \code{...} indicates the original values for
#'    \code{class(x)}
#' @param category_name name of column that indicates grouping variable
#' @param origdata original data used when fitting
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(mvtweedie)
#'
#' # load data set
#' data( Middleton_Island_TUPU, package="mvtweedie" )
#' DF = Middleton_Island_TUPU
#'
#' # Run Tweedie GLM
#' gam0 = gam( formula = Response ~ 0 + group, data = DF, family = tw )
#'
#' # Inspect results
#' class(gam0) = c( "mvtweedie", class(gam0) )
#' predict(gam0, se.fit=TRUE, origdata = DF)
#' }
#'
#'
#' @method predict mvtweedie
#' @export
predict.mvtweedie <-
function(x,
#                  original_class = "glmmTMB",
                  category_name = "group",
                  newdata,
                  origdata = x$frame,
                  se.fit = FALSE )
{
  # Error checks
  if( any(c("gam","glmmTMB") %in% class(x)) ){
    if( tolower(substr(family(x)$family,1,7)) != "tweedie" ) error("`predict.mvtweedie` only implemented for a Tweedie distribution")
    if( family(x)$link != "log" ) error("`predict.mvtweedie` only implemented for a log link")
  }else if( "fit_model"%in%class(x) ){
    if( se.fit==TRUE ) error("se.fit not implemented for predict using VAST")
  }else{
    stop("`predict.mvtweedie` only implemented for mgcv, glmmTMB and VAST")
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
    class(x) = setdiff( class(x), "mvtweedie" )
    #class(x) = original_class

    # Apply predict.original_class
    if( "fit_model" %in% class(x) ){
      # if using VAST
      pred_ic[,cI] = predict(x,
                   what="D_i",
                   Lat_i=x$data_frame[,'Lat_i'],
                   Lon_i=x$data_frame[,'Lon_i'],
                   t_i=x$data_frame[,'t_i'],
                   a_i=x$data_frame[,'a_i'],
                   c_iz=rep(cI-1,nrow(x$data_frame)),
                   v_i=x$data_frame[,'v_i'] )
    }else{
      pred = predict(x,
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
