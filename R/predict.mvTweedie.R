predict.mvTweedie <-
function(x,
#                  original_class = "glmmTMB",
                  newdata,
                  origdata = x$frame,
                  se.fit = FALSE )
{
  # Error checks
  if( se.fit==TRUE & "fit_model"%in%class(x) ) error("se.fit not implemented for predict using VAST")

  # Defaults
  if(missing(newdata) || is.null(newdata)) newdata = origdata

  # Predict each observation for each class
  se_pred_ic = pred_ic = array(NA, dim=c(nrow(newdata),nlevels(origdata[,'group'])))
  for(cI in 1:ncol(pred_ic)){

    # Modify data
    data = newdata
    data[,'group'] = factor( levels(origdata[,'group'])[cI], levels=levels(origdata[,'group']) )

    # Modify class
    class(x) = setdiff( class(x), "mvTweedie" )
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
                   newdata=data,
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
  prob_i = prob_ic[ cbind(1:nrow(pred_ic), match(newdata[,'group'],levels(origdata[,'group']))) ]

  # return prediction
  if( se.fit==TRUE ){
    # Normalize SE-squared for each observation and class
    rowsum_se2_ic = outer( rowSums(se_pred_ic^2), rep(1,ncol(pred_ic)) )
    se2_prob_ic = prob_ic^2 * ( se_pred_ic^2/pred_ic^2 - 2*se_pred_ic^2/(pred_ic*rowsum_pred_ic) + rowsum_se2_ic/rowsum_pred_ic^2 )
    se_i = sqrt(se2_prob_ic[ cbind(1:nrow(se2_prob_ic), match(newdata[,'group'],levels(origdata[,'group']))) ])
    out = list("fit"=prob_i, "se.fit"=se_i)
  }else{
    out = prob_i
  }
  return(out)
}
